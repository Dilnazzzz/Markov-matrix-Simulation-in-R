library(tidyverse) # includes ggplot2, dplyr, and others
library(plotly)

#set.seed(200322)

markov3 <- function(n = 100, iter = 100, start_probs = c(0, 0.25, 0.25, 0.25, 0.25), trans_probs = matrix(c(3/19, 10/11, 1/8, 3/7, 1/3,
                                                                                                          3/19, 0, 3/4, 2/7, 1/9, 
                                                                                                          6/19, 0, 0, 0, 1/9,
                                                                                                          4/19, 0, 1/8, 1/7, 1/3,
                                                                                                          3/19, 1/11, 0, 1/7, 1/9), ncol = 5),
                    plot_prob = TRUE) { # Here we have a full non-zero transition matrix.
  #Check to see if probability entries are valid.
  if(sum(start_probs) != 1 | any(start_probs < 0)) 
    stop("start_probs must be non-negative and sum to 1.")
  if(any(apply(trans_probs, 1, sum) != 1))
    stop("trans_probs matrix rows must sum to 1.")
  if(any(trans_probs < 0))
    stop("elements of trans_probs must be non-negative")
  
  dt <- matrix(NA, nrow = iter, ncol = n) # Initialize matrix to hold iterations
  # Run chain
  for(i in 1:iter) {
    for(j in 1:n) {
      if(i == 1) { # if we're at the beginning of the simulation
        dt[i, j] <- sample(x = c(0, 1, 2, 3, 4), size = 1, prob = start_probs)
      } else {
        if(dt[i - 1, j] == 0) { # if the previous state was 0
          dt[i, j] <- sample(x = c(0, 1, 2, 3, 4), size = 1, prob = trans_probs[,1])
        } else if (dt[i - 1, j] == 1) { # if the previous state was 1
          dt[i, j] <- sample(x = c(0, 1, 2, 3, 4), size = 1, prob = trans_probs[,2])
        } else if (dt[i - 1, j] == 1) {
          dt[i, j] <- sample(x = c(0, 1, 2, 3, 4), size = 1, prob = trans_probs[,3])
        } else if (dt[i - 1, j] == 1) {
          dt[i, j] <- sample(x = c(0, 1, 2, 3, 4), size = 1, prob = trans_probs[,4])
        } else {
          dt[i, j] <- sample(x = c(0, 1, 2, 3, 4), size = 1, prob = trans_probs[,5])
        }
      }
    }
  }
  
  # Plot results?
  if(plot_prob) {
    df <- data.frame(x = 1:iter,
                     n0 = apply(subset(dt == 0), 1, sum),
                     n1 = apply(subset(dt == 1), 1, sum),
                     n2 = apply(subset(dt == 2), 1, sum),
                     n3 = apply(subset(dt == 3), 1, sum),
                     n4 = apply(subset(dt == 4), 1, sum))
    p <- df %>%
      pivot_longer(cols = c(n0, n1, n2, n3, n4)) %>%
      ggplot(aes(x, value, color = name)) +
      geom_line() +
      scale_color_discrete(labels = c("0", "1", "2", "3", "4")) +
      labs(x = "Iteration",
           y = "Prop.") +
      theme_minimal(base_size = 16)
    
    show(p)
  }
  
  # Return chain as dataframe
  return(as.data.frame(dt))
}

df2 <- markov3()


df_long2 <- df2 %>%
  rowid_to_column(var = "iter") %>%
  pivot_longer(cols = V1:V100) %>%
  group_by(name) %>%
  mutate(x = value + rnorm(1, 0, .10) + rnorm(n(), 0, .01),
         y = value %% 2 + rnorm(1, 0, .10) + rnorm(n(), 0, .01),
         initial = value[1],
         is_0 = ifelse(value == 0, TRUE, FALSE),
         is_1 = ifelse(value == 1, TRUE, FALSE),
         is_2 = ifelse(value == 2, TRUE, FALSE),
         is_3 = ifelse(value == 3, TRUE, FALSE),
         is_4 = ifelse(value == 4, TRUE, FALSE)) %>%
  group_by(iter) %>%
  mutate(prop0 = mean(is_0),
         prop1 = mean(is_1),
         prop2 = mean(is_2),
         prop3 = mean(is_3),
         prop4 = 1 - prop0 - prop1 - prop2 - prop3) %>%
  ungroup()

anim2 <- df_long2 %>%
  plot_ly(
    x = ~x,
    y = ~y,
    color = ~factor(initial),
    size = 2,
    colors = c("#FF0000", "#003aff", "#32CD32", "#FFFF00", "#6B3FA0"),
    frame = ~iter,
    type = 'scatter',
    mode = 'markers',
    showlegend = FALSE
  )

anim2 <- anim2 %>%
  add_text(x = 0, y = .35, text = ~prop0, textfont = list(color = "#FF0000", size = 24, opacity = .6)) %>%
  #add_text(x = -0.5, y = .35, text = "C:", textfont = list(color = "#FF0000", size = 24, opacity = .6)) %>%
  
  add_text(x = 1, y = 1.25, text = ~prop1, textfont = list(color = "#003aff", size = 24, opacity = .6)) %>%
  #add_text(x = 0.5, y = 1.25, text = "A:", textfont = list(color = "#003aff", size = 24, opacity = .6)) %>%
  
  add_text(x = 2, y = .35, text = ~prop2, textfont = list(color = "#32CD32", size = 24, opacity = .6)) %>%
  #add_text(x = 1.5, y = .35, text = "D:", textfont = list(color = "#32CD32", size = 24, opacity = .6)) %>%
  
  add_text(x = 3, y = 1.25, text = ~prop3, textfont = list(color = "#FFFF00", size = 24, opacity = .6)) %>%
  #add_text(x = 2.5, y = 1.25, text = "B:", textfont = list(color = "#FFFF00", size = 24, opacity = .6)) %>%
  
  add_text(x = 4, y = 0.35, text = ~prop4, textfont = list(color = "#6B3FA0", size = 24, opacity = .6)) 
  #add_text(x = 3.5, y = 0.35, text = "E:", textfont = list(color = "#6B3FA0", size = 24, opacity = .6))


ax2 <- list(
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE,
  title = ""
)

anim2 <- anim2 %>%
  layout(xaxis = ax2, yaxis = ax2) %>%
  animation_opts(redraw = FALSE) %>%
  animation_slider(hide = TRUE) %>%
  animation_button(x = .6, y = .10, showactive = TRUE, label = "Run Simulation") %>%
  config(displayModeBar = FALSE, scrollZoom = FALSE, showTips = FALSE)

anim2


