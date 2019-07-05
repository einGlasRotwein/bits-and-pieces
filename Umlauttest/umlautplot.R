
library(tidyverse)

umlautplot <- diamonds %>% 
  ggplot(aes(x = cut, y = price)) +
  stat_summary() +
  labs(x = "Label mit Umläuten")
