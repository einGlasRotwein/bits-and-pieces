
library(tidyverse)

# This loads a dataframe into your environment. It's quite ugly.
# If you want to see how I created the whole thing, look at bread_underthehood.R, but for now,
# I just suggest to run the code and spoil the magic afterwards.
urlfile <-"https://raw.github.com/einGlasRotwein/bits-and-pieces/master/bRead/df_bread.csv"
df_bread <- read.csv(urlfile)

# Look at this ugly thing. It doesn't make sense.
df_bread

# Pivot it into long format!
(
  df_beautiful <- df_bread %>% 
    pivot_longer(cols = -baker, 
                 names_to = c(".value", "bread_no", "letter_no"),
                 names_pattern = "(.*)_(.*)_(.*)") %>% 
    arrange(baker, bread_no, letter_no)
)
