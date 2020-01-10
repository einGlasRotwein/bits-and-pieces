
# Script for visualisation of cake counts in the work time tracking data.

library(tidyverse)

source("./working_time_under_the_hood.R")

# Some functions for forcing R to print a specified number of decimals. I took them from the
# package prmisc by Martin Papenberg so you don't have to download the whole package.
# If you want to, though, you can do that here:
# https://github.com/m-Py/prmisc
# Then, you can change the following line to: library(prmisc)
source("./print_decimals.R")

# theme for pie chart
pie_theme <- theme(legend.text = element_text(size = 18), 
                   legend.title = element_text(size = 20),
                   plot.title = element_text(size = 24, hjust = .5),
                   plot.subtitle = element_text(size = 20, hjust = .5))

# theme for bar chart
col_theme <- theme(legend.position = "top", 
                   plot.title = element_text(size = 18, hjust = .5),
                   plot.subtitle = element_text(size = 16, hjust = .5),
                   axis.title = element_text(size = 14),
                   axis.text = element_text(size = 12),
                   legend.title = element_text(size = 14),
                   legend.text = element_text(size = 12))

#### DAY WITH(OUT) CAKE ####
cake_binary <- wtime[!is.na(wtime$stop), ] %>% # all the entries which have a stop time
  mutate(cake = ifelse(no_cakes != 0, TRUE, FALSE)) %>% # cake or not as logical
  count(cake) %>% 
  mutate(perc = n/sum(n)) # calculate percentage

# Arrange descending or ascending, depending on whether there are more or less than 50% cakes 
# (important for correct order for plotting)
if(cake_binary$perc[cake_binary$cake == TRUE] < .5) {
  cake_binary <- arrange(cake_binary, perc)
} else {
  cake_binary <- arrange(cake_binary, -perc)
}

# Count the number of days (i.e. NOT the number of entries)
n_day <- length(unique(as_date(wtime$start[!is.na(wtime$stop)])))
# If you'd rather have the number of entries, change n_day in the plot below to n_entries, uncomment 
# and run the following line:
# n_entries <- nrow(wtime)

n_cakes <- sum(wtime$no_cakes[!is.na(wtime$stop)]) # total number of cakes

# prepare data for pie chart
cake_binary <- cake_binary %>%
  mutate(lab_y_pos = cumsum(perc) - 0.5 * perc)

cake_binary %>%
  ggplot(aes(x = "", y = perc, fill = cake)) +
  geom_bar(width = 1, stat = "identity", colour = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab_y_pos, label = paste(force_decimals(perc * 100), '%')), 
            color = "white", size = 8) +
  scale_fill_manual("cake", values = c("#207365", "#1ad999"),
                    labels = c("no", "yes")) +
  labs(title = "days at the lab with(out) cake", subtitle = paste0("days = ", n_day, ", total number of cakes = ", n_cakes)) +
  theme_void() +
  pie_theme

#### TOP CAKE CONTRIBUTORS ####
bakers <- wtime$cake_tags[!is.na(wtime$cake_tags)]

# Split between .
bakers <- unlist(strsplit(bakers, "[.]"))

# individual bakers: strip of homemade information (-h) and get unique names
bakers_ind <- unique(gsub("-h", "", bakers))

bakers_count <- data.frame(baker = bakers_ind, total = NA, homemade = NA,
                           stringsAsFactors = FALSE)

# Count how many cakes each baker contributed in total and how many were homemade
for (i in seq_along(bakers_count$baker)) {
  cur_baker <- bakers_ind[i]
  bakers_count$total[i] <- sum(grepl(cur_baker, bakers))
  bakers_count$homemade[i] <- sum(grepl(paste0(cur_baker, "-h"), bakers))
}

# Sort - most frequent bakers
bakers_count <- bakers_count %>% 
  arrange(desc(total))

# Have bakers as factor ordered by number of cakes contributed
bakers_count$baker <- factor(bakers_count$baker, levels = bakers_count$baker)

# long format
bakers_count <- bakers_count %>% 
  pivot_longer(cols = c(total, homemade), values_to = "count",
               names_to = "type")

bakers_count %>%
  filter(baker != "random") %>% 
  ggplot(aes(x = baker, y = count, fill = type)) +
  geom_col(position = "identity",  colour = "black") +
  scale_y_continuous(breaks = seq(0, max(bakers_count$count), 1)) +
  scale_fill_manual("", values = c("#17418c", "#1c99cc")) +
  labs(title = "top bakers") +
  col_theme
