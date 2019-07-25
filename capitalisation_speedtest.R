
# I examined the capitalization function in Hmisc (which is, obviously, meant to 
# capitalise letters) and found that they do some sort of preselection: Strings
# that already start upper case are skipped and only those starting lower case
# are - well - capitalised. I thought is was quite clever because that sounds
# efficient, right?
# But ... is it?
# Checking which letters are already upper case takes some time in itself. Is it 
# worth it? I ran some speed tests varying the amount of lower case strings in
# the vectors passed to the function and let the version with preselection 
# (capped, as implemented in Hmisc) and the version without preselection
# compete against each other.
# Results: The uncapped version performs better as soon as there are ~ 30 %
# lower case strings or more. Then, just (sometimes redundantly) converting
# every first letter of a string to upper case is faster than checking if a
# string is already upper case and only converting those that aren't.

capped <- function(string) {
  capped <- grep("^[A-Z]", string, invert = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  return(string)
}

uncapped <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}

# perc = percent lower case
perc0 <- rep("Chocolate", 1000)
perc10 <- c(rep("Chocolate", 900), rep("chocolate", 100))
perc20 <- c(rep("Chocolate", 800), rep("chocolate", 200))
perc30 <- c(rep("Chocolate", 700), rep("chocolate", 300))
perc40 <- c(rep("Chocolate", 600), rep("chocolate", 400))
perc50 <- c(rep("Chocolate", 500), rep("chocolate", 500))
perc60 <- c(rep("Chocolate", 400), rep("chocolate", 600))
perc70 <- c(rep("Chocolate", 300), rep("chocolate", 700))
perc80 <- c(rep("Chocolate", 200), rep("chocolate", 800))
perc90 <- c(rep("Chocolate", 100), rep("chocolate", 900))
perc100 <- rep("chocolate", 1000)

# Run speed tests
library(microbenchmark)

# CAPPED
capped_perc0 <- microbenchmark(
  capped(perc0), times = 10000
)
capped_perc10 <- microbenchmark(
  capped(perc10), times = 10000
)
capped_perc20 <- microbenchmark(
  capped(perc20), times = 10000
)
capped_perc30 <- microbenchmark(
  capped(perc30), times = 10000
)
capped_perc40 <- microbenchmark(
  capped(perc40), times = 10000
)
capped_perc50 <- microbenchmark(
  capped(perc50), times = 10000
)
capped_perc60 <- microbenchmark(
  capped(perc60), times = 10000
)
capped_perc70 <- microbenchmark(
  capped(perc70), times = 10000
)
capped_perc80 <- microbenchmark(
  capped(perc80), times = 10000
)
capped_perc90 <- microbenchmark(
  capped(perc90), times = 10000
)
capped_perc100 <- microbenchmark(
  capped(perc100), times = 10000
)

# UNCAPPED
uncapped_perc0 <- microbenchmark(
  uncapped(perc0), times = 10000
)
uncapped_perc10 <- microbenchmark(
  uncapped(perc10), times = 10000
)
uncapped_perc20 <- microbenchmark(
  uncapped(perc20), times = 10000
)
uncapped_perc30 <- microbenchmark(
  uncapped(perc30), times = 10000
)
uncapped_perc40 <- microbenchmark(
  uncapped(perc40), times = 10000
)
uncapped_perc50 <- microbenchmark(
  uncapped(perc50), times = 10000
)
uncapped_perc60 <- microbenchmark(
  uncapped(perc60), times = 10000
)
uncapped_perc70 <- microbenchmark(
  uncapped(perc70), times = 10000
)
uncapped_perc80 <- microbenchmark(
  uncapped(perc80), times = 10000
)
uncapped_perc90 <- microbenchmark(
  uncapped(perc90), times = 10000
)
uncapped_perc100 <- microbenchmark(
  uncapped(perc100), times = 10000
)

# Get mean times
mb_names <- c(paste0("capped_", "perc", seq(0, 100, 10)), 
              paste0("uncapped_", "perc", seq(0, 100, 10)))
times <- rep(NA, length(mb_names))

for (i in seq_along(mb_names)) {
  cur_times <- get(mb_names[i])$time
  times[i] <- round((mean(cur_times) / 1000), 2)
}

# Dataframe for comparison
(comparison <- data.frame(func = mb_names, microsec = times))
comparison$perc_lower <- as.numeric(gsub("^.*_perc", "", comparison$func))/100
comparison$func <- gsub("_.*", "", comparison$func)

# Plot results
library(tidyverse)

case_theme <- theme(legend.position = "top",
                    plot.title = element_text(size = 16, hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5),
                    axis.title = element_text(size = 14),
                    axis.text = element_text(size = 12),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 14),
                    panel.grid.minor.x = element_blank())

comparison %>% 
  ggplot(aes(x = perc_lower, y = microsec, colour = func)) + 
  geom_line(size = 1) +
  geom_point(aes(shape = func), size = 3) +
  scale_colour_manual("Function", values = c("dark blue", "orangered")) +
  scale_y_continuous(breaks = seq(100, 700, 100), limits = c(100, 700)) +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  scale_shape_manual("Function", values = c(19, 17)) +
  labs(title = "Runtime Capitalisation Functions",
       subtitle = "with our without preselection of lower case letters", 
       y = "microseconds", x = "% lower case") +
  case_theme
