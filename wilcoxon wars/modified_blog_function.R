
library(tidyverse)

# This is a slightly modified version of the t_vs_w function for the blog post
# about p-values. It stores the samples, not only the p-values.

# Modified Wilcoxon Wars function
# This version only calculates a Welch t-test, but safes the samples for each
# group.
blogpost <- function(ngroup = c(10, 50, 100), nsim = 1000, pop1, pop2 = pop1, 
                     name = NULL) {
  start <- Sys.time()
  
  fillme <- matrix(nrow = nsim * length(ngroup), ncol = 2)
  colnames(fillme) <- c("t_p", "n")
  rawdat1 <- list()
  rawdat2 <- list()
  count <- 0
  
  for (i in ngroup) {
    for (j in 1:nsim) {
      dat1 <- sample(pop1, i)
      dat2 <- sample(pop2, i)
      
      rawdat1[[j + (count * nsim)]] <- dat1
      rawdat2[[j + (count * nsim)]] <- dat2
      
      t_test <- t.test(dat1, dat2)
      
      fillme[j + (count * nsim), "t_p"] <- t_test$p.value
      fillme[j + (count * nsim), "n"] <- i
    }
    count <- count + 1
  }
  fillme <- as.data.frame(fillme)
  fillme$ID <- 1:nrow(fillme)
  
  stop <- Sys.time()
  cat(name, "Duration =", stop - start)
  return(list(fillme, rawdat1, rawdat2))
}

# convert to ratings as integers from 1 - 10
burger <- function(distr){
  distr <- round(distr)
  distr[distr < 1] <- 1 
  distr[distr > 10] <- 10 
  
  return(distr)
}

# seed ("Kill your darlings")
kyd <- 11 +  9 + 12 + 12 + 
  25 + 15 + 21 + 18 + 
  4 +  1 + 18 + 12 +  9 + 14 +  7 + 19

# population size
pop <- 1000000

# group sizes
stpr <- c(10, 50, 100, 200, 500, 1000, 2000, 3000, 4000)

# runs per group size
nsim <- 10000

# run function
set.seed(kyd)
pop1 <- rnorm(pop, 5.5, 2.4)
pop1 <- burger(pop1)

# data for the blog post
for_blog <- blogpost(ngroup = stpr, nsim = nsim, pop1 = pop1, name = "blogpost")

p_values <- as.data.frame(for_blog[[1]])

groups1 <- for_blog[[2]]
groups2 <- for_blog[[3]]

# RESULTS
# proportion of p < .05
mean(p_values$t_p < .05)

# find columns where p-value == 1 (only some of them and only those in the
# large sample as there are so many)
head(p_values[p_values$t_p == 1 & p_values$n == 4000, ])

# get corresponding group samples
mean(groups1[[80405]])
mean(groups2[[80405]])

# p-value (almost) exactly p = .05
p_values[near(p_values$t_p, .05, tol = .000001), ]

# get corresponding group samples
mean(groups1[[67891]])
mean(groups2[[67891]])

# lowest p
p_values[p_values$t_p == min(p_values$t_p), ]

# get corresponding group samples
mean(groups1[[83188]])
mean(groups2[[83188]])

# biggest difference
# put group means in a data.frame and compute the difference
compare_means <- data.frame(mean_gr1 = sapply(groups1, mean), 
                            mean_gr2 = sapply(groups2, mean)) %>% 
  mutate(diff = abs(mean_gr1 - mean_gr2))

compare_means[compare_means$diff == max(compare_means$diff), ]
# ID 9256

# plot examples
for_plot_burgers <- data.frame(
  restaurant1 = c(groups1[[80405]], groups1[[67891]], 
                  groups1[[83188]], groups1[[9256]]),
  restaurant2 = c(groups2[[80405]], groups2[[67891]], 
                  groups2[[83188]], groups2[[9256]]),
  p_values = c(rep(p_values$t_p[80405], length(groups1[[80405]])), 
               rep(p_values$t_p[67891], length(groups1[[67891]])), 
               rep(p_values$t_p[83188], length(groups1[[83188]])), 
               rep(p_values$t_p[9256], length(groups1[[9256]]))),
  n = c(rep(length(groups1[[80405]]), length(groups1[[80405]])), 
        rep(length(groups1[[67891]]), length(groups1[[67891]])), 
        rep(length(groups1[[83188]]), length(groups1[[83188]])), 
        rep(length(groups1[[9256]]), length(groups1[[9256]]))),
  name = c(rep("example 1", length(groups1[[80405]])), 
           rep("example 2", length(groups1[[67891]])), 
           rep("example 3", length(groups1[[83188]])), 
           rep("example 4", length(groups1[[9256]])))
)

for_plot_burgers <- for_plot_burgers %>% 
  gather(restaurant, rating, restaurant1, restaurant2)

for_plot_burgers %>% 
  ggplot(aes(x = rating)) +
  geom_histogram(aes(fill = restaurant), binwidth = 1, alpha = .7, 
                 colour = "black", position = "identity") +
  scale_fill_manual("Burger Restaurant", values = c("#9e0000", "#2d2d2d"),
                    labels = c("restaurant 1", "restaurant 2")) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  facet_wrap(~name, nrow = 4, scales = "free_y") +
  labs(y = "count") +
  theme(legend.position = "top")
