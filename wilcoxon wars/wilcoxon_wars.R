
#### WILCOXON WARS ####

# Background
# Two burger restaurants are compared based on ratings customers gave them. To
# that end, a population of 10 million ratings is generated for each restaurant.
# Then, samples of the following sizes (per restaurant) are drawn: 10, 50, 100,
# 200, 500, 1000, 2000, 3000, 4000. For each sample size, 10,000 samples of the
# respective size are drawn. Then, the two restaurants are compared with both,
# a t-test and a Wilcoxon rank sum test for independent samples. In seven 
# different scenarios, parameters of the underlying populations are varied to 
# examine the robustness of the two tests. In each of the scenarios, the mean
# of the two populations is the same.

#### PREPARATION ####
library(tidyverse)
library(coin)

# Stop total time
masterstart <- Sys.time()

# Functions

#' t-test vs. Wilcoxon
#' Simulate a number of t-tests and Wilcoxon tests. Also prints how long the
#' process took.
#' 
#' @param ngroup A vector specifying the range of sample sizes per group.
#' @param nsim An integer specifying the number of samples to be drawn for each
#'             group size.
#' @param pop1 Population from which the first group is sampled.
#' @param pop2 Population from which the second group is sampled. Per default
#'             identical with the first.
#' @param name An optional name that is printed with the duration.
#' 
#' @return A data frame: 
#'   The first column (t_p) holds the p-values returned from the Welch t-test
#'   The second column (t_vareq_p) holds the p-values returned from the t-test
#'   with var.equal = TRUE
#'   The third column (w_p) holds the p-values returned from the Wilcoxon test
#'   The fourth column (perm_p) holds the p-values returned from a permutation
#'   test (independence_test from the package coin)
#'   The fifthcolumn (boot) holds the p-values returned from a bootstrapped
#'   t-test (boot.ttest2 from the package Rfast)
#'   The sixth column (n) hold the group size
t_vs_w <- function(ngroup = c(10, 50, 100), nsim = 1000, pop1, pop2 = pop1, 
                   name = NULL) {
  start <- Sys.time()
  
  fillme <- matrix(nrow = nsim * length(ngroup), ncol = 6)
  colnames(fillme) <- c("t_p", "t_vareq_p", "w_p", "perm_p", "boot_p", "n")
  count <- 0
  
  for (i in ngroup) {
    for (j in 1:nsim) {
      dat1 <- sample(pop1, i)
      dat2 <- sample(pop2, i)
      
      dat <- data.frame(dat1 = dat1, dat2 = dat2)
      dat <- tidyr::gather(dat, population, value, dat1, dat2)
      dat$population <- factor(dat$population)
      
      t_test <- t.test(value ~ population, data = dat)
      t_test_varequ <- t.test(value ~ population, data = dat, var.equal = TRUE)
      w_test <- wilcox.test(value ~ population, data = dat, exact = FALSE)
      perm_test <- coin::independence_test(value ~ population, data = dat,
                                           distribution = "approximate")
      boot <- Rfast::boot.ttest2(dat$value[dat$population == "dat1"], 
                                 dat$value[dat$population == "dat2"])
      
      fillme[j + (count * nsim), "t_p"] <- t_test$p.value
      fillme[j + (count * nsim), "t_vareq_p"] <- t_test_varequ$p.value
      fillme[j + (count * nsim), "w_p"] <- w_test$p.value
      fillme[j + (count * nsim), "perm_p"] <- coin::pvalue(perm_test)
      fillme[j + (count * nsim), "boot_p"] <- boot[2]
      fillme[j + (count * nsim), "n"] <- i
    }
    count <- count + 1
  }
  fillme <- as.data.frame(fillme)
  fillme$ID <- 1:nrow(fillme)
  
  stop <- Sys.time()
  cat(name, "Duration =", stop - start)
  return(fillme)
}

#' burger function
#' Takes a distribution as argument and converts the numbers to integers from
#' 1 - 10.
burger <- function(distr){
  distr <- round(distr)
  distr[distr < 1] <- 1 
  distr[distr > 10] <- 10 
  
  return(distr)
}

#' ordinal burger ratings
#' Takes burger restaurant ratings from 1 - 10 and turns them into ordinal
#' ratings according to the following classification:
#' 1 -  4: poor       1
#' 5 -  8: good       2
#' 9 - 10: excellent  3
ordinal_burgers <- function(ratings) {
  ratings <- case_when(ratings %in% 1:4 ~ 1,
                       ratings %in% 5:8 ~ 2,
                       ratings %in% 9:10 ~ 3)
}

# function to generate a lognormal distribution with specified mean and sd
lognorm_spec <- function(mean, sd){
  location <- log(mean^2 / sqrt(sd^2 + mean^2))
  shape <- sqrt(log(1 + (sd^2 / mean^2)))
  
  return(c("meanlog" = location, "sdlog" = shape))
}

# Odds and ends

# Seed
# Cross sum of the letters' positions spelling "kill your darlings" i.e. the 
# Wilcoxon test
kyd <- 11 +  9 + 12 + 12 + 
  25 + 15 + 21 + 18 + 
  4 +  1 + 18 + 12 +  9 + 14 +  7 + 19

# population size
pop <- 1000000

# group sizes
stpr <- c(10, 50, 100, 200, 500, 1000, 2000, 3000, 4000)

# runs per group size
nsim <- 10000


#### 01 ####
# default
# normal distribution; 1 - 10
set.seed(kyd)
pop1 <- rnorm(pop, 5.5, 2.4)
pop1 <- burger(pop1)
pop2 <- pop1

wilcox_war01 <- t_vs_w(ngroup = stpr, nsim = nsim, pop1 = pop1, 
                       name = "01 - default")

#### 02 ####
# ordinal scale
# normal distribution; 1 - 10
# recoded to 1, 2, 3 (see above)
set.seed(kyd)
pop1 <- ordinal_burgers(pop1)
pop2 <- pop1

wilcox_war02 <- t_vs_w(ngroup = stpr, nsim = nsim, pop1 = pop1, 
                       name = "02 - ordinal")

#### 03 ####
# extremely skewed distribution - lognormal
set.seed(kyd)
pop1 <- rlnorm(pop)
pop2 <- pop1

wilcox_war03 <- t_vs_w(ngroup = stpr, nsim = nsim, pop1 = pop1, 
                       name = "03 - lognormal")

#### 04 ####
# different standard deviations for the two populations
# normal distribution; 1 - 10
set.seed(kyd)
pop1 <- rnorm(pop, 5.5, .5)
pop1 <- burger(pop1)

pop2 <- rnorm(pop, 5.5, 3)
pop2 <- burger(pop2)

wilcox_war04 <- t_vs_w(ngroup = stpr, nsim = nsim, pop1 = pop1, pop2 = pop2, 
                       name = "04 - different SDs")

#### 04a ####
# Supposed to be lognormal, but did not work
params1 <- lognorm_spec(40, .5)
params2 <- lognorm_spec(40, 1)

set.seed(kyd)
pop1 <- round(rlnorm(pop, params1[1], params1[2]))
pop2 <- round(rlnorm(pop, params2[1], params2[2]))

wilcox_war04a <- t_vs_w(ngroup = stpr, nsim = nsim, pop1 = pop1, pop2 = pop2, 
                       name = "04a - still kinda normal")

#### 05 ####
# similar to Fagerland (2012)
# two lognormal distributions
# same mean and median, different sd
# TO DO: find lognormal with same median and mean, but still lognormal
set.seed(kyd)
pop1 <- rlnorm(pop, sdlog = .72)
pop2 <- rlnorm(pop, sdlog = .3)

# identical means
diff <- mean(pop1) - mean(pop2)
pop2 <- pop2 + diff

wilcox_war05 <- t_vs_w(ngroup = stpr, nsim = nsim, pop1 = pop1, pop2 = pop2, 
                       name = "05 - lognorm different SDs")

#### 06 ####
# similar to Fagerland (2012)
# two gamma distributions
set.seed(kyd)
pop1 <- rgamma(pop, shape = .24)
pop2 <- rgamma(pop, shape = .42)

# identical means
diff <- mean(pop1) - mean(pop2)
pop2 <- pop2 + diff

wilcox_war06 <- t_vs_w(ngroup = stpr, nsim = nsim, pop1 = pop1, pop2 = pop2, 
                       name = "06 - gamma different SDs")

#### 07 ####
# Fagerland-inspired escalation: lognormal vs. gamma distribution; both skewness
# around 3
set.seed(kyd)
pop1 <- rlnorm(pop, sdlog = .72)
pop2 <- rgamma(pop, shape = .42)

# identical means
diff <- mean(pop1) - mean(pop2)
pop2 <- pop2 + diff

wilcox_war07 <- t_vs_w(ngroup = stpr, nsim = nsim, pop1 = pop1, pop2 = pop2, 
                       name = "07 - lognormal vs. gamma")

masterstop<- Sys.time()
(masterdur <- masterstop - masterstart)

# Save output so simulations won't have to be run again
save(list = c("wilcox_war01", "wilcox_war02", "wilcox_war03", "wilcox_war04",
              "wilcox_war04a", "wilcox_war05", "wilcox_war06", "wilcox_war07"), 
     file = "wilcoxon_wars_data.RData")
