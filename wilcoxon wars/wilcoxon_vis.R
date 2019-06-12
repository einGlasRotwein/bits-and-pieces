
# Build plots from wilcoxon_wars.R simulation for wilcoxon_wars.Rmd

library(tidyverse)
library(ggforce)
library(scales)
library(tie)
library(propint)

load("wilcoxon_wars_data.RData")

# Functions

# Martin's interval function
#' Compute confidence interval for a proportion
#' 
#' @param x A numeric (1/0) or logical vector
#' 
#' @return A 3-element vector: 
#'   The first element (P) is the parameter estimate (proportion)
#'   The second element (l) is the lower bound of the CI
#'   The third element (u) is the upper bound of the CI
interval_prop <- function(x, level = 0.95) {
  x <- x[!is.na(x)]
  r <- sum(x)
  n <- length(x)
  ci <- propint::ci.one.prop(level * 100, r, n)
  output <- unname(unlist(ci))
  names(output) <- c("y", "ymin", "ymax")
  
  return(output)
}

#' polishes simulation output for plotting
#' 
#' @param wilcox_output Output from the t-test vs. Wilcoxon simulations

polish_wilcox <- function(wilcox_output) {
  wilcox_output <- wilcox_output %>% 
    gather(test, p_value, vars = ends_with("_p"))
  wilcox_output <- wilcox_output %>% 
    mutate(test = case_when(test == "t_p" ~ "t-test",
                            test == "t_vareq_p" ~ "t-test vareq",
                            test == "w_p" ~ "Wilcoxon",
                            test == "perm_p" ~ "permutation",
                            test == "boot_p" ~ "bootstrap")) %>% 
    mutate(test = factor(test, levels = c("t-test", "t-test vareq", "Wilcoxon",
                                          "permutation", "bootstrap"), 
                         ordered = TRUE))
  # significant = 1; n.s. = 0
  wilcox_output <- wilcox_output %>% 
    mutate(sign = ifelse(p_value < .05, 1, 0))
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

# population
pop <- 1000000

julis_theme <- theme(legend.position = "top", 
                     plot.title = element_text(size = 16, hjust = 0.5),
                     plot.subtitle = element_text(hjust = 0.5),
                     axis.title = element_text(size = 14),
                     axis.text = element_text(size = 12), 
                     legend.text = element_text(size = 12), 
                     legend.title = element_text(size = 14))

pd <- position_dodge(.5)

# seed (letters of "kill your darlings")
kyd <- 11 +  9 + 12 + 12 + 
  25 + 15 + 21 + 18 + 
  4 +  1 + 18 + 12 +  9 + 14 +  7 + 19

#### 1 - DEFAULT ####
set.seed(kyd)
pop1 <- rnorm(pop, 5.5, 2.4)
pop1 <- burger(pop1)
pop2 <- pop1

(
  plot_pop01 <- as.data.frame(pop1) %>% 
    ggplot(aes(x = pop1)) +
    geom_histogram(binwidth = 1, alpha = .7, colour = "black", 
                   fill = "#9e0000") +
    scale_x_continuous(breaks = seq(1, 10, 1)) +
    labs(x = "Rating", y = "count", title = "population", 
         subtitle = "01 - default") +
    julis_theme
)

wilcox_war01 <- polish_wilcox(wilcox_war01)

(
  tab_01 <- wilcox_war01 %>% 
    group_by(test, n) %>% 
    bow(tie(y, ymin, ymax) := interval_prop(sign))
)

# simulation results
(
  plot_out01 <- wilcox_war01 %>%
    ggplot(aes(x = factor(n), y = sign, colour = test)) + 
    stat_summary(fun.data = "interval_prop", geom = "errorbar", width = .1, 
                 position = pd, size = 1.3) +
    stat_summary(aes(shape = test), fun.data = "interval_prop", 
                 geom = "point", position = pd, size = 3.5) +
    scale_shape_manual(values = c(15, 19, 17, 18, 4, 10)) +
    scale_color_manual(values = c("#9e0000", "#2d2d2d", "#19a98a", "#e0bf07", "#0c00c4", "#0aaa00")) +
    scale_y_continuous(labels = percent) +
    labs(x = "group size", y = "proportion p < .05", 
         title = "proportion of significant results",
         subtitle = "01 - default") +
    julis_theme
)

#### 2 - ORDINAL SCALE ####
set.seed(kyd)
pop1 <- ordinal_burgers(pop1)
pop2 <- pop1

(
  plot_pop02 <- as.data.frame(pop1) %>% 
    ggplot(aes(x = pop1)) +
    geom_histogram(binwidth = 1, alpha = .7, colour = "black", 
                   fill = "#9e0000") +
    scale_x_continuous(breaks = seq(1, 10, 1)) +
    labs(x = "rating", y = "count", title = "population", 
         subtitle = "02 - ordinal") +
    scale_y_continuous(labels = comma) +
    julis_theme
)

wilcox_war02 <- polish_wilcox(wilcox_war02)

(
  tab_02 <- wilcox_war02 %>% 
    group_by(test, n) %>% 
    bow(tie(y, ymin, ymax) := interval_prop(sign))
)

(
  plot_out02 <- wilcox_war02 %>%
    ggplot(aes(x = factor(n), y = sign, colour = test)) + 
    stat_summary(fun.data = "interval_prop", geom = "errorbar", width = .1, 
                 position = pd, size = 1.3) +
    stat_summary(aes(shape = test), fun.data = "interval_prop", 
                 geom = "point", position = pd, size = 3.5) +
    scale_shape_manual(values = c(15, 19, 17, 18, 4, 10)) +
    scale_color_manual(values = c("#9e0000", "#2d2d2d", "#19a98a", "#e0bf07", "#0c00c4", "#0aaa00")) +
    scale_y_continuous(labels = percent) +
    labs(x = "group size", y = "proportion p < .05", 
         title = "proportion of significant results",
         subtitle = "02 - ordinal") +
    julis_theme
)

#### 3 - EXTREMELY SKEWED ####
set.seed(kyd)
pop1 <- rlnorm(pop)
pop2 <- pop1

(
  plot_pop03 <- as.data.frame(pop1) %>% 
    ggplot(aes(x = pop1)) +
    geom_histogram(binwidth = 1, alpha = .7, colour = "black", 
                   fill = "#9e0000") +
    labs(x = "rating", y = "count", title = "population", 
         subtitle = "03 - lognormal") +
    scale_y_continuous(labels = comma) +
    julis_theme
)

wilcox_war03 <- polish_wilcox(wilcox_war03)

(
  tab_03 <- wilcox_war03 %>% 
    group_by(test, n) %>% 
    bow(tie(y, ymin, ymax) := interval_prop(sign))
)

(
  plot_out03 <- wilcox_war03 %>%
    ggplot(aes(x = factor(n), y = sign, colour = test)) + 
    stat_summary(fun.data = "interval_prop", geom = "errorbar", width = .1, 
                 position = pd, size = 1.3) +
    stat_summary(aes(shape = test), fun.data = "interval_prop", 
                 geom = "point", position = pd, size = 3.5) +
    scale_shape_manual(values = c(15, 19, 17, 18, 4, 10)) +
    scale_color_manual(values = c("#9e0000", "#2d2d2d", "#19a98a", "#e0bf07", "#0c00c4", "#0aaa00")) +
    scale_y_continuous(labels = percent) +
    labs(x = "group size", y = "proportion p < .05", 
         title = "proportion of significant results",
         subtitle = "03 - lognormal") +
    julis_theme
)

#### 4 - DIFFERENT SDS NORMAL####
set.seed(kyd)
pop1 <- rnorm(pop, 5.5, .5)
pop1 <- burger(pop1)

pop2 <- rnorm(pop, 5.5, 3)
pop2 <- burger(pop2)

forplot_pop04 <- data.frame(pop = rep(c("pop1", "pop2"), each = pop),
                            value = c(pop1, pop2))

(
  plot_pop04 <- forplot_pop04 %>% 
    ggplot(aes(x = value)) +
    geom_histogram(binwidth = 1, alpha = .7, colour = "black", 
                   fill = "#9e0000") +
    scale_x_continuous(breaks = seq(1, 10, 1)) +
    labs(x = "rating", y = "count", title = "population", 
         subtitle = "04 - different SDs") +
    scale_y_continuous(labels = comma) +
    facet_wrap(~pop, nrow = 2) +
    julis_theme
)

wilcox_war04 <- polish_wilcox(wilcox_war04)

(
  tab_04 <- wilcox_war04 %>% 
    group_by(test, n) %>% 
    bow(tie(y, ymin, ymax) := interval_prop(sign))
)

(
  plot_out04 <- wilcox_war04 %>%
    ggplot(aes(x = factor(n), y = sign, colour = test)) + 
    stat_summary(fun.data = "interval_prop", geom = "errorbar", width = .1, 
                 position = pd, size = 1.3) +
    stat_summary(aes(shape = test), fun.data = "interval_prop", 
                 geom = "point", position = pd, size = 3.5) +
    scale_shape_manual(values = c(15, 19, 17, 18, 4, 10)) +
    scale_color_manual(values = c("#9e0000", "#2d2d2d", "#19a98a", "#e0bf07", "#0c00c4", "#0aaa00")) +
    scale_y_continuous(labels = percent) +
    labs(x = "group size", y = "proportion p < .05", 
         title = "proportion of significant results",
         subtitle = "04 - different SDs") +
    julis_theme
)

#### 4a - DIFFERENT SDS STILL KINDA NORMAL ####
params1 <- lognorm_spec(40, .5)
params2 <- lognorm_spec(40, 1)

set.seed(kyd)
pop1 <- round(rlnorm(pop, params1[1], params1[2]))
pop2 <- round(rlnorm(pop, params2[1], params2[2]))

forplot_pop04a <- data.frame(pop = rep(c("pop1", "pop2"), each = pop),
                            value = c(pop1, pop2))

(
  plot_pop04a <- forplot_pop04a %>% 
    ggplot(aes(x = value)) +
    geom_histogram(binwidth = 1, alpha = .7, colour = "black", 
                   fill = "#9e0000") +
    scale_x_continuous(breaks = seq(37, 44, 1)) +
    labs(x = "rating", y = "count", title = "population", 
         subtitle = "04a - still kinda normal") +
    scale_y_continuous(labels = comma) +
    facet_wrap(~pop, nrow = 2) +
    julis_theme
)

wilcox_war04a <- polish_wilcox(wilcox_war04a)

(
  tab_04a <- wilcox_war04a %>% 
    group_by(test, n) %>% 
    bow(tie(y, ymin, ymax) := interval_prop(sign))
)

(
  plot_out04a <- wilcox_war04a %>%
    ggplot(aes(x = factor(n), y = sign, colour = test)) + 
    stat_summary(fun.data = "interval_prop", geom = "errorbar", width = .1, 
                 position = pd, size = 1.3) +
    stat_summary(aes(shape = test), fun.data = "interval_prop", 
                 geom = "point", position = pd, size = 3.5) +
    scale_shape_manual(values = c(15, 19, 17, 18, 4, 10)) +
    scale_color_manual(values = c("#9e0000", "#2d2d2d", "#19a98a", "#e0bf07", "#0c00c4", "#0aaa00")) +
    scale_y_continuous(labels = percent) +
    labs(x = "group size", y = "proportion p < .05", 
         title = "proportion of significant results",
         subtitle = "04a - still kinda normal") +
    julis_theme
)

#### 5 - DIFFERENT LOGNORMAL ####
set.seed(kyd)
pop1 <- rlnorm(pop, sdlog = .72)
pop2 <- rlnorm(pop, sdlog = .3)

diff <- mean(pop1) - mean(pop2)
pop2 <- pop2 + diff

plot_pop05_01 <- as.data.frame(pop1) %>% 
  ggplot(aes(x = pop1)) +
  geom_histogram(binwidth = .5, alpha = .7, colour = "black", 
                 fill = "#9e0000") +
  labs(x = "rating", y = "count", title = "population 1", 
       subtitle = "05 - different lognormal distributions") +
  scale_y_continuous(labels = comma, breaks = seq(0, 500000, 100000),
                     limits = c(0, 500000)) +
  julis_theme +
  facet_zoom(xlim = c(0, 8))

plot_pop05_02 <- as.data.frame(pop2) %>% 
  ggplot(aes(x = pop2)) +
  geom_histogram(binwidth = .5, alpha = .7, colour = "black", 
                 fill = "#9e0000") +
  labs(x = "rating", y = "count", title = "population 2", 
       subtitle = "05 - different lognormal distributions") +
  scale_y_continuous(labels = comma, breaks = seq(0, 500000, 100000),
                     limits = c(0, 500000)) +
  julis_theme +
  facet_zoom(xlim = c(0, 8))

wilcox_war05 <- polish_wilcox(wilcox_war05)

(
  tab_05 <- wilcox_war05 %>% 
    group_by(test, n) %>% 
    bow(tie(y, ymin, ymax) := interval_prop(sign))
)

(
  plot_out05 <- wilcox_war05 %>%
    ggplot(aes(x = factor(n), y = sign, colour = test)) + 
    stat_summary(fun.data = "interval_prop", geom = "errorbar", width = .1, 
                 position = pd, size = 1.3) +
    stat_summary(aes(shape = test), fun.data = "interval_prop", 
                 geom = "point", position = pd, size = 3.5) +
    scale_shape_manual(values = c(15, 19, 17, 18, 4, 10)) +
    scale_color_manual(values = c("#9e0000", "#2d2d2d", "#19a98a", "#e0bf07", "#0c00c4", "#0aaa00")) +
    scale_y_continuous(labels = percent) +
    labs(x = "group size", y = "proportion p < .05", 
         title = "proportion of significant results",
         subtitle = "05 - different lognormal distributions") +
    julis_theme
)

#### 6 - DIFFERENT GAMMA ####
set.seed(kyd)
pop1 <- rgamma(pop, shape = .24)
pop2 <- rgamma(pop, shape = .42)

diff <- mean(pop1) - mean(pop2)
pop2 <- pop2 + diff

plot_pop06_01 <- as.data.frame(pop1) %>% 
  ggplot(aes(x = pop1)) +
  geom_histogram(binwidth = .5, alpha = .7, colour = "black", 
                 fill = "#9e0000") +
  labs(x = "rating", y = "count", title = "population 1", 
       subtitle = "06 - different gamma distributions") +
  scale_y_continuous(labels = comma, breaks = seq(0, 200000, 50000),
                     limits = c(0, 200000)) +
  julis_theme +
  facet_zoom(xlim = c(0, 8))

plot_pop06_02 <- as.data.frame(pop2) %>% 
  ggplot(aes(x = pop2)) +
  geom_histogram(binwidth = .5, alpha = .7, colour = "black", 
                 fill = "#9e0000") +
  labs(x = "rating", y = "count", title = "population 2", 
       subtitle = "06 - different gamma distributions") +
  scale_y_continuous(labels = comma, breaks = seq(0, 200000, 50000),
                     limits = c(0, 200000)) +
  julis_theme +
  facet_zoom(xlim = c(0, 8))

wilcox_war06 <- polish_wilcox(wilcox_war06)

(
  tab_06 <- wilcox_war06 %>% 
    group_by(test, n) %>% 
    bow(tie(y, ymin, ymax) := interval_prop(sign))
)

(
  plot_out06 <- wilcox_war06 %>%
    ggplot(aes(x = factor(n), y = sign, colour = test)) + 
    stat_summary(fun.data = "interval_prop", geom = "errorbar", width = .1, 
                 position = pd, size = 1.3) +
    stat_summary(aes(shape = test), fun.data = "interval_prop", 
                 geom = "point", position = pd, size = 3.5) +
    scale_shape_manual(values = c(15, 19, 17, 18, 4, 10)) +
    scale_color_manual(values = c("#9e0000", "#2d2d2d", "#19a98a", "#e0bf07", "#0c00c4", "#0aaa00")) +
    scale_y_continuous(labels = comma) +
    labs(x = "group size", y = "proportion p < .05", 
         title = "proportion of significant results",
         subtitle = "06 - different gamma distributions") +
    julis_theme
)

#### 7 FAGERLAND-ESCALATION ####
set.seed(kyd)
pop1 <- rlnorm(pop, sdlog = .72)
pop2 <- rgamma(pop, shape = .42)

diff <- mean(pop1) - mean(pop2)
pop2 <- pop2 + diff

plot_pop07_01 <- as.data.frame(pop1) %>% 
  ggplot(aes(x = pop1)) +
  geom_histogram(binwidth = .5, alpha = .7, colour = "black", 
                 fill = "#9e0000") +
  labs(x = "rating", y = "count", title = "population 1", 
       subtitle = "07 - Fagerland escalation") +
  scale_y_continuous(labels = comma, breaks = seq(0, 400000, 100000),
                     limits = c(0, 400000)) +
  julis_theme +
  facet_zoom(xlim = c(0, 8))

plot_pop07_02 <- as.data.frame(pop2) %>% 
  ggplot(aes(x = pop2)) +
  geom_histogram(binwidth = .5, alpha = .7, colour = "black", 
                 fill = "#9e0000") +
  labs(x = "rating", y = "count", title = "population 2", 
       subtitle = "07 - Fagerland escalation") +
  scale_y_continuous(labels = comma, breaks = seq(0, 400000, 100000),
                     limits = c(0, 400000)) +
  julis_theme +
  facet_zoom(xlim = c(0, 8))

wilcox_war07 <- polish_wilcox(wilcox_war07)

(
  tab_07 <- wilcox_war07 %>% 
    group_by(test, n) %>% 
    bow(tie(y, ymin, ymax) := interval_prop(sign))
)

(
  plot_out07 <- wilcox_war07 %>%
    ggplot(aes(x = factor(n), y = sign, colour = test)) + 
    stat_summary(fun.data = "interval_prop", geom = "errorbar", width = .1, 
                 position = pd, size = 1.3) +
    stat_summary(aes(shape = test), fun.data = "interval_prop", 
                 geom = "point", position = pd, size = 3.5) +
    scale_shape_manual(values = c(15, 19, 17, 18, 4, 10)) +
    scale_color_manual(values = c("#9e0000", "#2d2d2d", "#19a98a", "#e0bf07", "#0c00c4", "#0aaa00")) +
    scale_y_continuous(labels = percent) +
    labs(x = "group size", y = "proportion p < .05", 
         title = "proportion of significant results",
         subtitle = "07 - Fagerland escalation") +
    julis_theme
)
