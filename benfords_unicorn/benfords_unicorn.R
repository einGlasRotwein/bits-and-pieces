## BENFORD'S LAW

# Some data and plots to visualise Benford's Law
# unicorn content included

## LOAD LIBRARIES
library(tidyverse)
library(ggimage)
library(gganimate)

## EXAMPLE
# Benford's Law usually holds for data that is associated with
# growth. Hence, we create a passbook where we initially store
# 10 euros. Every day, our balance increases by 1 %. The example 
# isn't too realistic, but let's just go with the flow.
# We document how much money we have each day, and we stop when 
# we have saved at least 10,000 euros. Sorry for the big stupid 
# loop - it illustrated the example well.

passbook <- 10

while (tail(passbook, 1) < 10000) {
  passbook <- c(passbook, tail(passbook, 1) * 1.01)
}

## EXTRACT FIRST DIGIT
# For Benford's Law, we only need the first digit of each
# number. This function extracts it.
first_digit <- 
  sapply(strsplit(as.character(passbook), ""), function(x) return(x[1]))

# Let's put all the stuff in a data.frame so we can plot it later.
# We also create a dummy column we will need later.
benfords_dataframe <- data.frame(day = 1:length(passbook), balance = passbook, 
                       digit = first_digit, dummy = 1)

## PLOTS
# Colours for each leading digit
benford_colours <-  c("#9A031E", "#C86FC9", "#40798C", "#F7AF9D", "#C9F299", 
                      "#F45D01", "#2D7DD2", "#EEB902", "#81E979")

# First, a plot how our balance increases over time. The leading
# digits are highlighted in different colours.
benfords_dataframe %>% 
  ggplot(aes(x = day, y = balance, fill = digit)) +
  geom_col(width = 1) +
  scale_fill_manual(values = benford_colours) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(legend.position = "top", axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14))

# We can't really see the smaller numbers, so here's a better
# overview of the ratio of leading digits.
benfords_dataframe %>% 
  ggplot(aes(x = day, y = dummy, fill = digit)) +
  geom_col(width = 1) +
  scale_fill_manual(values = benford_colours) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(legend.position = "top", legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        panel.background = element_blank()) +
  coord_fixed(20)

# Have a look at the percentages here
(percent <- 
    round(as.vector(table(as.numeric(benfords_dataframe$digit)))/
            sum(as.vector(table(as.numeric(benfords_dataframe$digit)))), 2))

# Prepare the data for plot visualising the amount of each leading digit.
digitcount <- benfords_dataframe %>% 
  group_by(digit) %>% 
  count(digit) %>% 
  mutate(perc = n/sum(.$n)) %>% 
  mutate(perc = round(perc * 100, 2))

# ... and plot it
digitcount %>% 
  ggplot(aes(x = factor(digit), y = n)) +
  geom_col(fill = benford_colours) +
  geom_text(aes(label = perc), nudge_y = -10) +
  labs(y = "count", x = "digit") +
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14))


# UNICORN CONTENT
# let's get the unicorn image
# assuming your R project is in the main folder - if not, change
# file path accordingly!
unicorn <- "./benfords unicorn/pics/unicorn.png"

# prepare data for the plot
digitcount$plus <- digitcount$n + 10 # to adjust the image
digitcount$image <- unicorn
digitcount$digit <- 1:9 # because we don't want a factor for the transition

(
  plot_pink <- 
    ggplot(digitcount, aes(x = as.factor(digit), y = n)) +
    geom_col(alpha = .8, colour = "black", fill = "#f318d2") +
    geom_image(aes(x = digit, y = n, image = image), size = .25) +
    labs(x = "digit", y = "count") +
    theme(axis.title = element_text(size = 16), 
          axis.text = element_text(size = 14)) +
    transition_reveal(digit)
)

# version with rainbow-coloured bars
(
  plot_rainbow <- 
    ggplot(digitcount, aes(x = as.factor(digit), y = n)) +
    geom_col(alpha = .8, colour = "black", fill = rainbow(9)) +
    geom_image(aes(x = digit, y = n, image = image), size = .25) +
    labs(x = "digit", y = "count") +
    theme(axis.title = element_text(size = 16), 
          axis.text = element_text(size = 14)) +
    transition_reveal(digit)
)

# in case you want to save the animation
anim_save("unicorn.gif", plot_pink)
anim_save("unicorn_rainbow.gif", plot_rainbow)
