####### SETUP WORKSPACE #######

# Load dependencies
library(tidyverse)
library(car)
library(broom)

# Set working directory
setwd("~/GitHub/osl-maglio-polman-2014/src")

# Import data
maglio <- read_csv("../data/Maglio and Polman 2014 Experiment 1.csv")

####### CLEAN DATA #######

# Which stations correpond to the integers
maglio %>%
  group_by(direction, station) %>%
  summarize(mean = mean(subjective_distance), count = n())

# Station, direction, orientation into factor
maglio_clean <- within(maglio, {
  station <- factor(station, labels = c("spadina", "st_george", "bloor_yonge", "sherbourne"))
  direction <- factor(direction)
  orientation <- factor(orientation, labels = c("towards", "away"))
})

####### TWO-WAY ANOVA #######

# Construct aov object
maglio_aov <- aov(subjective_distance ~ orientation * station, data = maglio_clean)

# Conduct ANOVA with type III sum of squares
options(contrasts = c("contr.sum", "contr.poly"))
(maglio_anova <- Anova(maglio_aov, type = "III"))

# Tidy model to calculate effect size
(maglio_tidy <- tidy(maglio_anova))

# Effect size for station
(eta_station <- maglio_tidy$sumsq[3] / (maglio_tidy$sumsq[3] + maglio_tidy$sumsq[5]))

# Effect size for interaction
(eta_interact <- maglio_tidy$sumsq[4] / (maglio_tidy$sumsq[4] + maglio_tidy$sumsq[5]))

# Outliers?

# Normality?

# Homoscedasticity?
plot(maglio_aov, 1)

