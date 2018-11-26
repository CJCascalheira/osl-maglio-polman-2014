####### SETUP WORKSPACE #######

# Load dependencies
library(tidyverse)

# Set working directory
setwd("~/GitHub/osl-maglio-polman-2014/src")

# Import data
maglio <- read_csv("../data/Maglio and Polman 2014 Experiment 1.csv")

####### CLEAN DATA #######

# Which stations correpond to the integers
maglio %>%
  group_by(direction, station) %>%
  summarize(mean = mean(subjective_distance), count = n())

# Station and direction into factor
maglio_clean <- within(maglio, {
  station <- factor(station, labels = c("spadina", "st_george", "bloor_yonge", "sherbourne"))
  direction <- factor(direction)
})

####### TWO-WAY ANOVA #######

# Outliers?

# Normality?

# Homoscedasticity?