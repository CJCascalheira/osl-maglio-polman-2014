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

# Split data for independent t-tests
stations <- c("spadina", "st_george", "bloor_yonge", "sherbourne")

for (i in stations) {
  assign(paste(i, "east", sep = "_"), 
         maglio_clean %>% filter(station == i & direction == "EAST"))
  assign(paste(i, "west", sep = "_"), 
         maglio_clean %>% filter(station == i & direction == "WEST"))
}

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
ggplot(maglio_clean, aes(x = station, y = subjective_distance, color = orientation)) +
  geom_boxplot()

# Normality?
aov_residuals <- residuals(maglio_aov)
shapiro.test(aov_residuals)

# QQ plot of residuals
plot(maglio_aov, 2)

# Density curve of subjective distance
ggplot(maglio_clean, aes(x = subjective_distance)) +
  geom_density() +
  facet_grid(orientation ~ station)

# Homoscedasticity?
leveneTest(subjective_distance ~ orientation * station, data = maglio_clean)

# Residuals versus fitted values 
plot(maglio_aov, 1)

####### INDEPENDENT T-TESTS #######

### Spadina
(spadina_t <- t.test(spadina_east$subjective_distance, spadina_west$subjective_distance,
                     paired = FALSE, var.equal = TRUE))

# Spadina effect size
(spadina_tidy <- tidy(spadina_t))
(eta_spadina <- spadina_tidy$statistic^2 / (spadina_tidy$statistic^2 + spadina_tidy$parameter))

# Outliers?
maglio_clean %>% filter(station == "spadina") %>%
  ggplot(aes(x = direction, y = subjective_distance)) +
  geom_boxplot()

# Normality?
with(spadina_east, shapiro.test(subjective_distance))
with(spadina_west, shapiro.test(subjective_distance))

# Homoscedasticity?
var.test(spadina_east$subjective_distance, spadina_west$subjective_distance)

############################################################################

### St. George
(st_george_t <- t.test(st_george_east$subjective_distance, st_george_west$subjective_distance,
                      paired = FALSE, var.equal = TRUE))

# St. George effect size
(st_george_tidy <- tidy(st_george_t))
(eta_st_george <- st_george_tidy$statistic^2 / (st_george_tidy$statistic^2 + st_george_tidy$parameter))

# Outliers?
maglio_clean %>% filter(station == "st_george") %>%
  ggplot(aes(x = direction, y = subjective_distance)) +
  geom_boxplot()

# Normality?
with(st_george_east, shapiro.test(subjective_distance))
with(st_george_west, shapiro.test(subjective_distance))

# Homoscedasticity?
var.test(st_george_east$subjective_distance, st_george_west$subjective_distance)

############################################################################

### Bloor-Yonge
(bloor_yonge_t <- t.test(bloor_yonge_west$subjective_distance, bloor_yonge_east$subjective_distance,
                         paired = FALSE, var.equal = TRUE))

# Bloor-Yonge effect size
(bloor_yonge_tidy <- tidy(bloor_yonge_t))
(eta_bloor_yonge <- bloor_yonge_tidy$statistic^2 / (bloor_yonge_tidy$statistic^2 + bloor_yonge_tidy$parameter))

# Outliers?
maglio_clean %>% filter(station == "bloor_yonge") %>%
  ggplot(aes(x = direction, y = subjective_distance)) +
  geom_boxplot()

# Normality?
with(bloor_yonge_west, shapiro.test(subjective_distance))
with(bloor_yonge_east, shapiro.test(subjective_distance))

# Homoscedasticity?
var.test(bloor_yonge_west$subjective_distance, bloor_yonge_east$subjective_distance)

############################################################################

### Sherbourne
(sherbourne_t <- t.test(sherbourne_west$subjective_distance, sherbourne_east$subjective_distance,
                        paired = FALSE, var.equal = TRUE))

# Sherbourne effect size
(sherbourne_tidy <- tidy(sherbourne_t))
(eta_sherbourne <- sherbourne_tidy$statistic^2 / (sherbourne_tidy$statistic^2 + sherbourne_tidy$parameter))

# Outliers?
maglio_clean %>% filter(station == "sherbourne") %>%
  ggplot(aes(x = direction, y = subjective_distance)) +
  geom_boxplot()

# Normality?
with(sherbourne_west, shapiro.test(subjective_distance))
with(sherbourne_east, shapiro.test(subjective_distance))

# Homoscedasticity?
var.test(sherbourne_west$subjective_distance, sherbourne_east$subjective_distance)

############################################################################
