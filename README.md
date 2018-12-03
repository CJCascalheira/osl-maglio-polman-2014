# Open Stats Lab Factorial ANOVA in R Using Data from Maglio & Polman (2014)
A solution to Kevin P. McIntyre's Open Stats Lab activity on factorial ANOVA using a data set from Maglio and Polman (2014). Written in R.

# Open Stats Lab
Kevin P. McIntyre developed this amazing resource for students of psychology. Check out [Open Stats Lab](https://sites.trinity.edu/osl/) for a collection of all activities.

Each activity includes an article from *Psychological Science*, a data set, and an activity to complete in SPSS. However, if you are an open source fanatic, you could also complete the activity in [JASP](https://jasp-stats.org/). For tips on how to use JASP, check out [this resource](https://osf.io/t56kg/) created by Buchanan, Hopke, and Donaldson (2018).

I prefer to get my hands deep into the data. Dr. McIntyre does not *yet* offer an R activity to accompany the work of [Maglio and Polman (2014)](https://journals.sagepub.com/doi/pdf/10.1177/0956797614530571), so here is one possible solution written in R.

**NOTE: Equations, effect sizes, and assumption graphs are not shown here. Check the R markdown document for complete analysis.**

# Analysis
I will perform assumption checks for each test prior to running it. We already know that the data meet all assumptions, otherwise the authors would have used a different analytic approach. However, checking the assumptions is helpful because:

1. reproducibility and accuracy can be verified; and
2. if you are a student, then you should form the habit of testing assumptions.

This analysis will follow the data science workflow advocated by [Garrett Grolemund and Hadley Wickham](https://r4ds.had.co.nz/introduction.html). First, we will set-up our session and import the data. Then, we must clean the data. Next, we will transform, model, and visualize the data to understand it. Finally, we will communicate our findings.

## Import
Setting up the workspace involves loading packages and importing the data.

```r
library(tidyverse)
library(car)
library(broom)
library(psych)
```

A relative path is used because the working directory is set.

```r
maglio <- read_csv("../data/Maglio and Polman 2014 Experiment 1.csv")
```

## Clean
Coding clarity is important since we did not design the study. Which integer of the `station` variable correponds to Spadina, St. George, Bloor-Yonge, and Sherbourne? It is reasonable to think that the assignment of integers reads from left to right based on the Green Line map (see activity description).

Calculation of the means enables verification of this assumption. Count is included to understand the grouping of participants better. We can compare our average subjective distance to the numbers reported in [Figure 1](https://journals.sagepub.com/doi/pdf/10.1177/0956797614530571).

```r
maglio %>%
  group_by(direction, station) %>%
  summarize(mean = mean(subjective_distance), count = n())
```

The assumption held true: 1 corresponds to Spadina, 2 to St. George, and so on.

Notice that the sample sizes in each **cell** are unequal. That is, the number of participants within each level combination of factors (i.e., Spadina East, Spadina West, etc.) varies. Four cells have 26 participants, three cells have 25 participants, and 1 cell has 23 participants. 

Uneven sample sizes across cells creates an **unbalanced design**. This will affect the code we write to analyze these data.

But let's not get ahead of ourselves. First, we must code these integers to transform `station` into a factor. Might as well transform `direction` and `orientation` into factors in the same call. The purpose of these transformations is to meet a requriement of factorial ANOVA: categorical predictors that are factors with two or more levels.

```r
maglio_clean <- within(maglio, {
  station <- factor(station, labels = c("spadina", "st_george", "bloor_yonge", "sherbourne"))
  direction <- factor(direction)
  orientation <- factor(orientation, labels = c("towards", "away"))
})
```

We know from the instructions that our task includes conducting independent t-tests. Let's split the data frame according the station and direction ahead of time.

```r
stations <- c("spadina", "st_george", "bloor_yonge", "sherbourne")

for (i in stations) {
  assign(paste(i, "east", sep = "_"), 
         maglio_clean %>% filter(station == i & direction == "EAST"))
  assign(paste(i, "west", sep = "_"), 
         maglio_clean %>% filter(station == i & direction == "WEST"))
}
```

## Understand
A factorial ANOVA compares the means across two or more independent variables, thereby splitting the sample into four or more groups. Contrast this with a one-way ANOVA, which only has one independent variable that splits the sample into two or more groups. In terms of analyzing variance, if we have more than one independent variable, then conduct a factorial ANOVA. Notice that, in both cases, we only have *one* dependent variable.

The presence of two independent variables classifies a factorial ANOVA as two-way. The assumptions of a two-way ANOVA echo those of the one-way ANOVA. Three are met before data collection, three are tested.

**Assumptions Before Data Collection**

1. **Two or more categorical independent variables** as factors with two or more levels;

2. **One continuous dependent variable** at the interval or ratio level;

3. **Independence of observations**, or a between-groups design, in which participants are separated into distinct groups with no overlap;

**Tested Assumptions**

4. **No significant outliers**, assessed with boxplots;

5. **Normality**, assessed with the Shapiro-Wilk test, histograms, Q-Q plots, and density curves for each level combination of the two factors;

6. **Homoscedasticity**, assessed with Levene's test for each combination of the two factors.

An analysis of variance is robust. It can provide valid results with minor violations of normality. Since the first three assumptions are met during experimental design, we must test for outliers, normality, and homogeneity of variances.

The null and alternative hypotheses, assessed with the *F* test, can make claims about the means between cells or claims about the interaction between factors. For this study, they are specified in the R markdown document.

Maglio and Polman (2014) conducted a 2 x 4 between-groups factorial experiment. The first factor, `direction`, has two levels: eastbound and westbound. The second factor, `station`, has four levels: Spadina, St. George, Bloor-Yonge, and Sherbourne. Thus, the sample of participants is divided into eight groups.

### Two-way ANOVA
Since we have an unbalanced design with unequal sample sizes in each cell, we must specify the type of *sum of squares*. Type-III is usually [recommended](http://www.sthda.com/english/wiki/two-way-anova-test-in-r). When the design in unbalanced, each type of *sum of squares* will yield different results.

With that in mind, let's start by constructing the analysis of variance.

```r
maglio_aov <- aov(subjective_distance ~ orientation * station, data = maglio_clean)
```

Now we summarize the model to test for interaction effects. Note that we *must* include the `options()` call **first** since the sum of squares is Type III. Failure to do so will produce a correct effect with the interaction, but spurious effects for each factor by itself. In this case, failure to set `options()` will cause all effects to appear significant.

```r
options(contrasts = c("contr.sum", "contr.poly"))
(maglio_anova <- Anova(maglio_aov, type = "III"))
```

There are two significant effects, one on station and the other on the interaction between orientation and station. Therefore, we should assess the effect size, or partial eta-squared. This is given by the equation found in the R markdown document.

We can use `broom::tidy` to sweep the model's output into a tidy data frame. This will prevent entry errors and promote exact calculations.

```r
(maglio_tidy <- tidy(maglio_anova))
```

Now, calculating the effect size is a simple plug-in procedure.

```r
(eta_station <- maglio_tidy$sumsq[3] / (maglio_tidy$sumsq[3] + maglio_tidy$sumsq[5]))
(eta_interact <- maglio_tidy$sumsq[4] / (maglio_tidy$sumsq[4] + maglio_tidy$sumsq[5]))
```

The two-way ANOVA revealed no main effect on orientaion, *F* < 1, *p* = .573. The *F*-value did not exceed the critical value. However, there was a significant effect on station, *F*(3, 194) = 24.10, *p* < .001. The interaction between orientation and station was also significant, *F*(3, 194) = 16.28, *p* < .001.

#### Outliers?
Outliers can reduce the accuracy of results and skew data.

```r
ggplot(maglio_clean, aes(x = station, y = subjective_distance, color = orientation)) +
  geom_boxplot()
```

There are three outliers present.

#### Normality?
The null hypothesis of the Shapiro-Wilk test is that the data are normally distributed. Remember that, for analysis of variance, we test the normality of residuals.

```r
aov_residuals <- residuals(maglio_aov)
shapiro.test(aov_residuals)
```

Since *p* < .001, we reject the null hypothesis: the data are not normally distributed. The ANOVA is a robust procedure, so it can withstand violations of normality. Let's visualize the distribution.

One method is the Q-Q plot, a visualization that compares the quantiles of a normal distribution against the quantiles of the model's residuals. Normally distributed residuals will follow a straight diagonal line.

```r
plot(maglio_aov, 2)
```

Here, we see that the residuals undulate along the line. The pattern seems consistent if not normal, despite the labeled outliers in the upper right-hand corner of the plot.

What does a density curve for each cell (i.e., group) reveal?

```r
ggplot(maglio_clean, aes(x = subjective_distance)) +
  geom_density() +
  facet_grid(orientation ~ station)
```

The data are clearly not normal. They are not even skewed in similar directions!

#### Homoscedasticity?
The null hypothesis of Levene's test is that the variances are homogenous.

```r
leveneTest(subjective_distance ~ orientation * station, data = maglio_clean)
```

There is no evidence in favor of rejecting the null hypothesis, *F* < 1, *p* = .773. The data are homoscedastic.

We can also visualize homogenity of variances as the relationship between residuals and fitted values.

```r
plot(maglio_aov, 1)
```

The plot shows no relationship. The red line is practically at zero. 

### Independent t-Tests
The assumptions of the independent t-test are nearly identical to the two-way ANOVA, except for the requirement of the independent variable, which in this case, must be a single factor with two or more levels. The results from the three assumptions that are tested (i.e., outliers, normality, and homoscedasticity) will likely be inherited from the ANOVA. However, we will test these assumptions below for each independent t-test.

We shall delineate the interaction between orientation and station by comparing the subjective-distance scores across eastbound and westbound participants for each of the stations.

Social scientists typically report Cohen's *d* as the effect size of independent t-tests. Cohen's *d* is standardized. In the present study, the author's chose to report partial eta-squared, which indicates the [proportion of overlap](http://psychology.okstate.edu/faculty/jgrice/psyc3214/independentsamples_t_test.pdf) between the factor and the reponse variable. Effect sizes are considered:

* **small** = .01
* **medium** = .06
* **large** = .14

The formula to calculate partial eta-squared for independent t-tests is specified in the R markdown document.

We will tidy the t-test model like we did above. Once the model is in a data frame, `statistic` corresponds to the observed *t*-value, whereas parameter corresponds to the degrees of freedom.

#### Spadina
```r
# Spadina t-test
(spadina_t <- t.test(spadina_east$subjective_distance, spadina_west$subjective_distance,
                     paired = FALSE, var.equal = TRUE))

# Spadina effect size
(spadina_tidy <- tidy(spadina_t))
(eta_spadina <- spadina_tidy$statistic^2 / (spadina_tidy$statistic^2 + spadina_tidy$parameter))
```

Westbound participants rated Spadina as being closer than eastbound participants, *p* < .001.

##### Outliers?
```r
maglio_clean %>% filter(station == "spadina") %>%
  ggplot(aes(x = direction, y = subjective_distance)) +
    geom_boxplot()
```

There are two outliers.

##### Normality?
```r
with(spadina_east, shapiro.test(subjective_distance))
with(spadina_west, shapiro.test(subjective_distance))
```

Both *p*s < .05, so we reject the null hypothesis: the data are not normal.

##### Homoscedasticity?
```r
var.test(spadina_east$subjective_distance, spadina_west$subjective_distance)
```

The variances are homogenous, *p* = .647.

#### St. George
```r
# St. George t-test
(st_george_t <- t.test(st_george_east$subjective_distance, st_george_west$subjective_distance,
                      paired = FALSE, var.equal = TRUE))

# St. George effect size
(st_george_tidy <- tidy(st_george_t))
(eta_st_george <- st_george_tidy$statistic^2 / 
    (st_george_tidy$statistic^2 + st_george_tidy$parameter))
```

Westbound participants rated St. George as being closer than eastbound participants, *p* < .001.

##### Outliers?
```r
maglio_clean %>% filter(station == "st_george") %>%
  ggplot(aes(x = direction, y = subjective_distance)) +
  geom_boxplot()
```

There is one outlier among eastbound participants.

##### Normality?
```r
with(st_george_east, shapiro.test(subjective_distance))
with(st_george_west, shapiro.test(subjective_distance))
```

Since both *p*s < .05, we accept the alternative hypothesis: the data are not normally distributed.

##### Homoscedasticity?
```r
var.test(st_george_east$subjective_distance, st_george_west$subjective_distance)
```

Since *p* = .241, we accept the null hypothesis: the data are homoscedastic.

#### Bloor-Yonge
```r
# Bloor-Yonge t-test
(bloor_yonge_t <- t.test(bloor_yonge_west$subjective_distance,
                         bloor_yonge_east$subjective_distance,
                         paired = FALSE, var.equal = TRUE))

# Bloor-Yonge effect size
(bloor_yonge_tidy <- tidy(bloor_yonge_t))
(eta_bloor_yonge <- bloor_yonge_tidy$statistic^2 /
    (bloor_yonge_tidy$statistic^2 + bloor_yonge_tidy$parameter))
```

Eastbound participants rated Bloor-Yonge as being closer than westbound participants, *p* = .053.

##### Outliers?
```r
maglio_clean %>% filter(station == "bloor_yonge") %>%
  ggplot(aes(x = direction, y = subjective_distance)) +
  geom_boxplot()
```

No outliers present.

##### Normality?
```r
with(bloor_yonge_west, shapiro.test(subjective_distance))
with(bloor_yonge_east, shapiro.test(subjective_distance))
```

Both *p*s < .05, so we reject the null hypothesis. The normality assumption is violated.

##### Homoscedasticity?
```r
var.test(bloor_yonge_west$subjective_distance, bloor_yonge_east$subjective_distance)
```

Since *p* < .05, we reject the null hypothesis: the variances are heterogenous.

#### Sherbourne
```r
# Sherbourne t-test
(sherbourne_t <- t.test(sherbourne_west$subjective_distance, sherbourne_east$subjective_distance,
                        paired = FALSE, var.equal = TRUE))

# Sherbourne effect size
(sherbourne_tidy <- tidy(sherbourne_t))
(eta_sherbourne <- sherbourne_tidy$statistic^2 / 
    (sherbourne_tidy$statistic^2 + sherbourne_tidy$parameter))
```

Eastbound participants rated Sherbourne as being closer than westbound participants, *p* < .001.

##### Outliers?
```r
maglio_clean %>% filter(station == "sherbourne") %>%
  ggplot(aes(x = direction, y = subjective_distance)) +
  geom_boxplot()
```

No outliers.

##### Normality?
```r
with(sherbourne_west, shapiro.test(subjective_distance))
with(sherbourne_east, shapiro.test(subjective_distance))
```

The data are not normally distributed (both *p*s < .05).

##### Homoscedasticity?
```r
var.test(sherbourne_west$subjective_distance, sherbourne_east$subjective_distance)
```

The variances are equal (*p* = .919).

### Visualize
Let's generate a line graph to depict the relationship between station and the direction of travel, which corresponds to orientation. 

First we need to construct an APA-style theme.

```r
apa_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12))
```

Next, we need the mean and standard error of subjective-distance ratings for westbound and eastbound participants for each station. The following code is clunky, but it accomplishes the task.

```r
sp_e <- describe(spadina_east$subjective_distance)[,-c(1:2,4:12)]
sp_w <- describe(spadina_west$subjective_distance)[,-c(1:2,4:12)]
st_e <- describe(st_george_east$subjective_distance)[,-c(1:2,4:12)]
st_w <- describe(st_george_west$subjective_distance)[,-c(1:2,4:12)]
bl_e <- describe(bloor_yonge_east$subjective_distance)[,-c(1:2,4:12)]
bl_w <- describe(bloor_yonge_west$subjective_distance)[,-c(1:2,4:12)]
sh_e <- describe(sherbourne_east$subjective_distance)[,-c(1:2,4:12)]
sh_w <- describe(sherbourne_west$subjective_distance)[,-c(1:2,4:12)]
```

Bind the temporary variables holding the descriptive statistics into one variable, adding two new columns for direction and station.

```r
descriptives <- rbind(sp_e, sp_w, st_e, st_w, bl_e, bl_w, sh_e, sh_w)

# Add columns
(descriptives <- descriptives %>%
  mutate(
    direction = c("east", "west", "east", "west", "east", "west", "east", "west"),
    station = c("spadina", "spadina", "st_george", "st_george", 
                "bloor_yonge", "bloor_yonge", "sherbourne", "sherbourne")
  ))
```

Transform the variables station and direction into factors with pretty labels.

```r
# Station
descriptives$station <- factor(descriptives$station, 
                               levels = c("spadina", "st_george", "bloor_yonge", "sherbourne"),
                               labels = c("Spadina", "St. George", "Bloor-Yonge", "Sherbourne"))

# Direction
descriptives$direction <- factor(descriptives$direction,
                                 levels = c("east", "west"),
                                 labels = c("Eastbound", "Westbound"))
```

Finally, we construct the line graph to [plot this two-way design](https://drsimonj.svbtle.com/mean-and-ci-plot-for-twoway-designs-using-ggplot2).

```r
ggplot(descriptives, aes(x = station, y = mean, group = direction)) +
  geom_line(aes(linetype = direction), size = 1) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  geom_point(size = 5, position = position_dodge(width = 0.01)) +
  geom_point(size = 4, position = position_dodge(width = 0.01), color = "white") +
  expand_limits(y = c(0, 5)) +
  scale_y_continuous(breaks = seq(0, 5, 0.5), expand = c(0, 0)) +
  guides(linetype = guide_legend("")) +
  labs(x = "", y = "Subjective Distance") +
  apa_theme +
  theme(legend.position = c(0.1, 0.99))
```

![Subjective-distance rating for each station by direction.](https://github.com/corycaaz/osl-maglio-polman-2014/blob/master/data/results/figure_1.png)

**Figure 1**: The mean subjective-distance rating for each group based on their orientation towards and away from the four stations.

## Communicate
The two-way ANOVA revealed no main effect on orientaion, *F* < 1, *p* = .573. The *F*-value did not exceed the critical value. However, there was a significant effect on station, *F*(3, 194) = 24.10, *p* < .001. The interaction between orientation and station was also significant, *F*(3, 194) = 16.28, *p* < .001. Delineation of this interaction revealed that eastbound participants perceived stations to the East as being closer than did westbound participants, both for Bloor-Yonge (*p* = .053) and Sherbourne (*p* < .001). Travlers heading west from Bay station rated the subjective-distance of St. George (*p* < .001) and Spadina (*p* < .001) as closer than did eastbound participants.

# Acknowledgements
I am thankful for my advisor, Dr. Brandt A. Smith for introducing me to R, JASP, and OSL. The discipline of psychology is advocating for preregistered, open materials. His encouragement to utilize open data and open source software has positioned me in the middle of the reproducible movement.

I would still be clicking checkboxes and dropdowns to analyze data if it were not for [DataCamp](https://www.datacamp.com), [Rose Maier](https://rstudio-pubs-static.s3.amazonaws.com/65059_586f394d8eb84f84b1baaf56ffb6b47f.html), [Alboukadel Kassambara](http://www.sthda.com/english/wiki/two-way-anova-test-in-r), [Jonathan Baron](https://www.sas.upenn.edu/~baron/from_cattell/rpsych/rpsych.html#htoc60), and the team behind [personality-project](http://personality-project.org/r/r.guide.html#withinone).

## Dependencies
This analysis was performed using the RStudio IDE.