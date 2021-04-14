#'---
#'title: "Statistics Course - UWI"
#'author: "AlbyDR"
#'date: "`r format(Sys.Date())`"
#'always_allow_html: true
#'output: github_document
#'---
#'
#'## Example 3.2: Interval od Confidence and Hypothesis Test
#'### 
suppressPackageStartupMessages({
  library(tidyverse)     # ggplot, readr and dplyr packages
  library(lubridate)     # date-time
  library(infer)
  library(tidymodels)
  library(summarytools)
  library(moderndive)
  library(extRemes)
  })  # colours
#'
#'
DWD_temperature <- read_rds("DWD_temperature.rds")
DWD_precipitation <- read_rds("DWD_precipitation.rds")
#'
#'
#' ## Confidence Intervals
DWD_temperature %>%
filter(year(timestamp) >= 1900) %>%
  group_by(year = year(timestamp),
           doy = yday(timestamp)) %>%
  summarise(mean = mean(air_temp, na.rm=T)) %>%
  summarise(mean_cl_normal(mean)) %>%
#print(n=10)
  ggplot(aes(x = year, y = y)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.3) +
  geom_jitter(size = 1, color = "blue") +
  scale_x_continuous(name="year", breaks = c(seq(1900,2020,5)))+
  scale_y_continuous(limits = c(5.5, 12.5), breaks=c(6:13)) +
  labs(x = 'year', y = 'air temperature (average +/- ci)') +
  theme(axis.text.x = element_text(color="grey25", size=8, angle=90))
#'
#' Distribution of the proportion of precipitation per hour
DWD_precipitation %>%
  filter(year(timestamp) == 2020) %>%
  mutate(precip_h = as_factor(precip_h) %>% fct_recode(dry = "0", raining = "1")) %>%
  specify(response = precip_h, success = "raining") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop")  %>%
  visualize()
#'
#' generate samples based on the population
DWD_precipitation %>%
  filter(year(timestamp) == 2020) %>%
  mutate(precip_h = as_factor(precip_h) %>% fct_recode(dry = "0", raining = "1")) %>%
  specify(response = precip_h, success = "raining") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop") -> stat_prop
#'
stat_prop
#'
#' calculate the CI
get_confidence_interval(stat_prop, level = 0.95) -> stat_prop_ci
stat_prop_ci
get_confidence_interval(stat_prop, level = 0.99) -> stat_prop_ci
stat_prop_ci
#'
#' visualize CI
visualize(stat_prop, bin = 20) +
  shade_ci(endpoints = stat_prop_ci, color = "red", fill = "lightblue", alpha = 0.3) +
  geom_vline(xintercept = 0.166, linetype = "dashed")
#'
#' If we repeated our sampling procedure a large number of times, we expect about 95% of
#' the times the confidence intervals will capture the value of the population parameter proportion.
#'
#' Distribution of the data precipitation in mm
DWD_precipitation %>%
  filter(year(timestamp) >= 2020) %>%
  ggplot() +
  geom_histogram(aes(x = precip_mm) , bins = 250, colour = "blue", fill = "lightblue")
#'
#' Distribution of the mean of precipitation in mm
DWD_precipitation %>%
  filter(year(timestamp) == 2020) %>%
  specify(response = precip_mm) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")  -> stat_mean
#'
# calculate the CI
get_confidence_interval(stat_mean, level = 0.99) -> stat_mean_ci
stat_mean_ci
#'
# visualize CI
visualize(stat_mean, bin = 20) +
  shade_ci(endpoints = stat_mean_ci, color = "red", fill = "lightblue", alpha = 0.3)
#'
#' If we repeated our sampling procedure a large number of times, we expect about 99% of
#' the times the confidence intervals will capture the value of the population parameter mean
#'
#'## Hypothesis Test
#'
#' define null hypothesis for a difference in proportion p1 - p2 = 0
#' p1 raining or dry and p2 Cumulus cloud or non-cumulus
null_distribution <- DWD_precipitation %>%
  filter(year(timestamp) == 2020  & month(timestamp) == 5) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Cumulus", other_level = "non-cumulus"),
         precip_h = as_factor(precip_h) %>% fct_recode(dry = "0", raining = "1")) %>%
  specify(formula = precip_h ~ cloud, success = "raining") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "diff in props", order = c("Cumulus", "non-cumulus"))
#'
null_distribution
#'
#' calculate the statistic test with the data for the alternative hypothesis p1 - p2 > 0
obs_diff_prop <- DWD_precipitation %>%
  filter(year(timestamp) == 2020  & month(timestamp) == 5) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Cumulus", other_level = "non-cumulus"),
         precip_h = as_factor(precip_h) %>% fct_recode(dry = "0", raining = "1")) %>%
  specify(formula = precip_h ~ cloud, success = "raining") %>%
  calculate(stat = "diff in props", order = c("Cumulus", "non-cumulus"))
#'
obs_diff_prop
#'
#' visualize
visualize(null_distribution, bins = 10) +
  shade_p_value(obs_stat = obs_diff_prop, direction = "right")
#'
#' check the p-value
null_distribution %>%
  get_p_value(obs_stat = obs_diff_prop, direction = "right")
#'
#'
#' lets do the same but now Stratus cloud vs others cloud
null_distribution <- DWD_precipitation %>%
  filter(year(timestamp) == 2020 & month(timestamp) == 5) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Stratus", other_level = "non-stratus"),
         precip_h = as_factor(precip_h) %>% fct_recode(dry = "0", raining = "1")) %>%
  specify(formula = precip_h ~ cloud, success = "raining") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("Stratus", "non-stratus"))
#'
null_distribution
#'
obs_diff_prop <- DWD_precipitation %>%
  filter(year(timestamp) == 2020 & month(timestamp) == 5) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Stratus", other_level = "non-stratus"),
         precip_h = as_factor(precip_h) %>% fct_recode(dry = "0", raining = "1")) %>%
  specify(formula = precip_h ~ cloud, success = "raining") %>%
  calculate(stat = "diff in props", order = c("Stratus", "non-stratus"))
#'
obs_diff_prop
#'
visualize(null_distribution, bins = 20) +
  shade_p_value(obs_stat = obs_diff_prop, direction = "right")
#'
null_distribution %>%
  get_p_value(obs_stat = obs_diff_prop, direction = "right")
#'
#'
#' using diff in means distribution to test mean
descr(DWD_precipitation$air_temp)

DWD_precipitation %>%
  filter(year(timestamp) == 2020 & month(timestamp) == 4) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Cumulus", other_level = "non-cumulus")) %>%
  ggplot(aes(x = cloud, y = air_temp)) +
  geom_boxplot() +
  labs(x = "cloud", y = "air_temp")
#'
null_distribution <- DWD_precipitation %>%
  filter(year(timestamp) == 2020 & month(timestamp) == 4) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Cumulus", other_level = "non-cumulus")) %>%
  specify(formula = air_temp ~ cloud) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Cumulus", "non-cumulus"))  # "diff in means"
#'
null_distribution
#'
obs_diff_mean <- DWD_precipitation %>%
  filter(year(timestamp) == 2020 & month(timestamp) == 4) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Cumulus", other_level = "non-cumulus")) %>%
  specify(formula = air_temp ~ cloud) %>%
  calculate(stat = "diff in means", order = c("Cumulus", "non-cumulus"))
#'
obs_diff_mean
#'
visualize(null_distribution, bins = 20) +
  shade_p_value(obs_stat = obs_diff_mean, direction = "left")
#'
null_distribution %>%
  get_p_value(obs_stat = obs_diff_mean, direction = "left")
#'
#'
#'
#'
DWD_precipitation %>%
  filter(year(timestamp) <= 2019) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Stratus", other_level = "non-stratus"),
         prec_hour = as_factor(precip_h) %>%
           fct_recode(dry = "0", raining = "1")) %$%
  #print(n = 10)
  ctable(prec_hour, cloud ,   useNA = "no",
         chisq = TRUE, OR = TRUE, headings = FALSE) %>%
  print() #method = "render"
#'
#' despite the p-value is very small, the odds ratio is almost one (no difference),
#' this is an effect of the sample size that reduce the SE and make narrow CI that
#' not contain 1 but are very both lower and upper intervals very close to it.
#'
DWD_precipitation %>%
  filter(year(timestamp)==2019) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Cumulus", other_level = "non-cumulus"),
         prec_hour = as_factor(precip_h) %>%
           fct_recode(dry = "0", raining = "1")) %$%
  #print(n = 10)
  ctable(prec_hour,cloud ,   useNA = "no",
         chisq = TRUE, OR = TRUE, headings = FALSE) %>%
  print() #method = "render"
#'
#'
#' **don't trust!** There are name reasons to not trust this test, one of then is the number of sample,
#' with 217687 observations the ci is to narrow as we see plotting uncertainty. Lets the other reasons 
#' to the inference course
#' 
#'
unique(DWD_precipitation$cloud_type)
#'
fct_count(DWD_precipitation$cloud_type)[10,]
#'
#'
DWD_precipitation %>%
  filter(year(timestamp)==2020 & month(timestamp)==4) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Cumulus", other_level = "non-cumulus"),
         prec_hour = as_factor(precip_h) %>%
           fct_recode(dry = "0", raining = "1")) %$%
  #print(n = 10)
  ctable(prec_hour,cloud ,   useNA = "no",
         chisq = TRUE, OR = TRUE, headings = FALSE) %>%
  print() #method = "render"
#'
DWD_precipitation %>%
  filter(year(timestamp)==2020 & month(timestamp)==12) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Cumulus", other_level = "non-cumulus"),
         prec_hour = as_factor(precip_h) %>%
           fct_recode(dry = "0", raining = "1")) %$%
  #print(n = 10)
  ctable(prec_hour,cloud ,   useNA = "no",
         chisq = TRUE, OR = TRUE, headings = FALSE) %>%
  print() #method = "render"
#'
DWD_precipitation %>%
  filter(year(timestamp)<=2019 & month(timestamp)==4) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Cumulus", other_level = "non-cumulus"),
         prec_hour = as_factor(precip_h) %>%
           fct_recode(dry = "0", raining = "1")) %$%
  #print(n = 10)
  ctable(prec_hour,cloud ,   useNA = "no",
         chisq = TRUE, OR = TRUE, headings = FALSE) %>%
  print() #method = "render"
#'
DWD_precipitation %>%
  filter(year(timestamp) == 2019) %>%
  mutate(cloud = fct_other(cloud_type, keep = "Stratus", other_level = "non-stratus"),
         prec_hour = as_factor(precip_h) %>%
           fct_recode(dry = "0", raining = "1")) %$%
  #print(n = 10)
  ctable(prec_hour, cloud ,   useNA = "no",
         chisq = TRUE, OR = TRUE, headings = FALSE) %>%
  print() #method = "render"
#'
#' Note that 1 is not included in the confidence interval which must mean that the 
#' p-value is smaller than 0.05. We can confirm this using:
#'
  DWD_precipitation %>%
  filter(year(timestamp) == 2019 & month(timestamp) == 9) %>%
  mutate(day_night = fct_collapse(sunlight_times,
                                  "Day" = unique(sunlight_times)[-1]),
         prec_hour = as_factor(precip_h) %>%
           fct_recode(dry = "0", raining = "1")) %$%
  #print(n=10)
  ctable(day_night, prec_hour,   useNA = "no",
         chisq = TRUE, headings = FALSE) %>%
  print() #method = "render"
#'
DWD_precipitation %>%
  filter(year(timestamp) <= 2010 & month(timestamp) == 9) %>%
  mutate(day_night = fct_collapse(sunlight_times,
                                  "Day" = unique(sunlight_times)[-1]),
         prec_hour = as_factor(precip_h) %>%
           fct_recode(dry = "0", raining = "1")) %$%
  #print(n=10)
  ctable(day_night, prec_hour,   useNA = "no",
         chisq = TRUE, headings = FALSE) %>%
  print() #method = "render"
#'
#'
#'
DWD_temperature %>%
  filter(year(timestamp) >= 1900) %>%
  group_by(year = year(timestamp)) %>%
  summarise(mean = mean(air_temp, na.rm=T)) %>% 
  #print(n=10)
  ggplot(aes(x = year, y = mean)) +
  geom_jitter(size = 2, color = "blue") +
  geom_smooth(n = 200,  span = 0.1, level = 0.99) +
  scale_x_continuous(name="year", breaks = c(seq(1900,2020,5)))+
  scale_y_continuous(limits = c(6.3, 11.6), breaks=c(6:13)) +
  labs(x = 'year', y = 'air temperature (average +/- ci)') +
  theme(axis.text.x = element_text(color="grey25", size=8, angle=90))  
#'
