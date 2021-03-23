#'---
#'title: "Statistics Course - UWI"
#'author: "AlbyDR"
#'date: "`r format(Sys.Date())`"
#'always_allow_html: true
#'output: github_document
#'---
#'
#'## Example 2.3: Exploratory Data Analysis  
#'
suppressPackageStartupMessages({
library(tidyverse)     # ggplot, readr and dplyr packages
library(lubridate)     # date-time
library(summarytools)
library(kableExtra)
library(magrittr)
library(corrr)
library(GGally)
library(lubridate)
library(minerva)
library(colorspace) })
#'
DWD_temperature <- read_rds("DWD_temperature.rds")
DWD_precipitation <- read_rds("DWD_precipitation.rds")
#'
#' Tidy data and vector types <dbl> , <chr> and <fct>
DWD_precipitation
#' 
#' Lets have a general summary (base function) 
summary(DWD_precipitation)
#' Notice that the <chr> do not show and cloud_cover statistics make no sense as <bdl>
#' but cloud_type <fct> present the sum of the main categories or levels.
#' 
#' For categorical variables (chr and fct) make sense to check frequencies (abs, %)
freq(DWD_precipitation$sunlight_times, cumul = F, report.nas = FALSE, headings = FALSE) #
#' 
freq(DWD_precipitation$cloud_type, plain.ascii = FALSE, style = "rmarkdown")
#'
#' If you make a cross table between to categorical
ctable(DWD_precipitation$cloud_type, DWD_precipitation$precip_h, prop = "r")
#'
ggplot(DWD_precipitation) +
  geom_jitter(aes(y = cloud_type, x = precip_h))
#'
#'
DWD_precipitation %>%
  mutate(day_night = fct_collapse(sunlight_times, 
                                  "Day" = unique(sunlight_times)[-1]),
         prec_hour = as_factor(precip_h) %>% 
           fct_recode(dry = "0", raining = "1")) %$%
  #print(n=10)
  ctable(prec_hour, day_night,  useNA = "no",
         chisq = TRUE, OR = TRUE, RR = TRUE,
         headings = FALSE) %>%
   print() #method = "render"
#' **don't trust!** There are name reasons to not trust this test, one of then is the number of sample,
#' with 217687 observations the ci is to narrow as we see plotting uncertainty. Lets the other reasons 
#' to the inference course
#'
# DWD_precipitation %>%
#   filter(year(timestamp)==2020 & month(timestamp)==4) %>%
#   mutate(day_night = fct_collapse(sunlight_times, 
#                                   "Day" = unique(sunlight_times)[-1]),
#          prec_hour = as_factor(precip_h) %>% 
#            fct_recode(dry = "0", raining = "1")) %$%
#   #print(n=10)
#   ctable(day_night, prec_hour,   useNA = "no",
#          chisq = TRUE, OR = TRUE, headings = FALSE) %>%
#   print() #method = "render"
#'
#'
#' While for quantitative variables (continuous and  discrete) we can run descriptive statistics
descr(DWD_precipitation$precip_mm, stats = "all") 
#' Note: including `%>% tb()` in the end to transpose the table and `stats = "all"` to show more statistics
#' 
#' You can compare the difference between groups and see all quantitative variables together 
# stby(data = DWD_precipitation, 
#      INDICES = DWD_precipitation$sunlight_times, 
#      FUN = descr, stats = "common", transpose = F)
#'
#' Also use pipe to refine it
DWD_precipitation %>% 
  mutate(day_night = fct_collapse(sunlight_times, 
                                  "Day" = unique(sunlight_times)[-1])) %>%
  group_by(day_night = day_night) %>%
  descr(stats = "common") %>%
  print(n = "inf")
#'
#'
stby(DWD_precipitation[,c(2,5,8)], DWD_precipitation$precip_h, descr, stats = "fivenum") %>%
  tb(order = 3) %>%
  kable(format = "html", digits = 2) %>%
  collapse_rows(columns = 1, valign = "top")
#' 
#' Or you can have a general summary (descriptive statistics) for the data only by doing
#view(dfSummary(DWD_precipitation[,-4])) # excluding the variable four (cloud_cover)
#view(dfSummary(DWD_temperature))
#'
# for markdown
print(dfSummary(DWD_precipitation[,-4], graph.magnif = 0.75), method = 'render')
#'
#' join air_temp to the precipitation df
DWD_precipitation <- left_join(DWD_precipitation, DWD_temperature[,1:2], by = "timestamp" )
#'
#' Lets check the statistics
descr(DWD_precipitation[,c(2,3,5,7,9)], stats = "all")
#' **all symmetric?** not at all
#' 
#' **CV** is variation coefficient=std_dev/mean.
#' 
#' **skewness** is a measure of asymmetry. Close to 0 indicates that the distribution is symmetrical around its mean. 
#'           A positive number indicate tail on the right, whereas a negative number means the opposite.
#'           
#'**kurtosis** describes the distribution tails where higher number indicate the presence of outliers. 
#'
#' **IQR*** (Inter quartile range) is the difference between the 3rd and 1st quartile (or 75th and 25th percentiles).
#'  
#' **MAD** ( median absolute deviation) is a more robust estimate the standard deviation in the presence of outliers.
#'
#' Temperature look symmetric and approximating to a Normal distribution
DWD_precipitation %>%
  mutate(quarters = factor(quarter(timestamp, fiscal_start = 1))) %>%
  ggplot() +
  geom_density(aes(air_temp, fill = quarters), 
               alpha = 0.3, color = "white") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  scale_fill_discrete_qualitative(name = "Quarter", 
                                  palette = "Harmonic",# spacecolor package function
                                  labels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))
#' Try always to combine statistics with a plot
#' 
#' What about RH, is it symmetric?
DWD_precipitation %>%
  mutate(day_night = fct_collapse(sunlight_times, 
                                  "Day" = unique(sunlight_times)[-1])) %>%
  ggplot() +
  geom_density(aes(rel_humidity, fill = day_night), 
               alpha = 0.3, color = "white")
#' the difference between mean and med is a good indication when you don't
#' remember specific statistics such as Skewness and Kurtosis   
#'
#' What about precipitation?
DWD_precipitation %>%
  ggplot() +
  geom_density(aes(precip_mm), alpha = 0.3, fill = "blue") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed")
#' **What a long tail**
#' 
#' Lets have a close look, plot only raining periods and up to 1mm
DWD_precipitation %>%
  filter(precip_h == 1) %>%
  ggplot() +
  geom_density(aes(precip_mm), alpha = 0.3, fill = "blue") +
  scale_x_continuous(name="mm", limits = c(0,1), breaks = c(seq(0,1,0.1))) +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed")
#' **What is happening**, gotcha! the measurements system resolution is only 0.1,
#' so its not really continuous in a close look. you can check using unique(DWD_precipitation$precip_mm)
#' 
#' Lets leave this discussion to extreme events analysis
#'
#' There is only outlyiers
DWD_precipitation %>%
  filter(year(timestamp)==1999 | year(timestamp)==2019) %>%
  ggplot() +
  geom_boxplot(aes(y = precip_mm, x = factor(year(timestamp))), fill = "lightblue") 
#' **There is only outliers!** outside the whiskers 1.5 × IQR 
#' 
#' 99.3% of the data falls in this interval. So every 1000 values that are normally distributed, 
#' we expect to see about 7 outside of this range.
#' 
#' lets have a close look - excluding zeros and sum the m in a day
DWD_precipitation %>%
  filter(year(timestamp)==1999 | year(timestamp)==2019) %>%
  filter(precip_h == 1) %>%
  group_by(year = year(timestamp), doy = yday(timestamp)) %>%
  summarise(sum_mm = sum(precip_mm, na.rm = TRUE)) %>%
  #print(n=10)
  ggplot() +
  geom_boxplot(aes(y = sum_mm, x = factor(year)), fill = "lightblue") +
  scale_y_continuous(limits = c(0, 10))
#' 17 higher values was removed from the plot
#' 
#' variables like precipitation are challenging not only to present outliers, but also
#' to present many zero records. While the RH is limited by zero (not in practical terms) and 100 with
#' can also a problem, for instance a ci may contain a upper limit above 100.
#'
ggplot(DWD_precipitation, aes(sample = air_temp)) + 
  stat_qq() + stat_qq_line()
#'
ggplot(DWD_precipitation, aes(sample = precip_mm)) + 
  stat_qq() + stat_qq_line()
#'
#'
DWD_precipitation %>%
  select_if(is.numeric) %>%
correlate( use = "pairwise.complete.obs", quiet = TRUE) 
#'
#'
DWD_precipitation %>%
  select_if(is.numeric) %>%
  correlate( use = "pairwise.complete.obs", quiet = TRUE) %>% 
  focus(air_temp)
#'
#'
DWD_precipitation %>%
  filter(year(timestamp) >= 2020) %>%
  mutate(day_night = fct_collapse(sunlight_times, 
                                  "Day" = unique(sunlight_times)[-1])) %>%
  ggpairs(mapping = aes(color = day_night), columns = c(2, 3, 5, 9))
#'
#'
DWD_precipitation %>%
  filter(year(timestamp) >= 2020) %>%
  ggcorr(label = TRUE, label_alpha = TRUE)
#'
#'
DWD_precipitation %>%
  filter(year(timestamp) >= 2020 & month(timestamp) == 4) %>%
  mutate(day_night = fct_collapse(sunlight_times, 
                                  "Day" = unique(sunlight_times)[-1])) %>%
  ggscatmat(columns = c("air_temp", "precip_mm", "rel_humidity"), color = "day_night", alpha = 0.8)
#'
#' Pearson’s correlation assumes the variables to be roughly normally distributed and
#' it is not robust in the presence of outliers. Check the difference between Person and Spearman
DWD_precipitation %>% 
  summarise(r = cor(air_temp, rel_humidity, use = 'pairwise.complete.obs')) %>% pull(r)
#' Spearman method ranq x and y so it is less impacted by outliers
DWD_precipitation %>% 
  summarise(r = cor(air_temp, rel_humidity, method = "spearman", use = 'pairwise.complete.obs')) %>% pull(r)
#'There in not so difference here
#'
DWD_precipitation %>% 
  summarise(r = cor(precip_mm, rel_humidity, use = 'pairwise.complete.obs')) %>% pull(r)
#'
DWD_precipitation %>% 
  summarise(r = cor(precip_mm, rel_humidity, method = "spearman", use = 'pairwise.complete.obs')) %>% pull(r)
#' For precipitation the difference are noticeable
#' 
#' Spearman’s linear coefficient is appropriate for both quantitative and ordinal variables. 
#' In addition rank based correlations are not dependent on the normal distributional assumption and
#' they are more resistant to outliers 
#' 
#' Non-linear correlations 
mine(filter(DWD_precipitation, year(timestamp) >= 2020)[,2:5], na.rm = T)
