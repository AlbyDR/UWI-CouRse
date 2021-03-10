#'---
#'title: "Statistics Course - UWI"
#'author: "AlbyDR"
#'date: "`r format(Sys.Date())`"
#'output: github_document
#'---
#'
#'## Example 2.2:Visualization - Part I: Categorical data
#'
#'
library(tidyverse)
library(lubridate)
DWD_data <- read_rds("DWD_prec_data.rds")
#'
#' 
#'### Plotting categorical data
#'
#' The visualization of Categorical variables are limited to represent 
#' counts of cases in the chr vector (e.g. bar height) or used as a factor 
#' to compare groups (cross-tabulated data) using a second variable (discrete, continuous or categorical).
#' Always check first if the variable is nominal or ordinal. 
#' 
#' You can add many different plots inside of a `ggplot() +`, here 
#' we will focus on `geom_bar()` or `geom_col()`
#' 
#' **Bar Graph**
#' 
#' Lets start simple
fct_count(DWD_data$cloud_cover)
#'
#' *Plot*: 
#'* cloud_cover variable as it was not a factor
ggplot(DWD_data, aes(x=unclass(cloud_cover))) +
geom_bar()
#'
#' Using the aes in the geom_ the result is the same
#' `ggplot(DWD_data) +`
#'  `geom_bar(aes(x=unclass(cloud_cover)))`
#'
#'* Cloud_cover variable as a factor
ggplot(DWD_data, aes(x=cloud_cover)) +
  geom_bar()
#'
#' Plot ordering cloud_cover according the frequency 
#' 
#' Lets introduce pipe operator `%>%` to manipulate and summarise variables 
#' (i.e.vectors) without create a new variable or dataset.Include `%>% fct_rev()` to revert.
#' 
DWD_data %>% 
  mutate(cloud_cover = cloud_cover %>% fct_infreq()) %>% 
  count(cloud_cover, sort = TRUE) %>%
  print(n = Inf)  # print all the lines
#' 
#' Very similar to `fct_count(DWD_data$cloud_cover)` but in order according to n
#' 
DWD_data %>% 
  mutate(cloud_cover = cloud_cover %>% fct_infreq()) %>%  
  ggplot(aes(cloud_cover)) + 
  geom_bar()
#' 
#' Reducing to 6 categories (aggregate remains on others)
DWD_data %>%
  mutate(cloud_cover = fct_lump(cloud_cover, n = 6)) %>%
  count(cloud_cover, sort = TRUE) %>%
  print(n = Inf)
#'
DWD_data %>%
  mutate(cloud_cover = fct_lump(cloud_cover, n = 6)) %>%
  ggplot(aes(cloud_cover)) + 
  geom_bar()
#' 
#' Just in case you miss a pie chart,... just in case
DWD_data %>% 
  mutate(cloud_cover = cloud_cover %>% fct_infreq()) %>% 
  count(cloud_cover, sort = TRUE) %>%
  ggplot() +
  geom_col(aes(x = 1, y = n, fill = cloud_cover), position = "fill") +
  coord_polar(theta = "y") # trick, check without this line
#' 
#' Can be better!
DWD_data %>% 
  mutate(cloud_cover = cloud_cover %>% fct_infreq()) %>% 
  count(cloud_cover, sort = TRUE) %>%
  ggplot() +
  geom_col(aes(x = 1, y = n, fill = cloud_cover), position = "fill") +
  geom_text(aes(x = 0, y = 0, label = "Cloud")) + # text in the middle
  coord_polar(theta = "y") +
  xlim(0, 1.5) + # role in the middle
  theme_bw() +
  theme(axis.title = element_blank(), # taking out the axis and extra info
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background=element_blank(),
        strip.text=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
#'
#' **Combining two variables in a plot**
#' 
ggplot(DWD_data, aes(x = cloud_cover, y=precip_mm, fill = cloud_cover)) +
  geom_col(position = "dodge")
#' 
ggplot(DWD_data, aes(x = year(timestamp), y=precip_mm, fill = cloud_cover)) +
  geom_col(position = "dodge")
#' 
#' Lets filter for the years 1999 and 2019
DWD_data %>% 
  filter(year(timestamp)==1999 | year(timestamp)==2019) %>%
  #print(n = 10)  
  ggplot() +
  geom_col(aes(x = factor(year(timestamp)), y = precip_mm, 
               fill = cloud_cover), position = "dodge")
#' 
#' We can do better!
DWD_data %>%
  filter(year(timestamp)==1999 | year(timestamp)==2019) %>% 
  mutate(cloud_cover = fct_lump(cloud_cover, n = 3)) %>%
  group_by(year=year(timestamp), cloud=cloud_cover) %>%
  summarise(n = n(),
            mm = sum(precip_mm, na.rm=T))%>%
  mutate(freq = mm / sum(mm), ) %>%
  #print(n = Inf)
  ggplot(aes(x = cloud, y = freq, fill = factor(year))) +
  geom_col(position = "dodge")
#' 
#' **Stacked bar graph**
DWD_data %>%
  mutate(cloud_cover = fct_lump(cloud_cover, n = 3),
         sunlight_times = fct_collapse(sunlight_times, 
                                       "Day" = unique(sunlight_times)[-1])) %>%
  group_by(sunlight=sunlight_times, cloud=cloud_cover) %>%
  summarise(n = n(),
            mm = sum(precip_mm, na.rm=T))%>%
  #print(n = Inf)
  ggplot(aes(x = cloud, y = mm, fill = factor(sunlight))) +
  geom_col()
#' 
#' 100% stacked bar graph
DWD_data %>%
  filter(year(timestamp)==1999 | year(timestamp)==2019) %>% 
  mutate(cloud_cover = fct_lump(cloud_cover, n = 3)) %>%
  group_by(year=year(timestamp), cloud=cloud_cover) %>%
  summarise(n = n(),
            mm = sum(precip_mm, na.rm=T))%>%
  mutate(freq = mm / sum(mm), ) %>%
  #print(n = Inf)
  ggplot(aes(x =factor(year) , y = freq, fill = cloud)) +
  geom_col()
#' 
#' 
#' Exercise: 
#' 1. Run the plot below and answer, What these plots show different from the previous using mm
#' 1. Plot the variable `sunlight_times` in a bar graph
#'
DWD_data %>%
  mutate(prec_hour = as_factor(prec_hour) %>% 
           fct_recode(sunny = "0", raining = "1")) %>%
  ggplot(aes(x = factor(cloud_cover), fill = factor(prec_hour))) +
  geom_bar(position = "dodge")
#' 
DWD_data %>%
  mutate(prec_hour = as_factor(prec_hour) %>% 
           fct_recode(sunny = "0", raining = "1")) %>%
  ggplot(aes(x = prec_hour, fill = cloud_cover)) +
  geom_bar(position = "dodge")
#' 
#' 
