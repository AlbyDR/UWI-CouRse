#'---
#'title: "Statistics Course - UWI"
#'author: "AlbyDR"
#'date: "`r format(Sys.Date())`"
#'always_allow_html: true
#'output: github_document
#'---
#'
#'## Example 2.4: Modelling 
#'
packages_list2.4 <- c("tidyverse", "lubridate", "colorspace", "GGally", "vip", "tidymodels")
#'
new.packages <- packages_list2.4[!(packages_list2.4 %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#'
update.packages <- packages_list2.4[(packages_list2.4 %in% old.packages()[,"Package"])]
if(length(update.packages)) install.packages(update.packages)
#'
invisible(lapply(packages_list2.4, library, character.only = T, quietly = TRUE, warn.conflicts = F))
#'
#'
DWD_temperature <- read_rds("DWD_temperature.rds")
DWD_precipitation <- read_rds("DWD_precipitation.rds")
#'
#DWD_precipitation$day_night <- if_else(DWD_precipitation$sunlight_times == "Night", 0, 1)
#'
#' correlation 
DWD_precipitation %>%
  filter(year(timestamp) >= 2020 & month(timestamp) == 8) %>%
  mutate(day_night = fct_collapse(sunlight_times,
                                  "Day" = unique(sunlight_times)[-1])) %>%
  ggscatmat(columns = c("air_temp", "precip_mm", "rel_humidity"), color = "day_night", alpha = 0.8)
#'
#' when have occurred precipitation in the hour
DWD_precipitation %>%
  filter(year(timestamp) >= 2020 & month(timestamp) == 8) %>%
  filter(precip_h == 1) %>%
  summarise(r = cor(air_temp, rel_humidity, use = 'pairwise.complete.obs')) %>% pull(r)
#'
#' when haven't occurred precipitation in the hour
DWD_precipitation %>%
  filter(year(timestamp) >= 2020 & month(timestamp) == 8) %>%
  filter(precip_h == 0) %>%
  summarise(r = cor(air_temp, rel_humidity, use = 'pairwise.complete.obs')) %>% pull(r)
#'
#'
DWD_precipitation %>%
  filter(year(timestamp) >= 2020 & month(timestamp) == 8) %>%
  ggplot(aes(x = rel_humidity, y = air_temp, colour = factor(precip_h))) + 
  geom_jitter(alpha = 0.05) +  
  stat_smooth(method = lm, level = 0.99) +
  scale_colour_manual(name = "precip_h" ,values = c("blue","brown")) +
  ggtitle("Air Temperature vs Relative Humidity") +
  labs(y='air temperature [ºC]',  x='relative humidity [%]') +
  annotate("text", x = 80, y = 33, colour = "brown", size = 6,
           label = "italic(r) [raining] == -0.69", parse = TRUE) +
  annotate("text", x = 80, y = 36, colour = "blue", size = 6,
           label = "italic(r) [dry] == -0.86", parse = TRUE) +
  theme()
#'
#' But how air temperature explain RH (simple regression model)
#' Linear Regression Model Specification
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  # formula and fit
  fit(rel_humidity ~ air_temp,
      data = filter(DWD_precipitation, 
                    year(timestamp) >= 2020 & month(timestamp) == 8)) %>%
  # summary of training model
  pluck("fit") %>% summary()
#'
#' including precip_h also as in the model (multiple regression)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
    fit(rel_humidity ~ air_temp + precip_h,
        data = filter(DWD_precipitation, 
                      year(timestamp) >= 2020 & month(timestamp) == 8)) %>%
    pluck("fit") %>% summary()
#' It have improved a bit.
#'  
#' only during the day
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
    fit(rel_humidity ~ air_temp + precip_h,
        data = filter(DWD_precipitation, 
                      year(timestamp) >= 2020 & month(timestamp) == 8 & sunlight_times != "Night")) %>%
    pluck("fit") %>% summary()
#'   
#'  
#' only during the night
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
    # formula and fit
    fit(rel_humidity ~ air_temp + precip_h,
        data = filter(DWD_precipitation, 
                      year(timestamp) >= 2020 & month(timestamp) == 8 & sunlight_times == "Night")) %>%
    # summary of training model
    pluck("fit") %>% summary()
#' drop significantly
#' 
#' if we include a dummy variable day and night
#'
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h + day_night, data = filter(DWD_precipitation, year(timestamp) >= 2020 & month(timestamp) == 8)) %>%
  pluck("fit") %>% #glance()
  summary()   
#' It is like before, the drop of Rsquared (coefficient of explanation) is probably because at night has less
#' variation to explain. 
#'
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h,
      data = filter(DWD_precipitation, year(timestamp) >= 2020 & month(timestamp) == 8)) -> lm_RH_fit
#'
#' Coefficients and accuracy (training)
tidy(lm_RH_fit)
glance(lm_RH_fit)
#'
#' residuals assessment
par(mfrow = c(2,2))
plot(lm_RH_fit$fit)
#'
#' variable importance
vip(lm_RH_fit)
#'
#' model prediction (predict on April of the same year)
predict(lm_RH_fit, new_data = filter(DWD_precipitation, year(timestamp) >= 2020 & month(timestamp) == 4)) %>%
bind_cols(filter(DWD_precipitation, year(timestamp) >= 2020 & month(timestamp) == 4)) -> lm_RH_pred
#'
#' check the metrics of accuracy
metrics_reg <- metric_set(rsq, rmse, mpe)
metrics_reg(lm_RH_pred, truth = rel_humidity, estimate = .pred) 
#' Seem that the Rsquared decrease, the RMSE increased 4x and the bias rocket to -102
#' 
#' lets confirm with a plot
ggplot(lm_RH_pred) +
  geom_point(aes(y = rel_humidity, x = .pred)) + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_continuous(limits = c(10,140), breaks = seq(10, 140, 20)) + 
  scale_y_continuous(limits = c(10,140), breaks = seq(10, 140, 20)) +
  annotate("rect", xmin = 100, xmax = 140, ymin = 10, ymax = 100,
           alpha = .1, fill = "red")
#' The model is very bias! It predict 'always' higher values than the observed.
#' 
#' Also, the model predict many values over 100%, with is unreasonable. But is its because a percentage,
#' should not be treated as a continuous variable
#' 
#' lets try to predict now the month = 8, but from the previous year 
predict(lm_RH_fit, new_data = filter(DWD_precipitation, year(timestamp) >= 2019 & month(timestamp) == 8)) %>%
  bind_cols(filter(DWD_precipitation, year(timestamp) >= 2019 & month(timestamp) == 8)) -> lm_RH_pred
#' 
metrics_reg(lm_RH_pred, truth = rel_humidity, estimate = .pred) 
#' The Rsquared dropped a bit, but the error and bias are very similar
#' 
ggplot(lm_RH_pred) +
  geom_point(aes(y = rel_humidity, x = .pred)) + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_continuous(limits = c(10,140), breaks = seq(10, 140, 20)) + 
  scale_y_continuous(limits = c(10,140), breaks = seq(10, 140, 20)) +
  annotate("rect", xmin = 100, xmax = 140, ymin = 10, ymax = 100,
           alpha = .1, fill = "red")
#' Much better!!! the bias is gone and only two values slightly higher than 100%
#' 
#' Apparently the model can be generalized to other year if in the same month. 
#' 
#' How is the model accuracy for the entire year? 
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h, data = filter(DWD_precipitation, year(timestamp) >= 2020)) %>%
  pluck("fit") %>% summary()
#' The accuracy is drop significantly
#'
#' Lets stratify by month
DWD_precipitation %>%
  filter(year(timestamp) >= 2020) %>%
  ggplot(aes(x = rel_humidity, y = air_temp, colour = month(timestamp, label = T))) + 
  geom_jitter(alpha = 0.05) +  
  stat_smooth(method = lm, level = 0.99) +
  labs(y='air temperature [ºC]',  x='relative humidity [%]') +
  theme()
#' The relationship look similar (negative / inversely proportional), but winter months the variation 
#' are smaller (short lines).
#' 
#' However, as the average temperature and relative humidity are quite different across the year. So
#' is a model for each month the solution? It will will be to laborious to have 12 models.
#' hierarchical 
#' In this case **Multilevel or Hierarchical Model** is probably the best solution because its allow the 
#' model to have a slope and/or an intercept for each model level (month in this case), but keeping only one #' model.
#' 
#' # Modeling approaches
#' 
#' **warning** notice that time is not consider in the explicit in the model, and if you repeat 
#' this same model using the timestamp by year, month or day, you may find a complete 
#' different relationship.
#' 
#' Despite time is a continuous variable, a timestamp is date-time index and not a random variables,
#' therefore I prefer consider time and space as a domain, rather a variable. 
#' Time would be a random variable if you consider the interval of time until a event occur,
#' for instance, the duration of a rainfall or of each phase of the phrenology of a species of plant.
#' 
#' As you often aggregate (discretization) time and space in intervals (hour/day/year or pixel/plot/region), and
#' we saw, for instance, that air temperature has seasonality (day and months) and trend (getting warm), 
#' the aggregation can change complete the interpretation. Also the way that you aggregate, for instance,
#' temperature and RH you may average, but precipitation you may sum or average.
#' 
#' Most of the regression do not 
#' handle well data that are temporal or spatial dependent as violet the 
#' assumption of independent and identical distributed (i.i.d) observations.
#' We will se an example later in the course.
#' 
#' 
#' 