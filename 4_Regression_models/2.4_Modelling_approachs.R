#'---
#'title: "Statistics Course - UWI"
#'author: "AlbyDR"
#'date: "`r format(Sys.Date())`"
#'always_allow_html: true
#'output: github_document
#'---
#'
#'## Example: Modeling with R
#'
packages_list3 <- c("tidyverse", "lubridate", "GGally","colorspace","summarytools",
                    "tidymodels", "moderndive", "vip", "kableExtra", "bayesplot", 
                    "bayestestR",  "rstanarm", "insight", "modelbased",
                    "performance", "see", "car", "lmtest")
#'
#' new.packages <- packages_list3[!(packages_list3 %in% installed.packages()[,"Package"])]
#' if(length(new.packages)) install.packages(new.packages)
#' #'
#' update.packages <- packages_list3[(packages_list3 %in% old.packages()[,"Package"])]
#' if(length(update.packages)) install.packages(update.packages)
#'
invisible(lapply(packages_list3, library, character.only = T, quietly = TRUE, warn.conflicts = F))
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
  ggscatmat(columns = c("air_temp", "precip_mm", "rel_humidity"), 
            color = "day_night", alpha = 0.8)
#'
#' when have occurred precipitation in the hour
DWD_precipitation %>%
  filter(year(timestamp) >= 2020 & month(timestamp) == 8) %>%
  filter(precip_h == 1) %>%
  summarise(r = cor(air_temp, rel_humidity, 
                    use = 'pairwise.complete.obs')) %>% pull(r)
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
#' But how much air temperature explain RH (simple regression model)
#' Linear Regression Model Specification
#' 
#' **Simple Regressions** 
#' 
#' summary of training model - predictors and metrics
#' most common regression table results 
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp, # formula
      data = filter(DWD_precipitation, 
                    year(timestamp) >= 2020 & month(timestamp) == 8)) %>%
   pluck("fit") %>% summary()

#' regression_table focus only on the predictors and CI  
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp,
      data = filter(DWD_precipitation, 
                    year(timestamp) >= 2020 & month(timestamp) == 8)) %>%
  pluck("fit") %>% get_regression_table() 

#' focus only metrics
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp,
      data = filter(DWD_precipitation, 
                    year(timestamp) >= 2020 & month(timestamp) == 8)) %>%
  pluck("fit") %>%glance()

#' **Multiple Regressions**
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
#' only during the night
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
    fit(rel_humidity ~ air_temp + precip_h,
        data = filter(DWD_precipitation, 
                      year(timestamp) >= 2020 & month(timestamp) == 8 & sunlight_times == "Night")) %>%
    pluck("fit") %>% summary()
#' drop significantly
#' 
#' if we include a dummy variable day and night
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h + day_night, 
      data = filter(DWD_precipitation, year(timestamp) >= 2020 & month(timestamp) == 8)) %>%
  pluck("fit") %>% summary() 
#' It is like before. 
#'
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h + day_night,
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
#' **model prediction**
#' Let try to predict on April of the same year
predict(lm_RH_fit, new_data = filter(DWD_precipitation, year(timestamp) >= 2020 & month(timestamp) == 4)) %>%
bind_cols(filter(DWD_precipitation, year(timestamp) >= 2020 &
                   month(timestamp) == 4)) -> lm_RH_pred
#'
#' check the metrics of accuracy
metrics_reg <- metric_set(rsq, mpe, yardstick::rmse)
metrics_reg(lm_RH_pred, truth = rel_humidity, estimate = .pred) 
#' Seem that the Rsquared decrease, the RMSE increased 4x and 
#' the bias rocket to -100
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
#' **Model Selection**
#' many explanatory variables
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = filter(DWD_precipitation, year(timestamp) >= 2020)) %>%
  pluck("fit") %>% summary()
#' the factors was converted in dummy variables (0/1) according with the number of categories
#' the Rsquared is better, but in practice not all of this parameters are significant in practice 
#'
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = filter(DWD_precipitation, year(timestamp) >= 2020)) %>%
  pluck("fit") %>% get_regression_table() %>% print(n = 24)
#'
#' what happens if we take a sample before modeling
set.seed(123, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% summary()
#' The Rsquare is simmilar but now many predictors are not significant at all
#'
#' lets check some metrics
set.seed(123, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_summaries()
#' 
#' other metrics
set.seed(123, sample.kind = "Rounding") 
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% glance()
#' many different statistics
#'
#' Lets check the importance of the predictors
set.seed(123, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% vip()
#' timestamp? this kind of variable should not be in the model
#' 
set.seed(123, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + day_night + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% summary()
#' 
set.seed(123, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + day_night, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% summary()
#' 
#' 
set.seed(123, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_h + day_night, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% summary()
#' 
#' **Residual Analysis**
set.seed(123, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_h + day_night, 
      data = sample_n(filter(DWD_precipitation, 
                             year(timestamp) >= 2020), 100)) -> lm_fit
#'
ggplot(mapping = aes(x = lm_fit$fit$fitted.values, y = lm_fit$fit$model$rel_humidity)) +
  geom_point(color = '#006EA1') +
  geom_abline(intercept = 0, slope = 1, color = 'orange') +
  labs(title = 'Linear Regression Results - rel_humidity train Set',
       x = 'Predicted rel_humidity',
       y = 'Observed rel_humidity')
#'
set.seed(123, sample.kind = "Rounding")
new_data <- sample_n(filter(DWD_precipitation, year(timestamp) >= 2019), 100)
#' 
test_pred <- predict(lm_fit, new_data = new_data) %>% 
  bind_cols(new_data)
#' 
ggplot(data = test_pred,
       mapping = aes(x = .pred, y = rel_humidity)) +
  geom_point(color = '#006EA1') +
  geom_abline(intercept = 0, slope = 1, color = 'orange') +
  labs(title = 'Linear Regression Results - rel_humidity Test Set',
       x = 'Predicted rel_humidity',
       y = 'Observed rel_humidity')
#'
par(mfrow=c(2,2)) # plot all 4 plots in one
plot(lm_fit$fit, pch = 16, col = '#006EA1')
#' as the 100 sample is spread over the year the pattern are not clear
#' 
#' Autocorrelation
lm_fit %>% pluck("fit") %>% durbinWatsonTest() %>% glance()
#' or
lm_fit %>% pluck("fit") %>% pluck("residuals") %>% Box.test(type = "Ljung-Box") %>% glance()
#' apparently no temporal autocorrelation
#'
#' Multicollinearity
lm_fit %>%  pluck("fit") %>% vif()
#' apparently no Multicollinearity, 
#' VIF values that exceeds 5 or 10 indicates a problematic amount of collinearity.
#' 
#' Heterocedasticity
lm_fit %>%  pluck("fit") %>% bptest() %>% glance()
#' I would reject Heterocedasticity
#'
#' Normality test
lm_fit %>% pluck("fit") %>% pluck("residuals") %>% shapiro.test() %>% glance()
#' I would nor reject Normal
#' 
#' 
#' What's happens If we take the data in a sequence
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_h + day_night, 
      data = filter(DWD_precipitation, year(timestamp) >= 2020 & 
              month(timestamp) == 6 & hour(timestamp) == 12)) -> lm_fit_seq
#' 
par(mfrow=c(2,2)) # plot all 4 plots in one
#'
lm_fit_seq %>% pluck("fit") %>% plot(pch = 16, col = '#006EA1')
#' Autocorrelation
lm_fit_seq %>% pluck("fit") %>% durbinWatsonTest() %>% glance()
#' or
lm_fit_seq %>% pluck("fit") %>% pluck("residuals") %>% Box.test(type = "Ljung-Box") %>% glance()
#' temporal autocorrelation just pop-out
#'
#' Heterocedasticity
lm_fit_seq %>%  pluck("fit") %>% bptest() %>% glance()
#' We should reject Heterocedasticity
#'
#' Normality test
lm_fit_seq %>% pluck("fit") %>% pluck("residuals") %>% shapiro.test() %>% glance()
#' even Normality is rejected
#' 
#' **sample size effect**
set.seed(123, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_h + precip_mm + day_night  + cloud_cover, 
      data = sample_n(filter(DWD_precipitation, 
                             year(timestamp) >= 2020), 100)) -> lm_fit

lm_fit %>% pluck("fit") %>% glance()
#'
new_data %>%
  dplyr::select(rel_humidity) %>%
  bind_cols(predict(lm_fit, new_data = new_data)) %>% 
  metrics(truth = rel_humidity, estimate = .pred)
#'
#' n=30
set.seed(123, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_h + precip_mm + day_night  + cloud_cover,
      data = sample_n(filter(DWD_precipitation, 
                             year(timestamp) >= 2020), 30)) -> lm_fit30
#'
lm_fit30 %>%  pluck("fit") %>% glance()
#' r.squared is bigger, but the error sigma is higher. R-adjusted is far smaller
#' 
new_data30 <- sample_n(filter(DWD_precipitation, year(timestamp) >= 2019), 30) 
#' 
new_data30 %>%
  dplyr::select(rel_humidity) %>%
  bind_cols(predict(lm_fit30, new_data = new_data30)) %>% 
  metrics(truth = rel_humidity, estimate = .pred)
#' The difference between accuracy in the training and testing set is huge
#' 
set.seed(123, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_h + precip_mm + day_night  + cloud_cover,  
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 1000)) -> lm_fit1000
#'
lm_fit1000 %>%  pluck("fit") %>% glance()
#' r.squared much bigger, but the error sigma is higher
#' 
new_data1000 <- sample_n(filter(DWD_precipitation, year(timestamp) >= 2019), 1000) 
#' 
new_data1000 %>%
  dplyr::select(rel_humidity) %>%
  bind_cols(predict(lm_fit1000, new_data = new_data1000)) %>% metrics(truth = rel_humidity, estimate = .pred)
#' The difference between accuracy in the training and testing set are small 
#' 
#' the test statistic and p-values are based on a t-distribution with degrees of freedom 
#' equal to df= n−p = 1000-2 = 998.
#'
#'
set.seed(999, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_points()
#'
set.seed(999, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% fitted()
#'
set.seed(999, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times,  
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% residuals()
#'
#'
#' **Interaction**
set.seed(999, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp*precip_h, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_table()
#'
#'the “interaction effect” is significant while the precip_h effect is not. However you can not exclude the
#' explanatory variable precip_h and keep the interaction, so both should be kept.
#'
set.seed(999, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp*precip_h, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_summaries()
#'
set.seed(999, sample.kind = "Rounding")
DWD_precipitation %>%
  filter(year(timestamp) >= 2020) %>%
  sample_n(100) %>%
  ggplot(aes(x = air_temp, y = rel_humidity, color = factor(precip_h))) +
  geom_point() +
  labs(x = "air temperature", y = "relative humidity", color = "precipitation") +
  geom_smooth(method = "lm", se = FALSE)
#' There is an interaction effect if the associated effect of one variable depends on the value of 
#' another variable. Here, the associated effect of the variable temperature depends on whether 
#' is raining or not. The difference in slopes for precipitation shows this.
#'
set.seed(999, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_table()
#' the parameters for model without interaction is ver different
#'
set.seed(999, sample.kind = "Rounding")
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_summaries()
#'
set.seed(999, sample.kind = "Rounding")
DWD_precipitation %>%
  filter(year(timestamp) >= 2020) %>%
  sample_n(100) %>%
  ggplot(aes(x = air_temp, y = rel_humidity, color = factor(precip_h))) +
  geom_point() +
  labs(x = "air temperature", y = "relative humidity", color = "precipitation") +
  geom_parallel_slopes(se = FALSE)
#' without interaction the slopes are parallel
#' 
#' 
#' 
#' **Bayesian Framework**
set.seed(999, sample.kind = "Rounding")
model <- stan_glm(rel_humidity ~ air_temp*precip_h, 
                  data=sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100))
#'
print(model, digits = 3)
describe_posterior(model)
#'
mcmc_dens(model, pars = c("air_temp"))+
  vline_at(-1.62  , col="red")
#'
mcmc_dens(model, pars = c("precip_h"))+
  vline_at(6.49, col="red")
#'
rope(get_parameters(model)$air_temp)
# For air_temp almost all the credible interval (HDI) is outside the ROPE range, which means that 
# coefficient is highly significant.
#'
model_performance(model)
#'
estimate_means(model, levels = c("precip_h"), length = 2)
#'
plot(rope(model))
#' CI: Credible Interval, it is used to quantify the uncertainty about the regression coefficients.
#' with 89% probability (given the data) that a coefficient lies above the CI_low value and under CI_high value
#' This straightforward probabilistic interpretation is completely different from the confidence interval
#' used in classical linear regression where the coefficient fall inside this confidence interval 
#' (if we choose 95% of confidence) 95 times if we repeat the study 100 times.
#'
#' pd: Probability of Direction, which is the probability that the effect goes to the positive or 
#' to the negative direction, and it is considered as the best equivalent for the p-value (p-value = 1-PD).
#'
#' ROPE_CI: Region of Practical Equivalence, since bayes method deals with true probabilities, 
#' it does not make sense to compute the probability of getting the effect equals zero (the null hypothesis) 
#' as a point (probability of a point in continuous intervals equal zero ). 
#' Thus, we define instead a small range around zero which can be considered practically the same as 
#' no effect (zero), this range therefore is called ROPE.
#' By default (according to Cohen, 1988) The Rope is [-0.1,0.1] from the standardized coefficients.
#'
#' Rhat: scale reduction factor 𝑅̂ , it is computed for each scalar quantity of interest, as the 
#' standard deviation of that quantity from all the chains included together, divided by the root 
#' mean square of the separate within-chain standard deviations. When this value is close to 1 
#' we do not have any convergence problem with MCMC.
#'
# ESS: effective sample size, it captures how many independent draws contain the same amount of 
# information as the dependent sample obtained by the MCMC algorithm, the higher the ESS the better. 
# The threshold used in practice is 400.
#'
#' **Logistic Model**
#' using a binomial model (e.g., a logistic model), it is possible to reformulate the following hypothesis,
#' “there is an important difference in this variable between the two groups” with the hypothesis 
#' “this variable is able to discriminate between (or classify) the two groups (rainfall and dry)”
#' 
set.seed(999, sample.kind = "Rounding")
model_B <- stan_glm(precip_h ~ rel_humidity + air_temp, family = "binomial", refresh = 0,
                    data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100))
#'
print(model_B, digits = 3)
describe_posterior(model_B)
#'
rope(get_parameters(model_B)$rel_humidity)
rope(get_parameters(model_B)$air_temp)
#'
model_performance(model_B)
estimate_means(model_B, levels = c("air_temp"), length = 2)
plot(rope(model_B))
#'
#'
#' logistic
set.seed(999, sample.kind = "Rounding")
model.BF <- glm(precip_h ~ rel_humidity + air_temp, family = binomial, 
                data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100))
#'
model.BF
summary(model.BF)
#'
#' **Poisson**
DWD_precipitation %>%
  filter(year(timestamp) >= 1900 & precip_h == 1) %>%
  group_by(year = year(timestamp)) %>%
  summarise(precip_hour_year = n(),
            rel_humidity = mean(rel_humidity, na.rm=T),
            air_temp = mean(air_temp, na.rm=T)) %>%
  glm(formula = precip_hour_year ~ rel_humidity + air_temp,
      family = poisson) %>%
  summary()
#'
#' 
DWD_precipitation %>%
  filter(year(timestamp) >= 1900 & precip_h == 1) %>%
  group_by(year = year(timestamp)) %>%
  summarise(precip_hour_year = n(),
            rel_humidity = mean(rel_humidity, na.rm=T),
            air_temp = mean(air_temp, na.rm=T)) %>%
  glm(formula = precip_hour_year ~ rel_humidity + air_temp,
      family = poisson) %>%
  glance()

