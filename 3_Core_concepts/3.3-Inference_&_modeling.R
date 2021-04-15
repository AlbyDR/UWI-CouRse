#'---
#'title: "Statistics Course - UWI"
#'author: "AlbyDR"
#'date: "`r format(Sys.Date())`"
#'always_allow_html: true
#'output: github_document
#'---
#'
#'## Example 3.3:Inference and Modeling - 
#'
#'
packages_list3.3 <- c("tidyverse", "lubridate", "GGally", "summarytools", "tidymodels",
                    "moderndive", "vip", "kableExtra", "bayesplot", "bayestestR", 
                    "rstanarm", "insight", "modelbased", "performance", "see")
#'
new.packages <- packages_list3.3[!(packages_list3.3 %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#'
update.packages <- packages_list2[(packages_list2 %in% old.packages()[,"Package"])]
if(length(update.packages)) install.packages(update.packages)
#'
invisible(lapply(packages_list3.3, library, character.only = T, quietly = TRUE, warn.conflicts = F))
#'
#'
DWD_temperature <- read_rds("DWD_temperature.rds")
DWD_precipitation <- read_rds("DWD_precipitation.rds")
#' 
#' **Simple linear regression**
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp, data = filter(DWD_precipitation, year(timestamp) >= 2020)) %>%
  pluck("fit") %>% summary()
#' 
#' **Multiple linear regression**
#' two explanatory variables (predictors, independent variables, model inputs)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h, data = filter(DWD_precipitation, year(timestamp) >= 2020)) %>%
  pluck("fit") %>% summary()
#' very low p-values as the sample size is very big.
#'
#' CI for the estimations
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h, data = filter(DWD_precipitation, year(timestamp) >= 2020)) %>%
  pluck("fit") %>% get_regression_table()
#' very narrow CI as the sample size is very big.
#'
#' many explanatory variables
#' 
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
set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% summary()
#' The Rsquare is simmilar but now many predictors are not significant at all
#'
#' lets check some metrics
set.seed(123) 
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_summaries()
#' other metrics
set.seed(123) 
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% glance()
#' many different statistics
#'
#' Lets check the importance of the predictors
set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% vip()
#' timestamp? this kind of variable should be not in the model
#' 
#' 
set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% summary()
#' 
set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% glance()
#' 
#' Residual analysis
set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) -> lm_fit

ggplot(mapping = aes(x = lm_fit$fit$fitted.values, y = lm_fit$fit$model$rel_humidity)) +
  geom_point(color = '#006EA1') +
  geom_abline(intercept = 0, slope = 1, color = 'orange') +
  labs(title = 'Linear Regression Results - rel_humidity train Set',
       x = 'Predicted rel_humidity',
       y = 'Observed rel_humidity')

set.seed(123)
new_data <- sample_n(filter(DWD_precipitation, year(timestamp) >= 2019), 100)

test_pred <- predict(lm_fit, new_data = new_data) %>% 
  bind_cols(new_data)

ggplot(data = test_pred,
       mapping = aes(x = .pred, y = rel_humidity)) +
  geom_point(color = '#006EA1') +
  geom_abline(intercept = 0, slope = 1, color = 'orange') +
  labs(title = 'Linear Regression Results - rel_humidity Test Set',
       x = 'Predicted rel_humidity',
       y = 'Observed rel_humidity')
#'
par(mfrow=c(2,2)) # plot all 4 plots in one
#'
plot(lm_fit$fit, 
     pch = 16,    
     col = '#006EA1')

linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = filter(DWD_precipitation, year(timestamp) >= 2020 & month(timestamp) == 6)) %>%
  pluck("fit") -> lm_fit

par(mfrow=c(2,2)) # plot all 4 plots in one
#'
plot(lm_fit, 
     pch = 16,    
     col = '#006EA1')

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") -> lm_fit

#' normality test
shapiro.test(lm_fit$residuals)
#' Autocorrelation
dwtest(lm_fit)
Box.test(lm_fit$residuals, type = "Ljung-Box")
#' Heterocedasticity
library(lmtest)
bptest(lm_fit)
#'Multicollinearity
library(car)
vif(lm_fit)
#' VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity.
#' 
#' 
set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 30)) %>%
  pluck("fit") %>% glance()
#' 
#' 
set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 500)) %>%
  pluck("fit") %>% glance()
#' 
#'
set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 1000)) %>%
  pluck("fit") %>% glance()
#' 
#'
set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 10000)) %>%
  pluck("fit") %>% glance()
#' the test statistic and p-values are based on a t-distribution with degrees of freedom equal to df= n−p = 999-2=996.
#'
#'
set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_points()
#'
set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% fitted()
#'
set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window + precip_mm + precip_h + sunlight_times,  
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% residuals()
#'
#'
#' **Interaction**
set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp*precip_h, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_table()
#'
#'the “interaction effect” is significant while the precip_h effect is not. However you can not exclude the
#' explanatory variable precip_h and keep the interaction, so both should be kept.
#'
set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp*precip_h, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_summaries()
#'
set.seed(999)
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
set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h, 
      data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_table()
#' the parameters for model without interaction is ver different
#'
set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_summaries()
#'
set.seed(999)
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
set.seed(999)
model <- stan_glm(rel_humidity ~ air_temp*precip_h, 
                  data=sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100))
#'
print(model, digits = 3)
describe_posterior(model)
#'
mcmc_dens(model, pars = c("air_temp"))+
  vline_at(-2.045  , col="red")
#'
mcmc_dens(model, pars = c("precip_h"))+
  vline_at(3.475, col="red")
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
set.seed(999)
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
set.seed(999)
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
