#'---
#'title: "Statistics Course - UWI"
#'author: "AlbyDR"
#'date: "`r format(Sys.Date())`"
#'always_allow_html: true
#'output: github_document
#'---

#'## Example 3.3:Inference and Modelling - 
#'
suppressPackageStartupMessages({
  library(tidyverse)     # ggplot, readr and dplyr packages
  library(lubridate)     # date-time
  library(GGally)        # label percentage 
  library(tidyquant) 
  library(vip) 
  library(moderndive)
  library(kableExtra)
  library(tidymodels) 
  library(bayesplot)
  library(rstanarm)
  library(bayestestR)
  library(insight)
  library(modelbased)
  library(performance)
  library(see)
  })  
#'
#'
DWD_temperature <- read_rds("DWD_temperature.rds")
DWD_precipitation <- read_rds("DWD_precipitation.rds")
#' 
#' 
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h, data = filter(DWD_precipitation, year(timestamp) >= 2020)) %>%
  pluck("fit") %>% summary()
#'
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h, data = filter(DWD_precipitation, year(timestamp) >= 2020)) %>%
  pluck("fit") %>% get_regression_table()
#'
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = filter(DWD_precipitation, year(timestamp) >= 2020)) %>%
  pluck("fit") %>% summary()

linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = filter(DWD_precipitation, year(timestamp) >= 2020)) %>%
  pluck("fit") %>% get_regression_table()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% summary()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_table() %>% kable()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% tidy()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% glance()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% vip()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 30)) %>%
  pluck("fit") %>% get_regression_summaries()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_summaries()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 500)) %>%
  pluck("fit") %>% get_regression_summaries()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 1000)) %>%
  pluck("fit") %>% get_regression_summaries()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 10000)) %>%
  pluck("fit") %>% get_regression_summaries()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 1000)) %>%
  pluck("fit") %>% summary()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ ., data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 1000)) %>%
  pluck("fit") %>% glance()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 1000)) %>%
  pluck("fit") %>% glance()

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_summaries()

# the test statistic and p-values are based on a t-distribution with degrees of freedom equal to df= n−p = 999-2=996.

set.seed(123)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_summaries()

set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_summaries()



set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_points()

set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% residuals()

set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + prec.window, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% fitted()


## interaction
set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp*precip_h, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_table()

# the “interaction effect” is significant while the precip_h effect is not. However you can not exclude the
# explanatory variable precip_h and keep the interaction, so both should be kept.

set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp*precip_h, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_table()

set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp*precip_h, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_summaries()

set.seed(999)
DWD_precipitation %>%
  filter(year(timestamp) >= 2020) %>%
  sample_n(100) %>%
  ggplot(aes(x = air_temp, y = rel_humidity, color = factor(precip_h))) +
  geom_point() +
  labs(x = "air temperature", y = "relative humidity", color = "precipitation") +
  geom_smooth(method = "lm", se = FALSE)

# There is an interaction effect if the associated effect of one variable depends on the value of 
# another variable. Here, the associated effect of the variable temperature depends on whether 
# is raining or not. The difference in slopes for precipitation shows this.

set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_table()

set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(rel_humidity ~ air_temp + precip_h, data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100)) %>%
  pluck("fit") %>% get_regression_summaries()

set.seed(999)
DWD_precipitation %>%
  filter(year(timestamp) >= 2020) %>%
  sample_n(100) %>%
  ggplot(aes(x = air_temp, y = rel_humidity, color = factor(precip_h))) +
  geom_point() +
  labs(x = "air temperature", y = "relative humidity", color = "precipitation") +
  geom_parallel_slopes(se = FALSE)


set.seed(999)
model <- stan_glm(rel_humidity ~ air_temp + precip_h, 
                  data=sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100))
print(model, digits = 3)
describe_posterior(model)

mcmc_dens(model, pars = c("air_temp"))+
  vline_at(-1.908  , col="red")

mcmc_dens(model, pars = c("precip_h"))+
  vline_at(14.9  , col="red")

rope(get_parameters(model)$air_temp)
# For air_temp almost all the credible interval (HDI) is outside the ROPE range, which means that 
# coefficient is highly significant.

model_performance(model)

estimate_means(model, levels = c("precip_h"), length = 2)

plot(rope(model))

# CI: Credible Interval, it is used to quantify the uncertainty about the regression coefficients.
# with 89% probability (given the data) that a coefficient lies above the CI_low value and under CI_high value
# This straightforward probabilistic interpretation is completely different from the confidence interval
# used in classical linear regression where the coefficient fall inside this confidence interval 
# (if we choose 95% of confidence) 95 times if we repeat the study 100 times.

# pd: Probability of Direction, which is the probability that the effect goes to the positive or 
# to the negative direction, and it is considered as the best equivalent for the p-value (p-value = 1-PD).

# ROPE_CI: Region of Practical Equivalence, since bayes method deals with true probabilities, 
# it does not make sense to compute the probability of getting the effect equals zero (the null hypothesis) 
# as a point (probability of a point in continuous intervals equal zero ). 
# Thus, we define instead a small range around zero which can be considered practically the same as 
# no effect (zero), this range therefore is called ROPE.
# By default (according to Cohen, 1988) The Rope is [-0.1,0.1] from the standardized coefficients.

# Rhat: scale reduction factor 𝑅̂ , it is computed for each scalar quantity of interest, as the 
# standard deviation of that quantity from all the chains included together, divided by the root 
# mean square of the separate within-chain standard deviations. When this value is close to 1 
# we do not have any convergence problem with MCMC.

# ESS: effective sample size, it captures how many independent draws contain the same amount of 
# information as the dependent sample obtained by the MCMC algorithm, the higher the ESS the better. 
# The threshold used in practice is 400.

# Logistic Model 
# using a binomial model (e.g., a logistic model), it is possible to reformulate the following hypothesis,
# “there is an important difference in this variable between the two groups” with the hypothesis 
# “this variable is able to discriminate between (or classify) the two groups (rainfall and dry)”
set.seed(999)
model_B <- stan_glm(precip_h ~ rel_humidity + air_temp, family = "binomial", refresh = 0,
                    data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100))
print(model_B, digits = 3)
describe_posterior(model_B)

rope(get_parameters(model_B)$rel_humidity)
rope(get_parameters(model_B)$air_temp)


model_performance(model_B)
estimate_means(model_B, levels = c("air_temp"), length = 2)
plot(rope(model_B))


# logistic
model.BF <- glm(precip_h ~ rel_humidity + air_temp, family = binomial, 
                data = sample_n(filter(DWD_precipitation, year(timestamp) >= 2020), 100))

model.BF
summary(model.BF)

# poisson
DWD_precipitation %>%
  filter(year(timestamp) >= 1900 & precip_h == 1) %>%
  group_by(year = year(timestamp)) %>%
  summarise(precip_hour_year = n(),
            rel_humidity = mean(rel_humidity, na.rm=T),
            air_temp = mean(air_temp, na.rm=T)) %>%
 glm(formula = precip_hour_year ~ rel_humidity + air_temp,
              family = poisson) %>%
  summary()


