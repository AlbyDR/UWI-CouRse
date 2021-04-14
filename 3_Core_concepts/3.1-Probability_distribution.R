#'---
#'title: "Statistics Course - UWI"
#'author: "AlbyDR"
#'date: "`r format(Sys.Date())`"
#'always_allow_html: true
#'output: github_document
#'---
#'
#'## Example 3.1: Probability Distribution  - 
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
#'### Probability Distributions
#' **Binomial**
rbinom(100000, size = 24, prob = 0.2) %>%
qplot(bins = 30, fill = I("lightblue"), colour = I("darkblue"))

#' probability to rain 0 and 5 times in a day
dbinom(0, size = 24, prob = .20)
dbinom(5, size = 24, prob = .20)
#' accumulated
pbinom(0:24, size = 24, prob = 0.2) %>%
  plot()
#'
#' **Geometric**
rgeom(100000, prob = 0.2) %>%
  qplot(bins = 120, fill = I("lightblue"), colour = I("darkblue"))
#' accumulated
pgeom(0:24, prob = 0.2) %>%
  plot()
#'
#' probability to rain 0 and 5 times in a day
dgeom(0, prob = .20)
dgeom(5, prob = .20)
dgeom(24, prob = .20)
#'
#' **Poisson**
rpois(100000, lambda  = 5) %>%
  qplot(bins = 120, fill = I("lightblue"), colour = I("darkblue"))
#'
#' accumulated
ppois(0:24, lambda  = 5) %>%
  plot()
#'
#' probability to rain 0 and 5 times in a day
dpois(0, lambda  = 5)
dpois(5, lambda  = 5)
dpois(24, lambda  = 5)
#'
#' **Exponential**
rexp(100000, rate  = 1/5) %>%
  qplot(bins = 80, fill = I("lightblue"), colour = I("darkblue"))
#'
#' accumulated
pexp(0:24, rate  = 1/5) %>%
  plot()
#'
#' probability to rain 0 and 5 times in a day
pexp(1, rate  = 1/5)
pexp(5, rate  = 1/5)
pexp(24, rate  = 1/5)
#'
#' **Normal**
rnorm(100000, mean  = 18, sd = 6) %>%
  qplot(bins = 80, fill = I("lightblue"), colour = I("darkblue")) +
  geom_vline(xintercept = 18, colour = "red", linetype = "dashed", size = 1.2)
#'
#' accumulated
pnorm(seq(-10,40,1), mean  = 18, sd = 6) %>%
  plot()
#'
#' probability to rain 0 and 5 times in a day
pnorm(0, mean  = 18, sd = 6)
pnorm(18, mean  = 18, sd = 6)
pnorm(30, mean  = 18, sd = 6, lower.tail = FALSE)
#'
#' Return dnorm(x) for 0 < x < 2, and NA for all other x
limitRange <- function(fun, mean, sd, min, max) {
  function(x) {
    y <- fun(x, mean, sd)
    y[x < min  |  x > max] <- NA
    return(y)
  }
}
#'
#' ggplot() with dummy data
ggplot(data.frame(x = c(-3, 3)), aes(x = x)) +
  stat_function(fun = limitRange(dnorm, 0, 1, -3, 3), geom = "area", fill = "blue", alpha = 0.2) +
  stat_function(fun = limitRange(dnorm, 0, 1, -2, 2), geom = "area", fill = "blue", alpha = 0.2) +
  stat_function(fun = limitRange(dnorm, 0, 1, -1, 1), geom = "area", fill = "blue", alpha = 0.2) +
  stat_function(fun = dnorm) +
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3))
#'
rnorm_plot <- data.frame("N01" = rnorm(1000))
#'
ggplot(rnorm_plot, aes(sample = N01)) +
  stat_qq(colour = "blue") + stat_qq_line() +
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3)) +
  scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3))
#'
1-2*pnorm(-1, 0, 1)
1-2*pnorm(-2, 0, 1)
1-2*pnorm(-3, 0, 1)
#'
descr(DWD_precipitation$air_temp, stats = "all")
#'
ggplot(DWD_precipitation, aes(x = air_temp)) +
  geom_density(size = I(1.5), colour = I("darkblue") ) +
  stat_function(fun = limitRange(dnorm, 10.16, 8.18, 10.16-8.18, 10.16+8.18),  geom = "area", fill = "blue", alpha = 0.1) +
  stat_function(fun = limitRange(dnorm, 10.16, 8.18, 10.16-2*8.18, 10.16+2*8.18),  geom = "area", fill = "blue", alpha = 0.1) +
  stat_function(fun = limitRange(dnorm, 10.16, 8.18, 10.16-3*8.18, 10.16+3*8.18),  geom = "area", fill = "blue", alpha = 0.1) +
  scale_x_continuous(breaks = seq(-20, 40, 10))
#'
ggplot(DWD_precipitation, aes(sample = air_temp)) +
  stat_qq(colour = "blue") + stat_qq_line() +
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3))
#'
1-2*pnorm(10.16-8.18, 10.16, 8.18)
#'
DWD_precipitation %>%
  filter(air_temp >= 10.16-8.18 & air_temp <= 10.16+8.18) %>%
  summarise(n = n())
#'
DWD_precipitation %>%
  filter(is.na(air_temp) != T) %>%
  summarise(n = n())
#'
156091/236566 # 0.66 vs 0.68
#'
#' probability to have a temperature below zero
pnorm(0, 10.16, 8.18, lower.tail = T)
pnorm(-10, 10.16, 8.18, lower.tail = T)
#'
#' Chi-squared Random numbers
rchisq(10000, df = 5) %>%
    qplot(bins = 80, fill = I("lightblue"), colour = I("darkblue") )
#'
#' t Random numbers
rt(10000, df = 5) %>%
    qplot(bins = 80, fill = I("lightblue"), colour = I("darkblue") )
#'
#' F Random numbers
rf(10000, df1 = 9999, df2 = 9999) %>%
    qplot(bins = 80, fill = I("lightblue"), colour = I("darkblue") )
#'
#' Cauchy Random numbers
rcauchy(10000, scale = 2) %>%
  qplot(bins = 50, fill = I("lightblue"), colour = I("darkblue") ) +
  scale_x_continuous(limits = c(-50,50))
#'
#' Generate Gumbel Random numbers
revd(10000, loc = 0, scale = 1, shape = 0) %>%
  qplot(bins = 80, fill = I("lightblue"), colour = I("darkblue") )
#'
#' Frechet distribution
revd(10000,loc= 0, scale = 1, shape = 0.2)  %>%
  qplot(bins = 80, fill = I("lightblue"), colour = I("darkblue") )
#'
#'# Generate Weibull Random numbers
revd(10000,loc= 0, scale = 1, shape = -0.6)  %>%
  qplot(bins = 80, fill = I("lightblue"), colour = I("darkblue") )
#'
#' Monte Carlo simulations - Normal distribution
set.seed(999)
simulated_temp <- tibble("temp" = rnorm(100, 10, 8))
#'
ggplot(simulated_temp) +
  geom_density(aes(x = temp), fill = "lightblue") +
  geom_vline(xintercept = 10, colour = "red", linetype = "dashed", size = 1.2) +
  scale_x_continuous(breaks = seq(-20, 40, 10))
#'
set.seed(999)
simulated_temp <- tibble("temp" = rnorm(100000, 10, 8))
#'
ggplot(simulated_temp) +
  geom_density(aes(x = temp), fill = "lightblue") +
  geom_vline(xintercept = 10, colour = "red", linetype = "dashed", size = 1.2) +
  scale_x_continuous(limits = c(-20 , 40), breaks = seq(-20, 40, 10))
#'
set.seed(999)
simulated_temp_1000 <- tibble("temp" = replicate(1000, {
  simulated_temp <- rnorm(100, 10, 8)
  mean(simulated_temp)
}))
#'
ggplot(simulated_temp_1000) +
  geom_density(aes(x = temp), fill = "lightblue") +
  geom_vline(xintercept = 10, colour = "red", linetype = "dashed", size = 1.2) +
  scale_x_continuous(limits = c(-20 , 40), breaks = seq(-20, 40, 10))
#'
descr(simulated_temp_1000$temp)
#' the mean is similar but the sd = SEM/sqrt(n))
8/sqrt(100)
#' the sd measure the dispersion around the mean of a data set, while Standard Error of a mean
#' it is measuring how much discrepancy the sample mean has in relation of the population mean.
#'
#'
DWD_precipitation %>%
  filter(year(timestamp)==2020) %>%
  ggplot() +
  geom_histogram(aes(x = precip_mm))
#'
DWD_precipitation %>%
  filter(year(timestamp)==2020) %>%
  ggplot(aes(sample = precip_mm)) +
  stat_qq() + stat_qq_line()
#'
set.seed(999)
simulated_recip_1000 <- tibble("mm" = replicate(100, {
  simulated_recip_temp <- sample_n(tibble("mm" = DWD_precipitation$precip_mm), 1000)
  mean(simulated_recip_temp$mm, na.rm=T)
}))
#'
ggplot(simulated_recip_1000) +
  geom_density(aes(x = mm), fill = "lightblue") #+
#'
simulated_recip_1000 %>%
  ggplot(aes(sample = mm)) +
  stat_qq() + stat_qq_line()
#'
