#'---
#'title: "Statistics Course - UWI"
#'author: "AlbyDR"
#'date: "`r format(Sys.Date())`"
#'always_allow_html: true
#'output: github_document
#'---
#'
#'## Example 4.2:Tuning Parameters
#'
suppressPackageStartupMessages({
  library(water)
  library(xts)
  library(lubridate)     # date-time
  library(GGally)        # label percentage 
  library(tidyquant) 
  library(vip) 
  library(summarytools)
  library(moderndive)
  library(kableExtra)
  library(tidyverse)
  library(rstanarm)
  library(bayestestR)
  library(insight)
  library(modelbased)
  library(performance)
  library(see)
  library(tidymodels)
  library(plsmod)
  library(mixOmics)
  library(randomForest)
  library(MASS)
  library(corrr)
  })  
#'
#'
DWD_dataset <- read_rds("DWD_dataset.rds")
DWD_precipitation <- read_rds("DWD_precipitation.rds")
#' 
ETmodel <- filter(DWD_dataset, year(timestamp) >= 2020)
#' 
ETmodel$ETo <- hourlyET(data.frame(wind = na.approx(ETmodel$wind_speed),
                                   RH = na.approx(ETmodel$RH), 
                                   temp = na.approx(ETmodel$air_temp),
                                   radiation = na.approx(ETmodel$Rin), 
                                   height = 81,  elev = 100,
                                   lat = 52.23, long = 13.04, tz = "UTC"), 
                        hours = hour(ymd_hms(ETmodel$timestamp, tz = "UTC")) , 
                        DOY = yday(ymd_hms(ETmodel$timestamp, tz = "UTC")), 
                        ET = "ETo")  
#' 
#' 
set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(ETo ~ rel_humidity + air_temp + wind_speed + Rin, data = ETmodel) %>%
  pluck("fit") %>% get_regression_summaries()
#' 
descr(ETmodel$ETo)
#' 
#' Lets include some measurements error
ETmodel$ET <- ETmodel$ETo + rnorm(17496, 0, 0.14/1.4)
#' 
set.seed(999)
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(ET ~ rel_humidity + air_temp + wind_speed + Rin, data = ETmodel) %>%
  pluck("fit") %>% get_regression_summaries()
#' 
set.seed(999)
ETdataS <- 
  ETmodel[,c(-20,-39)] %>%
  select_if(is.numeric) %>%
  drop_na() %>%
  sample_n(100)
#' 
summary(ETdataS)
#' 
linear_reg() %>% set_engine("lm") %>% set_mode("regression") %>%
  fit(ET ~ rel_humidity + air_temp + wind_speed + Rin, data = ETdataS) %>%
  pluck("fit") %>% get_regression_summaries()

#' **Tuning a PLSR**
#' 
set.seed(7777)
#'  split the data into traing (75%) and testing (25%)
ET_split <- initial_split(ETdataS, prop = 3/4)
ET_split
#' 
#'  extract training and testing sets
ET_train <- training(ET_split)
ET_test <- testing(ET_split)
#' 
#'  create CV object from training data
ET_cv <- vfold_cv(ET_train, repeats = 5)
#' 
#' define the recipe
ET_recipe <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(ET ~ ., data = ETdataS) %>%
  # and some pre-processing steps
  step_nzv(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>% 
  step_scale(all_numeric(), -all_outcomes())
#' 
#'  fix parameter
pls_spec4 <- plsmod::pls(num_comp = 4, predictor_prop = 1) %>% 
  set_engine("mixOmics") %>% 
  set_mode("regression") 
#' 
PLS_wf4 <- workflow() %>% 
  add_model(pls_spec4) %>% 
  add_recipe(ET_recipe) 
#' 
pls_fit4 <- PLS_wf4 %>% 
  fit(data = ET_train)
#' 
pls_fit4 %>% 
  pull_workflow_fit() %>% 
  tidy()
#' 
#'  training accuracy
ET_pred_train <- predict(pls_fit4, new_data = ET_train)
#' 
ET_pred_train <- 
  ET_train %>%
  dplyr::select(ET) %>%
  bind_cols(
    predict(pls_fit4, new_data = ET_train))
#' 
ET_pred_train %>% metrics(truth = ET, estimate = .pred) 
#' 
#'  testing accuracy
ET_pred_test <-  
  ET_test %>%
  dplyr::select(ET) %>%
  bind_cols(
    predict(pls_fit4, new_data = ET_test))
#' 
ET_pred_test %>% metrics(truth = ET, estimate = .pred)
#' 
#' **Tuning from a grid PLSR**
pls_spec_tune <- plsmod::pls(num_comp = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("mixOmics")
#' 
comp_grid <- tibble(num_comp = seq(from = 1, to = 20, by = 1))
#' 
ctrl <- control_resamples(save_pred = TRUE)
#' 
PLS_wf <- workflow() %>% 
  add_model(pls_spec_tune) %>% 
  add_recipe(ET_recipe) 
#' 
set.seed(456)
pls_fit <- 
  PLS_wf %>% 
  tune_grid(
    resamples = ET_cv,
    metrics = metric_set(rmse),
    #control = ctrl,
    grid = comp_grid)
#' 
pls_fit %>%
  collect_metrics() %>%
  #filter(.metric == "rmse") %>%
  mutate(num_comp = factor(num_comp)) %>%
  ggplot(aes(x = num_comp, y = mean)) +
    geom_line(size = 1.5, alpha = 0.6) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0.11, 0.15))
#' 
show_best(pls_fit, metric = "rmse")
#show_best(manual_tune, n = 3)
#' 
ET_final <- pls_fit %>%
  select_best(metric = "rmse")
#' 
ET_final
#' 
PLS_wf <- PLS_wf %>%
  finalize_workflow(ET_final)
#' 
PLS_wf_final <- PLS_wf %>%
  last_fit(ET_split) # fit on the training set and evaluate on test set
#' 
PLS_wf_final %>% collect_metrics()
#' 
PLS_wf_final %>%
  collect_predictions() %>% 
  ggplot(aes(x = ET, y = .pred)) + 
  geom_point(alpha = .15) +
  geom_abline(col = "red") + 
  coord_obs_pred() + 
  ylab("Predicted")
#' 
pls_fit_final <- PLS_wf %>% 
  fit(data = ET_train)
#' 
#' variable importance
pls_fit_final %>%
pull_workflow_fit() -> FItted_plsr
#' 
plotLoadings(pls_fit_final$fit$fit$fit, contrib="median", comp = 3)
#' 
plotVar(pls_fit_final$fit$fit$fit)
#' 
#' Imp_plsr <- tibble(var = rep(names(ETdataS[,-35]),3),
#'   loadings = c(data.frame(FItted_plsr$fit$loadings)$X.comp1,
#'                             data.frame(FItted_plsr$fit$loadings)$X.comp2,
#'                             data.frame(FItted_plsr$fit$loadings)$X.comp3),
#'   comp = c(rep(1,34),rep(2,34),rep(3,34)))
#' #' 
#' Imp_plsr %>%
#'   #mutate(var = fct_reorder(var, X1)) %>%
#'   ggplot(aes(y = loadings, x = var, fill = comp)) + 
#'   geom_col() +
#'   coord_flip() +
#'   xlab("") +
#'   theme_bw()
#' 
descr(ETdataS$Weather_changes_factor)
unique(ETdataS$Weather_changes_factor)
#'  weather_changes_factor ? This important? take care because this variable is
#'  not ordinal or continuous but there are many unique values.
#' 
#'  **Tuning from a grid RF**
rf_spec_tune <-  rand_forest(
  mode = "regression",
  mtry = tune(),
  trees = tune()) %>%
  set_engine("randomForest")
#' 
tree_grid <- expand_grid(mtry = c(2,4,8,12,16), trees = c(50, 100, 500))
#' 
ctrl <- control_resamples(save_pred = TRUE)
#' 
rf_wf <- workflow() %>%
  add_model(rf_spec_tune) %>% 
  add_recipe(ET_recipe) 
#' 
set.seed(456)
rf_fit <- 
  rf_wf %>% 
  tune_grid(
    resamples = ET_cv,
    metrics = metric_set(rmse),
    #control = ctrl,
    grid = tree_grid)
#' 
rf_fit %>% collect_metrics()
#' 
rf_fit %>%
  collect_metrics() %>%
  ggplot(aes(mtry, mean, color = factor(trees))) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) 
#' 
show_best(rf_fit, metric = "rmse")
#' 
rf_final <- rf_fit %>%
  select_best(metric = "rmse")
#' 
rf_wf <- rf_wf %>%
  finalize_workflow(rf_final)
#' 
rf_wf_final <- rf_wf %>%
  # fit on the training set and evaluate on test set
  last_fit(ET_split)
#' 
rf_wf_final %>% collect_metrics()
#' 
rf_wf_final %>% collect_predictions()
#' 
#'  fit the final model 
rf_specbest <- rand_forest(
    mode = "regression",
    mtry = 16,
    trees = 500) %>%
  set_engine("randomForest")
#' 
rf_best <- workflow() %>% 
  add_model(rf_specbest) %>% 
  add_recipe(ET_recipe)
#' 
rf_fitbest <- rf_best %>% 
  fit(data = ET_train)
#' 
rf_fitbest
#' 
#'  training accuracy
ET_train %>%
  dplyr::select(ET) %>%
  bind_cols(predict(rf_fitbest, new_data = ET_train)) %>% 
  metrics(truth = ET, estimate = .pred) 
#' 
#'  testing accuracy
ET_test %>%
  dplyr::select(ET) %>%
  bind_cols(predict(rf_fitbest, new_data = ET_test)) %>% 
  metrics(truth = ET, estimate = .pred)
#' 
##############################################################################
################ simulate an uncorrelated predictors #########################
##############################################################################
#'  data random generation independent of the response variable
#' 
ETdataS %>%
  select_if(is.numeric) %>%
  correlate(use = "pairwise.complete.obs", quiet = TRUE) %>% 
  focus(ET) %>%
  print(n = 35)
#' 
set.seed(31)
Mock_ETdataS <- tibble(data.frame(mvrnorm(n = length(ETdataS$ET), 
                                         colMeans(ETdataS[,-35], na.rm = FALSE, dims = 1),
                                         cov(ETdataS[,-35]) )))
#' 
Mock_ETdataS$ET <- ETdataS$ET
#' 
Mock_ETdataS %>%
  select_if(is.numeric) %>%
  correlate(use = "pairwise.complete.obs", quiet = TRUE) %>% 
  focus(ET) %>%
  print(n = 35)
#' 
set.seed(7777)
#'  split the data into training (75%) and testing (25%)
ET_splitS <- initial_split(Mock_ETdataS, prop = 3/4)
ET_trainS <- training(ET_splitS)
ET_testS <- testing(ET_splitS)
#' 
ET_recipeS <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(ET ~ ., data = Mock_ETdataS)
#' 
ET_cv <- vfold_cv(ET_train, v = 5, repeats = 1)
#' 
rf_wfS <- workflow() %>%
  add_model(rf_spec_tune) %>% 
  add_recipe(ET_recipeS) 
#' 
set.seed(456)
rf_fitS <- 
  rf_wfS %>% 
  tune_grid(
    resamples = ET_cv,
    grid = tree_grid)
#' 
rf_fitS %>% collect_metrics()
#' 
rf_finalS <- rf_fitS %>%
  select_best(metric = "rmse")
#' 
rf_fitS %>%
  collect_metrics() %>%
  filter(.metric=="rmse") %>%
  ggplot(aes(mtry, mean, color = factor(trees))) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) 
#' 
rf_wfS %>%
  finalize_workflow(rf_finalS) %>%
  last_fit(ET_split) %>% collect_metrics()
#' 
#'  fit the final model 
rf_specbest <- rand_forest(
  mode = "regression",
  mtry = 16,
  trees = 500) %>%
  set_engine("randomForest")
#' 
rf_bestS <- workflow() %>% 
  add_model(rf_specbest) %>% 
  add_recipe(ET_recipeS)
#' 
rf_fitbestS <- rf_bestS %>% 
  fit(data = ET_trainS)
#' 
rf_fitbestS
#' 
#'  training accuracy
ET_trainS %>%
  dplyr::select(ET) %>%
  bind_cols(predict(rf_fitbestS, new_data = ET_trainS)) %>% 
  metrics(truth = ET, estimate = .pred)
#' 
ET_trainS %>%
  dplyr::select(ET) %>%
  bind_cols(predict(rf_fitbestS, new_data = ET_trainS)) %>% 
  ggplot(aes(x = ET, y = .pred)) + 
  geom_point(alpha = .15) +
  geom_abline(col = "red") + 
  coord_obs_pred() + 
  ylab("Predicted")
#' 
#' training accuracy
ET_testS %>%
  dplyr::select(ET) %>%
  bind_cols(predict(rf_fitbestS, new_data = ET_testS)) %>% 
  metrics(truth = ET, estimate = .pred) 
#' 
ET_testS %>%
  dplyr::select(ET) %>%
  bind_cols(predict(rf_fitbestS, new_data = ET_testS)) %>% 
  ggplot(aes(x = ET, y = .pred)) + 
  geom_point(alpha = .15) +
  geom_abline(col = "red") + 
  coord_obs_pred() + 
  ylab("Predicted")
#' 
#' variable importance
VIP_rf <- pull_workflow_fit(rf_fitbestS)$fit
#' 
Imp_rf <- data.frame(unlist(VIP_rf$importance))
Imp_rf$var <- names(data.frame(t(VIP_rf$importance)))
#' 
Imp_rf %>%
  mutate(var = fct_reorder(var, IncNodePurity)) %>%
  ggplot(aes(y = IncNodePurity, x = var)) + 
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
#' 
#' 
correlate(ETdataS[,-35]) %>%
  print(n = 34)
#' 