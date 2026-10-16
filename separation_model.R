library(tidyverse)
library(xgboost)
library(tidymodels)
source("data_processing.R")

plays <- summarized_wr %>%
  initial_split(prop=0.8, strata = actual_separation)
set.seed(8)

plays_train <- training(plays)
plays_test <- testing(plays)
plays_folds <- vfold_cv(plays_train, v = 10)

xgboost_spec <- 
  boost_tree(trees = 200,
             min_n = tune(), tree_depth = tune(), 
             learn_rate = tune(), loss_reduction = tune(), 
             sample_size = tune()) %>%
  set_mode("regression") %>%
  set_engine("xgboost") 
#xgboost_spec

xgboost_recipe <- 
  recipe(formula = actual_separation ~  . , data = plays_train) %>%
  step_rm(game_id, play_id, nfl_id) %>%
  step_mutate(in_motion_at_ball_snap = as.integer(in_motion_at_ball_snap)) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors())  %>%
  step_corr(all_numeric_predictors(), threshold = 0.9)

xgboost_workflow <- 
  workflow() %>%
  add_recipe(xgboost_recipe) %>%
  add_model(xgboost_spec) 
#xgboost_workflow

set.seed(49)
xgboost_tune <- tune_grid(
  xgboost_workflow,
  resamples = plays_folds,
  grid = 3
)
show_best(xgboost_tune, metric = "rmse")

xgboost_best <- select_best(xgboost_tune, metric = "rmse")

final_xgboost_wkfl <- xgboost_workflow %>%
  finalize_workflow(xgboost_best)

set.seed(49)
full_folds <- vfold_cv(summarized_wr, v = 10, strata = actual_separation)

oof_res <- fit_resamples(
  final_xgboost_wkfl,
  resamples = full_folds,
  control = control_resamples(save_pred = TRUE)
)

oof_predictions <- collect_predictions(oof_res) %>%
  # .row lets you join back to combined_summary's row order
  arrange(.row) %>%
  bind_cols(summarized_wr %>% select(nfl_id, game_id, play_id))

xgboost_table <- oof_predictions %>%
  metrics(truth = actual_separation, estimate = .pred)

xgboost_table %>%
  knitr::kable()
