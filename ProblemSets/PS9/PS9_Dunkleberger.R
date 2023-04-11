#!/usr/bin/env Rscript

install.packages("tidymodels")
install.packages("glmnet")
library(tidymodels)
library(glmnet)
library(tidyverse)
library(magrittr)

set.seed(123456)
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

head(housing_train)
head(housing)

housing_recipe <- recipe(medv ~ ., data = housing) %>%
  step_log(all_outcomes()) %>%
  step_bin2factor(chas) %>%
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>%
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b, lstat, dis, nox, degree = 6)

housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)

housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped  %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select( medv)
housing_test_y  <- housing_test_prepped  %>% select( medv)

tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")
# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)
# 10-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) %>%
  add_recipe(housing_recipe)
# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# Optimal Lambda
best_rmse <- select_best(rec_res, metric = "rmse")

final_lasso <- finalize_workflow(rec_wf, best_rmse)

# Print out results in test set
last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print



## Ridge regression
tune_spec_ridge <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 0       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")
# define a grid over which to try different values of lambda
lambda_grid_ridge <- grid_regular(penalty(), levels = 50)
# 10-fold cross-validation
rec_folds_ridge <- vfold_cv(housing_train_prepped, v = 6)

rec_wf_ridge <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec_ridge) %>%
  add_recipe(housing_recipe)
# Tuning results
rec_res_ridge <- rec_wf_ridge %>%
  tune_grid(
    resamples = rec_folds_ridge,
    grid = lambda_grid_ridge
  )

# Optimal Lambda
best_rmse_ridge <- select_best(rec_res_ridge, metric = "rmse")

final_ridge <- finalize_workflow(rec_wf_ridge, best_rmse_ridge)

# Print out results in test set
last_fit(final_ridge, split = housing_split) %>%
  collect_metrics() %>% print


