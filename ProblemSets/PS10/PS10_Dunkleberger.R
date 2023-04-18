install.packages("rpart")
install.packages("e1071")
install.packages("kknn")
install.packages("nnet")
install.packages("kernlab")
library(tidyverse)
library(tidymodels)
library(magrittr)
library(modelsummary)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(kernlab)

set.seed(100)

income <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", col_names = FALSE)
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

######################
# Clean up the data
######################
# Drop unnecessary columns
income %<>% select(-native.country, -fnlwgt, education.num)
# Make sure continuous variables are formatted as numeric
income %<>% mutate(across(c(age,hours,education.num,capital.gain,capital.loss), as.numeric))
# Make sure discrete variables are formatted as factors
income %<>% mutate(across(c(high.earner,education,marital.status,race,workclass,occupation,relationship,sex), as.factor))
# Combine levels of factor variables that currently have too many levels
income %<>% mutate(education = fct_collapse(education,
                                            Advanced    = c("Masters","Doctorate","Prof-school"), 
                                            Bachelors   = c("Bachelors"), 
                                            SomeCollege = c("Some-college","Assoc-acdm","Assoc-voc"),
                                            HSgrad      = c("HS-grad","12th"),
                                            HSdrop      = c("11th","9th","7th-8th","1st-4th","10th","5th-6th","Preschool") 
),
marital.status = fct_collapse(marital.status,
                              Married      = c("Married-civ-spouse","Married-spouse-absent","Married-AF-spouse"), 
                              Divorced     = c("Divorced","Separated"), 
                              Widowed      = c("Widowed"), 
                              NeverMarried = c("Never-married")
), 
race = fct_collapse(race,
                    White = c("White"), 
                    Black = c("Black"), 
                    Asian = c("Asian-Pac-Islander"), 
                    Other = c("Other","Amer-Indian-Eskimo")
), 
workclass = fct_collapse(workclass,
                         Private = c("Private"), 
                         SelfEmp = c("Self-emp-not-inc","Self-emp-inc"), 
                         Gov     = c("Federal-gov","Local-gov","State-gov"), 
                         Other   = c("Without-pay","Never-worked","?")
), 
occupation = fct_collapse(occupation,
                          BlueCollar  = c("?","Craft-repair","Farming-fishing","Handlers-cleaners","Machine-op-inspct","Transport-moving"), 
                          WhiteCollar = c("Adm-clerical","Exec-managerial","Prof-specialty","Sales","Tech-support"), 
                          Services    = c("Armed-Forces","Other-service","Priv-house-serv","Protective-serv")
)
)


######################
# tidymodels time!
######################
income_split <- initial_split(income, prop = 0.8)
income_train <- training(income_split)
income_test  <- testing(income_split)




#####################
# logistic regression
#####################
print('Starting LOGIT')
# set up the task and the engine
tune_logit_spec <- logistic_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("classification")

# define a grid over which to try different values of the regularization parameter lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 3-fold cross-validation
rec_folds <- vfold_cv(income_train, v = 3)

# Workflow
rec_wf <- workflow() %>%
  add_model(tune_logit_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# what is the best value of lambda?
top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_logit_lasso <- finalize_workflow(rec_wf,
                                       best_acc
)
print('*********** LOGISTIC REGRESSION **************')
logit_test <- last_fit(final_logit_lasso,income_split) %>%
  collect_metrics()

logit_test %>% print(n = 1)
top_acc %>% print(n = 1)

# combine results into a nice tibble (for later use)
logit_ans <- top_acc %>% slice(1)
logit_ans %<>% left_join(logit_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "logit") %>% select(-starts_with(".config"))


#####################
# tree model
#####################
print('Starting TREE')
# set up the task and the engine
tune_tree_spec <- decision_tree(
  min_n = tune(), # tuning parameter
  tree_depth = tune(), # tuning parameter
  cost_complexity = tune(), # tuning parameter
) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (complexity, depth, etc.)
tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
tree_parm_df2 <- tibble(min_n = seq(10,100,by=10))
tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))
tree_parm_df  <- full_join(tree_parm_df1,tree_parm_df2,by=character()) %>% full_join(.,tree_parm_df3,by=character())

tree_ctrl <- vfold_cv(income_train, v = 3)

rec_wf_tree <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

rec_res_tree <- rec_wf_tree %>%
  tune_grid(
    resamples = tree_ctrl,
    grid = tree_parm_df
  )

top_acc_tree  <- show_best(rec_res_tree, metric = "accuracy")
best_acc_tree <- select_best(rec_res_tree, metric = "accuracy")
final_tree <- finalize_workflow(rec_wf_tree,
                                       best_acc_tree
)

print('*********** TREE MODEL **************')
tree_test <- last_fit(final_tree,income_split) %>%
  collect_metrics()

tree_test %>% print(n = 1)
top_acc_tree %>% print(n = 1)

# combine results into a nice tibble (for later use)
tree_ans <- top_acc_tree %>% slice(1)
tree_ans %<>% left_join(tree_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "Tree Model") %>% select(-starts_with(".config"))

#####################
# neural net
#####################
print('Starting NNET')
# set up the task and the engine
tune_nnet_spec <- mlp(
  hidden_units = tune(), # tuning parameter
  penalty = tune()
) %>% 
  set_engine("nnet") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
nnet_parm_df1 <- tibble(hidden_units = seq(1,10))
lambda_grid   <- grid_regular(penalty(), levels = 10)
nnet_parm_df  <- full_join(nnet_parm_df1,lambda_grid,by=character())

nnet_ctrl <- vfold_cv(income_train, v = 3)

rec_wf_nnet <- workflow() %>%
  add_model(tune_nnet_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

rec_res_nnet <- rec_wf_nnet %>%
  tune_grid(
    resamples = nnet_ctrl,
    grid = nnet_parm_df
  )

top_acc_nnet  <- show_best(rec_res_nnet, metric = "accuracy")
best_acc_nnet <- select_best(rec_res_nnet, metric = "accuracy")
final_nnet <- finalize_workflow(rec_wf_nnet,
                                best_acc_nnet
)

print('*********** NEURAL NET **************')
nnet_test <- last_fit(final_nnet,income_split) %>%
  collect_metrics()

nnet_test %>% print(n = 1)
top_acc_nnet %>% print(n = 1)

# combine results into a nice tibble (for later use)
nnet_ans <- top_acc_nnet %>% slice(1)
nnet_ans %<>% left_join(nnet_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "Neural Net") %>% select(-starts_with(".config"))


#####################
# knn
#####################
print('Starting KNN')
# set up the task and the engine
tune_knn_spec <- nearest_neighbor(
  neighbors = tune() # tuning parameter
) %>% 
  set_engine("kknn") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
knn_parm_df <- tibble(neighbors = seq(1,30))

knn_ctrl <- vfold_cv(income_train, v = 3)

rec_wf_knn <- workflow() %>%
  add_model(tune_knn_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

rec_res_knn <- rec_wf_knn %>%
  tune_grid(
    resamples = knn_ctrl,
    grid = knn_parm_df
  )

top_acc_knn  <- show_best(rec_res_knn, metric = "accuracy")
best_acc_knn <- select_best(rec_res_knn, metric = "accuracy")
final_knn <- finalize_workflow(rec_wf_knn,
                                best_acc_knn
)

print('*********** KNN **************')
knn_test <- last_fit(final_knn,income_split) %>%
  collect_metrics()

knn_test %>% print(n = 1)
top_acc_knn %>% print(n = 1)

# combine results into a nice tibble (for later use)
knn_ans <- top_acc_nnet %>% slice(1)
knn_ans %<>% left_join(knn_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "KNN") %>% select(-starts_with(".config"))


#####################
# SVM
#####################
print('Starting SVM')
# set up the task and the engine
tune_svm_spec <- svm_rbf(
  cost = tune(), 
  rbf_sigma = tune()
) %>% 
  set_engine("kernlab") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
svm_parm_df1 <- tibble(cost      = c(2^(-2),2^(-1),2^0,2^1,2^2,2^10))
svm_parm_df2 <- tibble(rbf_sigma = c(2^(-2),2^(-1),2^0,2^1,2^2,2^10))
svm_parm_df  <- full_join(svm_parm_df1,svm_parm_df2,by=character())

# YOU FILL IN THE REST
svm_ctrl <- vfold_cv(income_train, v = 3)

rec_wf_svm <- workflow() %>%
  add_model(tune_svm_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

rec_res_svm <- rec_wf_svm %>%
  tune_grid(
    resamples = svm_ctrl,
    grid = svm_parm_df
  )

top_acc_svm  <- show_best(rec_res_svm, metric = "accuracy")
best_acc_svm <- select_best(rec_res_svm, metric = "accuracy")
final_svm <- finalize_workflow(rec_wf_svm,
                               best_acc_svm
)

print('*********** SVM **************')
svm_test <- last_fit(final_svm,income_split) %>%
  collect_metrics()

svm_test %>% print(n = 1)
top_acc_svm %>% print(n = 1)

# combine results into a nice tibble (for later use)
svm_ans <- top_acc_nnet %>% slice(1)
svm_ans %<>% left_join(svm_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "SVM") %>% select(-starts_with(".config"))


#####################
# combine answers
#####################
all_ans <- bind_rows(logit_ans,tree_ans,nnet_ans,knn_ans,svm_ans)
datasummary_df(all_ans %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="markdown") %>% print
