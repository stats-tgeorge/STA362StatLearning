library(visdat)

library(tidymodels)

library(ISLR)

data("Carseats")
str(Carseats)

set.seed(100)
sales_split <- initial_split(Carseats,.5)
sales_train <- training(sales_split)


rec <- recipe(formula= Sales ~.,data = Carseats)|>
  step_normalize(all_numeric_predictors())|>
  step_dummy(all_nominal_predictors())

wf <- workflow()|>
  add_recipe(rec)

set.seed(7)
sales_cv <- vfold_cv(sales_train,5)

bag_spec <- rand_forest(
  mode = "regression",
  trees = tune(),
  mtry = 10) |>
  set_engine("ranger")

wf<- wf |> add_model(bag_spec)  

my_grid <- expand.grid(trees = c(10,25,50,100,200,300))

set.seed(7)
bag_model <- tune_grid(wf,
                       grid = my_grid,
                       resamples  = sales_cv,
                       metrics = metric_set(rmse, rsq))

bag_metrics <- bag_model |> collect_metrics()

bag_metrics

ggplot(bag_metrics|>filter(.metric=="rmse"), aes(x = trees, y = mean)) + 
  geom_point() + 
  geom_line() + 
  labs(y = "RMSE")

rf_spec<- rand_forest(
  mode = "regression",
  trees = tune(),
  mtry = floor(sqrt(10))) |>
  set_engine("ranger")

wf<- wf |> update_model(rf_spec)

set.seed(7)
rf_model <- tune_grid(wf,
                       grid = my_grid,
                       resamples  = sales_cv,
                       metrics = metric_set(rmse, rsq))

rf_metrics <- rf_model |> collect_metrics()

rf_metrics

ggplot(rf_metrics|>filter(.metric=="rmse"), aes(x = trees, y = mean)) + 
  geom_point() + 
  geom_line() + 
  labs(y = "RMSE")


# boosted tree

bt_spec <- boost_tree(
  mode = "regression",
  tree_depth =1,
  trees = tune())|>
  set_engine('xgboost')

wf<- wf|>update_model(bt_spec)

set.seed(7)
boost_model <- tune_grid(wf,
                      grid = my_grid,
                      resamples  = sales_cv,
                      metrics = metric_set(rmse, rsq))

boost_metrics <- boost_model |> collect_metrics()

boost_metrics

ggplot(boost_metrics|>filter(.metric=="rmse"), aes(x = trees, y = mean)) +   geom_point() + 
  geom_line() + 
  labs(y = "RMSE")
