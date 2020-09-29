library(tidyverse)
library(tidymodels)

imdb_data <- read_csv('CleanedIMDBData.csv')

train_ids <- which(imdb_data$Set == "train")

imdb_split <- initial_split(imdb_data)

# change split$in_id to include the predefined train samples
imdb_split$in_id <- train_ids

train_split <- training(imdb_split) %>% 
  select(-Set)

test_split <- testing(imdb_split) %>% 
  select(-Set)

preprocessing_recipe <- 
  recipes::recipe(imdb_score ~ ., data = training(imdb_split)) %>%
  # convert categorical variables to factors
  recipes::step_string2factor(all_nominal()) %>%
  # combine low frequency factor levels
  recipes::step_other(all_nominal(), threshold = 0.01) %>%
  # remove no variance predictors which provide no predictive information 
  recipes::step_nzv(all_nominal()) %>%
  prep()

imdb_cv_folds <- 
  recipes::bake(
    preprocessing_recipe, 
    new_data = training(imdb_split)
  ) %>%  
  rsample::vfold_cv(v = 5)

xgboost_model <- 
  parsnip::boost_tree(
    mode = "regression",
    trees = 1000,
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
  ) %>%
  set_engine("xgboost", objective = "reg:squarederror")

xgboost_params <- 
  dials::parameters(
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction()
  )

xgboost_grid <- 
  dials::grid_max_entropy(
    xgboost_params, 
    size = 60
  )

knitr::kable(head(xgboost_grid))

xgboost_wf <- 
  workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(imdb_score ~ .)

xgboost_tuned <- tune::tune_grid(
  object = xgboost_wf,
  resamples = imdb_cv_folds,
  grid = xgboost_grid,
  metrics = yardstick::metric_set(rmse, rsq, mae),
  control = tune::control_grid(verbose = TRUE)
)

xgboost_tuned %>%
  tune::show_best(metric = "rmse") %>%
  knitr::kable()

xgboost_best_params <- xgboost_tuned %>%
  tune::select_best("rmse")

knitr::kable(xgboost_best_params)

xgboost_model_final <- xgboost_model %>% 
  finalize_model(xgboost_best_params)

train_processed <- bake(preprocessing_recipe,  new_data = training(imdb_split))

train_prediction <- xgboost_model_final %>%
  # fit the model on all the training data
  fit(
    formula = imdb_score ~ ., 
    data    = train_processed
  ) %>%
  # predict the sale prices for the training data
  predict(new_data = train_processed) %>%
  bind_cols(training(imdb_split) %>% select(-Set))

xgboost_score_train <- 
  train_prediction %>%
  yardstick::metrics(imdb_score, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))

knitr::kable(xgboost_score_train)

test_processed  <- bake(preprocessing_recipe, new_data = testing(imdb_split))

test_prediction <- xgboost_model_final %>%
  # fit the model on all the training data
  fit(
    formula = imdb_score ~ ., 
    data    = train_processed
  ) %>%
  # use the training model fit to predict the test data
  predict(new_data = test_processed) %>%
  bind_cols(testing(imdb_split) %>% select(-Set))

# measure the accuracy of our model using `yardstick`
xgboost_score <- 
  test_prediction %>%
  yardstick::metrics(imdb_score, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))

knitr::kable(xgboost_score)

imdb_prediction_residual <- test_prediction %>%
  arrange(.pred) %>%
  mutate(residual_pct = (imdb_score - .pred) / .pred) %>%
  select(.pred, residual_pct)

ggplot(imdb_prediction_residual, aes(x = .pred, y = residual_pct)) +
  geom_point() +
  xlab("Predicted IMDB Score") +
  ylab("Residual (%)") +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent)
