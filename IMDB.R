# Necessary libraries
library(tidyverse)
library(caret)
library(xgboost)

imdb.clean <- read_csv('CleanedIMDBData.csv') %>% mutate_at(vars(movie_title, language, content_rating), factor)

# IVTrans <- dummyVars(imdb_score ~ . -movie_title -Set, data = imdb.clean)
# imdb.iv <- predict(IVTrans, newdata = imdb.clean) %>% as.data.frame() %>% bind_cols(., imdb.clean %>% select(movie_title, Set, imdb_score))

# pcTrans <- preProcess(x = imdb.clean %>% select(-imdb_score), method = 'pca')
# imdb.pca <- predict(pcTrans, newdata = imdb.clean)
# plot_correlation(imdb.pca, type = 'continuous', cor_args = list(use = 'pairwise.'))

# Center and Scaling
# trans.cs <- preProcess(x = imdb %>% select(-imdb_score), method = c('center', 'scale'))
# imdb.cs <- predict(trans.cs, newdata = imdb)

# Use one or the other

# trans.01 <- preProcess(x = imdb.clean %>% select(-imdb_score), method = 'range', rangeBounds = c(0, 1))
# imdb.01 <- predict(trans.01, newdata = imdb.clean)

imdb.train <- imdb.clean %>% filter(Set == 'train') %>% select(-c(Set, movie_title))
imdb.test <- imdb.clean %>% filter(Set == 'test') %>% select(-Set)

###########
## caret ##
###########

xgbTree.model <- train(imdb_score ~ ., 
                       data = imdb.train, # Using the training set to create the model
                       method = 'xgbTree', # Defining the model to be k-Nearest Neighbors
                       trControl = trainControl(method = "cv", number = 10), # Defining the resampling procedure that will be used for the model
                       preProcess = c('center', 'scale', 'zv'),
                       tuneGrid = expand.grid(nrounds = 500, #115
                                              max_depth = 6, #3
                                              eta = .025, #.195
                                              gamma = 0,
                                              colsample_bytree = .58, #.58
                                              min_child_weight = 1,
                                              subsample = .6666667 #.675
                                              ),
                       maximize = FALSE # Ensuring that we minimize RMSE
                       )

xgbTree.model

caret.submission <- data.frame(Id = imdb.test %>% pull(movie_title), Predicted = predict(xgbTree.model, imdb.test))
write.csv(caret.submission, "caret-preds.csv", row.names = FALSE)

xgbLinear.model <- train(imdb_score ~ ., 
                         data = imdb.train, # Using the training set to create the model
                         method = 'xgbLinear', # Defining the model to be k-Nearest Neighbors
                         trControl = trainControl(method = "cv", number = 10), # Defining the resampling procedure that will be used for the model
                         tuneGrid = expand.grid(lambda = c(.1, .01, .001),
                                                alpha = c(.003, .05, .0001),
                                                nrounds = 500,
                                                eta = .025),
                         maximize = FALSE # Ensuring that we minimize RMSE
)
xgbLinear.model

caret.submission <- data.frame(Id = imdb.test %>% pull(movie_title), Predicted = predict(xgbTree.model, imdb.test))
write.csv(caret.submission, "caret-preds.csv", row.names = FALSE)

# The train and test data must be the exact same in terms of number of columns and order

#############
## xgboost ##
#############

# Isolating the 'Id' column for the submission file
test.id <- imdb.test %>% select(movie_title) 

# Removing the column
imdb.test$movie_title <- NULL

# Isolating the response variable
train.y <- imdb.train$imdb_score

# Removing the response variable column
imdb.train$imdb_score <- NULL
imdb.test$imdb_score <- NULL

# Creating data.matrix
trainM <- data.matrix(imdb.train, rownames.force = NA)

# Creating DMarix for xgboost 
dtrain <- xgb.DMatrix(data = trainM, label = train.y, missing = NaN)

watchlist <- list(trainM = dtrain)

param <- list(objective = "reg:squarederror", 
              booster = "gbtree",
              eval_metric = "rmse",
              eta = .025,
              max_depth = 6,
              subsample = .66667,
              colsample_bytree = .55
              )

clf <- xgb.cv(params = param, 
              data = dtrain, 
              nrounds = 1000,
              nfold = 15,
              watchlist = watchlist,
              verbose = 1,
              print_every_n = 10,
              early_stopping_rounds = 20,
              maximize = FALSE
              )

xgb.model <- xgb.train(params = param, 
                       data = dtrain, 
                       nrounds = bestRound,
                       watchlist = watchlist,
                       verbose = 1,
                       maximize = FALSE
                       )

testM <- data.matrix(imdb.test, rownames.force = NA)
preds <- predict(xgb.model, testM)
xgboost.submission <- data.frame(Id = test.id, Predicted = preds)
names(xgboost.submission)[1] <- 'Id'
write.csv(xgboost.submission, "xgboost-preds.csv", row.names = FALSE)

usemodels::use_xgboost(imdb_score ~., data = imdb.train, prefix = 'impairment')

