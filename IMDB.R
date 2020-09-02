library(caret)
library(dplyr)

train <- read.csv("IMDBTrain.csv", header = TRUE)
test <- read.csv("IMDBTest.csv", header = TRUE)
sample <- read.csv("SampleSubmission.csv", header = TRUE)

train <- train %>% na.omit() %>% select(-movie_imdb_link)

fit <- randomForest::randomForest(train$imdb_score ~., data = train)

submission <- data.frame(Id = sample$Id, Predicted = predict(fit, test))

submission[is.na(submission$Predicted),]$Predicted <- mean(submission[!is.na(submission$Predicted),]$Predicted)

write.csv(submission, "rf.csv", row.names = FALSE)