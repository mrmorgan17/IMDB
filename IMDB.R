# Necessary libraries
library(tidyverse)

# Read in data
train <- read_csv("IMDBTrain.csv")
test <- read_csv("IMDBTest.csv")

# Exploratory Data Analysis

# Scatterplot of Budget vs. Score
ggplot(data = train, mapping = aes(x = budget, y = imdb_score)) + geom_point()
# Budget is in local currency need to convert to a single currency

ggplot(data = train, mapping = aes(x = gross, y = imdb_score)) + geom_point()

with(train, cor(gross, imdb_score, use = 'complete.obs'))

train[is.na(train$director_facebook_likes),]$director_facebook_likes <- round(mean(train$director_facebook_likes, na.rm = TRUE), digits = 0)

# Putting in 0 liked for the movies that had no 3rd actor
train[is.na(train$actor_3_facebook_likes) & is.na(train$actor_3_name),]$actor_3_facebook_likes <- 0

train[is.na(train$actor_1_facebook_likes),]$actor_1_facebook_likes <- round(mean(train$actor_1_facebook_likes, na.rm = TRUE), digits = 0)

train[is.na(train$actor_2_name),]$actor_2_name <- "None"

summary(train %>% select(c(5, 6, 7, 8)))
