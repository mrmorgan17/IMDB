# Necessary libraries
library(tidyverse)

# Read in data
train <- read_csv("IMDBTrain.csv")
test <- read_csv("IMDBTest.csv")

# Merge the training and test data
names(test)[names(test) == 'Id'] <- 'Id'
imdb <- bind_rows(train = train, test = test, .id = 'Set')

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

(imdb %>% select(c(actor_1_name, actor_2_name, actor_3_name, actor_1_facebook_likes, actor_2_facebook_likes, actor_3_facebook_likes, cast_total_facebook_likes, movie_facebook_likes)))

# Combining and removing duplicates
top_actors <- c(data.frame(sort(with(imdb, table(actor_1_name)), decreasing = TRUE)) %>% pull(actor_1_name) %>% head(20) %>% as.character(), # Creating a list of top actor_1s based on the numbers of movies they were in (top 20)
                data.frame(sort(with(imdb, table(actor_2_name)), decreasing = TRUE)) %>% pull(actor_2_name) %>% head(20) %>% as.character(),
                data.frame(sort(with(imdb, table(actor_3_name)), decreasing = TRUE)) %>% pull(actor_3_name) %>% head(20) %>% as.character()
                ) %>% unique()

# Removing None
top_actors <- top_actors[top_actors != 'None']

# Initializing vectors
top_actor1 <- numeric()
top_actor2 <- numeric()
top_actor3 <- numeric()

for(i in 1:nrow(imdb)) {
  # Going through the actor_1_name column to see which of our previously identified top actors were in a movie as actor_1
  top_actor1[i] <- ifelse(imdb[i,]$actor_1_name %in% top_actors, 1, 0)
  # Going through the actor_2_name column to see which of our previously identified top actors were in a movie as actor_2
  top_actor2[i] <- ifelse(imdb[i,]$actor_2_name %in% top_actors, 1, 0)
  # Going through the actor_3_name column to see which of our previously identified top actors were in a movie as actor_3
  top_actor3[i] <- ifelse(imdb[i,]$actor_3_name %in% top_actors, 1, 0)
}

# Combining all the top_actors columns
num_top_actors <- apply(cbind(top_actor1, top_actor2, top_actor3), 1, sum)

##### Popularity #####
# Clean up columns first
for(i in 1:dim(imdb)[1]) {
  if(is.na(imdb[i, "actor_1_name"])) {imdb[i, "actor_1_name"] = "None"}
  if(is.na(imdb[i, "actor_2_name"])) {imdb[i, "actor_2_name"] = "None"}
  if(is.na(imdb[i, "actor_3_name"])) {imdb[i, "actor_3_name"] = "None"}
  if(is.na(imdb[i, "actor_1_facebook_likes"])) {imdb[i, "actor_1_facebook_likes"] = 0}
  if(is.na(imdb[i, "actor_2_facebook_likes"])) {imdb[i, "actor_2_facebook_likes"] = 0}
  if(is.na(imdb[i, "actor_3_facebook_likes"])) {imdb[i, "actor_3_facebook_likes"] = 0}
}

# For each Actor column, extract just the name and FB likes. Change the COL names so they can be combined.
actor1_likes <- imdb[, c("actor_1_facebook_likes", "actor_1_name")] %>% distinct_all() #1418 distinct actor 1s
colnames(actor1_likes) <- c("likes", "actor_name")
actor2_likes <- imdb[, c("actor_2_facebook_likes", "actor_2_name")] %>% distinct_all() #2108 distinct actor 2s
colnames(actor2_likes) <- c("likes", "actor_name")
actor3_likes <- imdb[, c("actor_3_facebook_likes", "actor_3_name")] %>% distinct_all() #2488 distinct actor 1s
colnames(actor3_likes) <- c("likes", "actor_name")

# Combine all three sets and remove duplicates. This 'popularity' set is all actors and their FB likes.
popularity <- bind_rows(actor1_likes, actor2_likes, actor3_likes) %>% distinct_all()

# Subset 'popularity' by x%. There are 4315 distinct actors in the data, so the top x% should be...
percent <- .01
popularity <- popularity[order(-popularity$likes), ]
popularity <- popularity[1:(ceiling(dim(popularity)[1]*percent)),]
pop_actors <- popularity$actor_name
# These names and likes correspond to cast total likes pretty well, so we could probably scrap that column

# Create an indicator variable for 'popular actor'
# for(i in 1:dim(imdb)[1]){
#  if(imdb[i, "actor_1_name"] %in% pop_actors | imdb[i, "actor_2_name"] %in% pop_actors |
#     imdb[i, "actor_3_name"] %in% pop_actors) {
#    imdb[i, "popular_actor"] <- 1
#  }
#  else {
#    imdb[i, "popular_actor"] <- 0
#  }
# }

pop_actor1 <- numeric()
pop_actor2 <- numeric()
pop_actor3 <- numeric()

for(i in 1:nrow(imdb)) {
  # Going through the actor_1_name column to see which of our previously identified top actors were in a movie as actor_1
  if (imdb[i,]$actor_1_name %in% pop_actors) {
    pop_actor1[i] <- 1
  } else {
    pop_actor1[i] <- 0
  }
  # Going through the actor_2_name column to see which of our previously identified top actors were in a movie as actor_2
  if (imdb[i,]$actor_2_name %in% pop_actors) {
    pop_actor2[i] <- 1
  } else {
    pop_actor2[i] <- 0
  }
  # Going through the actor_3_name column to see which of our previously identified top actors were in a movie as actor_3
  if (imdb[i,]$actor_3_name %in% pop_actors) {
    pop_actor3[i] <- 1
  } else {
    pop_actor3[i] <- 0
  }
}

# Combining all the pop_actors columns
pop_actors_df <- cbind(pop_actor1, pop_actor2, pop_actor3)

# Creating a new variable of for the total number of popular actors in a movie 
num_pop_actors <- apply(pop_actors_df, 1, sum)
