##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(Rcpp)) install.packages("Rcpp", repos = "http://cran.us.r-project.org")

# Loading libraries
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(tidyr)
library(knitr)
library(dplyr)
library(stringr)
library(readr)
library(gridExtra)
library(Rcpp)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########################################################
# Dataset Exploration 
##########################################################

# In depth summary of validation and edx dataset
summary(validation)
summary(edx)

# Total number of movies and users
edx %>% 
  summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

# Histogram of number of ratings/user
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 20, color = "red") +
  scale_x_log10() +
  ggtitle("Number of ratings/user") +  
  xlab("Number of ratings") + 
  ylab("Number of users")

# Histogram of number of ratings/movie
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 20, color = "blue") +
  scale_x_log10() +
  ggtitle("Number of ratings/movie") +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  
# Bar chart of number of ratings/genre
edx %>%
  separate_rows(genres, sep = "|") %>%
  group_by(genres) %>%
  summarize(rats = n()) %>%
  arrange(desc(rats)) %>%
  geom_bar(aes(fill = genres)) +
  ggplot(aes(genres, rats)) +
  ggtitle("Number of ratings/genre")

##########################################################
# Modeling Results
##########################################################

# RMSE function
RMSE <- function(realRating, predictedRating)
{
  sqrt(mean((realRating - predictedRating)^2))
}

# Analyzing mean movie rating model
edxMean <- mean(edx$rating)
rmse <- RMSE(validation$rating, edxMean)

rmseResults <- data_frame(method = "Mean Movie Rating Model", RMSE = rmse)
rmseResults

# Analyzing movie effect model
edxMean <- mean(edx$rating)

movie_mean <- edx %>%
  group_by(movieId) %>% 
  summarize(movieAvg = mean(rating - edxMean))
movie_mean

movie_mean %>% qplot(movieAvg, data = ., geom = "histogram", bins = 20, color = I("purple"))

predictedRating <- edxMean +
  validation %>%
  left_join(movie_mean, by = "movieId") %>% 
  pull(movieAvg)
  
effectModel <- RMSE(predictedRating, validation$rating)

rmseResults <- bind_rows(rmseResults, data_frame(method = "Movie Effect Model", RMSE = effectmodel))
rmseResults

# Analyzing user impact model
edx %>%
  group_by(userId) %>% 
  summarize(ratingMean = mean(rating)) %>% 
  filter(n() >= 100) %>%
  ggplot(aes(ratingMean)) + 
  geom_histogram(bins = 20, color = "green")

userAvg <- edx %>% 
  left_join(movie_mean, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(userMean = mean(rating - edxMean - movieAvg))
userAvg

userAvg %>% qplot(movieAvg, data = ., geom = "histogram", bins = 20, color = I("orange"))

predictedRating <- validation %>% 
  left_join(movie_mean, by = 'movieId') %>%
  left_join(userAvg, by = 'userId') %>%
  mutate(prediction = edxMean + userMean + movieAvg) %>%
  pull(prediction)

# Updated RMSE results
userRMSE <- RMSE(predictedRating, validation$rating)
rmseResults <- bind_rows(rmseResults, data_frame(method="Movie/User Impact Model", RMSE = userRMSE))
rmseResults

# Possible lambdas with movie/user bias
possibleLAMBDAS <- seq(0, 10, 0.25)
rmses <- sapply(possibleLAMBDAS, function(l){
  
edxMean <- mean(edx$rating)

movieAvg <- edx %>%
  group_by(movieId) %>%
  summarize(movieAvg = sum(rating - edxMean)/(n() + l))

userMean <- edx %>% 
  left_join(movieAvg, by = "movieId") %>%
  group_by(userId) %>%
  summarize(userMean = sum(rating - edxMean - movieAvg)/(n() + l))
  
predictedRating <- validation %>% 
  left_join(movieAvg, by = "movieId") %>%
  left_join(userMean, by = "userId") %>%
  mutate(prediction = edxMean + userMean + movieAvg) %>%
  pull(prediction)
  
return(RMSE(validation$rating, predictedRating))
})

qplot(possibleLAMBDAS, rmses)

##########################################################
# Conclusion
##########################################################

# Final lambda and RMSE results
finalLAMBDA <- possibleLAMBDAS[which.min(rmses)]
finalLAMBDA

rmseResults <- bind_rows(rmseResults, data_frame(method = "Movie/User Impact Model with Regularized Effects", RMSE = min(rmses)))
rmseResults