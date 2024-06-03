# Data Extraction, Data Preparation and Data Exploration
# Load Required Packages & Libraries
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(knitr)
library(ggthemes)

# Data Extraction
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 500)

dl <- "ml-10M100K.zip"
if(!file.exists(dl)) download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file)) unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file)) unzip(dl, movies_file)

# Data Preparation
ratings <- as.data.frame(str_split(read_lines(ratings_file)
           , fixed("::"), simplify = TRUE)
           , stringsAsFactors = FALSE)

colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")

ratings <- ratings %>% 
           mutate(userId = as.integer(userId)
           , movieId = as.integer(movieId)
           , rating = as.numeric(rating)
           , timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file)
          , fixed("::"), simplify = TRUE)
          , stringsAsFactors = FALSE)

colnames(movies) <- c("movieId", "title", "genres")

movies <- movies %>% 
          mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")


# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 

test_index <- createDataPartition(y = movielens$rating
              , times = 1, p = 0.1, list = FALSE)

edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
                      semi_join(edx, by = "movieId") %>% 
                      semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Test set will be 20% of the edx set
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(edx$rating
              , times = 1, p = 0.2, list = FALSE)

train_edx <- edx[-test_index,]
test_edx_1 <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_edx <- test_edx_1 %>%
            semi_join(train_edx, by="userId") %>%
            semi_join(train_edx, by="movieId")

# Add rows removed from test set back into train set
removed_data <- anti_join(test_edx_1,test_edx)
train_edx <- rbind(train_edx,removed_data)

rm(removed_data, test_edx_1, test_index)

goal_rmse <- 0.86490

# Data Exploration

head(edx)
summary(edx)

# How many rows and columns are there in the edx dataset?
dimensions <- dim(edx)
names(dimensions) <- c("Number of Rows", "Number of Columns")
dimensions 


# Deep dive into movie ratings
#Number of movies in the edx dataset
count_of_movies <- edx %>% 
                   summarise(n_distinct(movieId)) %>%
                   kable(col.names = c("Number of Movies in the Edx Dataset"))
count_of_movies

# Movie with the greatest number of ratings
highest_rated_movie <- edx %>% 
                       group_by(title) %>% 
                       summarise(count_of_ratings = n()) %>%
                       arrange(desc(count_of_ratings)) %>%  
                       slice(1) %>% 
                       kable(col.names = c("Highest Rated Movie", "Number of Ratings"))
highest_rated_movie

#Number of zeros given as ratings in the edx dataset
number_of_zero_ratings <- sum(edx$rating == 0)%>% 
                          kable(col.names = c("Number of Zero Ratings in the Edx Dataset"))
number_of_zero_ratings

# Which ratings are more common?
edx %>% 
  ggplot(aes(rating)) +
  geom_histogram(color = "black") +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Movie Rating Distribution",
  subtitle = "Higher and whole star ratings are more common") +
  labs(x = "Rating", y = "Count") +
  theme_classic()

# Distribution of number of ratings per movies
edx %>% 
  group_by(movieId) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=count))+
  geom_histogram(color = "black")+
  scale_x_log10()+ 
  ggtitle("Distribution of Number of Ratings by Number of Movies", 
  subtitle = "The distribution is almost like normal distribution") +
  labs(x="Number of Ratings",  y ="Number of Movies") + 
  theme_classic()

# Deep dive into users who are rating
#Number of users are in the edx dataset
users_who_are_rating <- edx %>% 
                        summarise(n_distinct(userId)) %>%
                        kable(col.names = c("Number of Users Who Are Rating"))
users_who_are_rating

# Distribution of number of ratings by number of users 
edx %>% 
  group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "black") +
  scale_x_log10() + 
  ggtitle("Distribution of Number of Ratings by Number of Rating per User", 
  subtitle="The distribution is skewed right (positively skewed)") +
  labs (x="Number of Ratings", y = "Number of Users") + 
  theme_classic()

# Deep dive into movie genres
# How many different genres are there in the data, and the average rating for this genre
edx %>% 
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n(), avg_rating = mean(rating)) %>%
  kable(col.names = c("Distinct Genres","Number of Ratings","Average Ratings"))

# Average rating per genre combinations with more than 50,000 ratings
edx %>% 
  group_by(genres) %>%
  summarize(n = n(), average = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 50000) %>% 
  mutate(genres = reorder(genres, average)) %>%
  ggplot(aes(x = genres, y = average, ymin = average - 2*se, ymax = average + 2*se)) + 
  geom_point() +
  theme_classic() +
  ggtitle("Avereage Rating per Genre Combination",
  subtitle="Average rating varies with different genres") +
  labs (x="Genres", y = "Average") + 
  theme(axis.text.x = element_text(angle = 90))


# Modeling Analysis

## Basic Model
# Calculate the average of all movies
mu_hat <- mean(train_edx$rating)

# Predict the RMSE on the test set
RMSE_basic <- RMSE(test_edx$rating, mu_hat)

# Save prediction into data frame
rmse_results <- data_frame(Model = "Average movie rating model"
                , RMSE = RMSE_basic
                , Difference_from_goal = round(goal_rmse - RMSE_basic,4))
rmse_results %>% kable()


# Movie Effect Model
# Calculate the average of all movies
mu_hat <- mean(train_edx$rating)

# Calculate the average by movie
movies <- train_edx %>% 
          group_by(movieId) %>% 
          summarize(b_i = mean(rating - mu_hat))

# Predict the RMSE on the test set
RMSE_movies_model <- test_edx %>% 
                     left_join(movies, by='movieId') %>%
                     mutate(pred = mu_hat + b_i) %>% pull(pred)

RMSE_movies_result <- RMSE(test_edx$rating,RMSE_movies_model)

# Adding the results to the results data set
rmse_results <- bind_rows(rmse_results
                , data_frame(Model ="Movie Effect Model"
                , RMSE = RMSE_movies_result
                , Difference_from_goal = round(goal_rmse - RMSE_movies_result,4)))
rmse_results %>% kable()


# Movie & User Effect Model
# Calculate the average of all movies
mu_hat <- mean(train_edx$rating)

# Calculate the average by movie
movies <- train_edx %>% 
          group_by(movieId) %>% 
          summarize(b_i = mean(rating - mu_hat))

# Calculate the average by user
users <- train_edx %>% 
         left_join(movies, by='movieId') %>% 
         group_by(userId) %>% 
         summarize(b_u = mean(rating - mu_hat - b_i))

# Compute the predicted ratings on test set

RMSE_movies_users_model <- test_edx %>% 
                           left_join(movies, by='movieId') %>% 
                           left_join(users, by='userId') %>% 
                           mutate(pred = mu_hat + b_i + b_u) %>% 
                           pull(pred)

RMSE_movies_users_result <- RMSE(test_edx$rating, RMSE_movies_users_model)

# Adding the results to the results data set
rmse_results <- bind_rows(rmse_results
                , data_frame(Model ="Movie + User Effect Model"
                , RMSE = RMSE_movies_users_result
                , Difference_from_goal = round(goal_rmse - RMSE_movies_users_result,4)))
rmse_results %>% kable()


# Movie, User & Genre Effect Model
# Calculate the average of all movies
mu_hat <- mean(train_edx$rating)

# Calculate the average by movie
movies <- train_edx %>% 
          group_by(movieId) %>% 
          summarize(b_i = mean(rating - mu_hat))

# Calculate the average by user
users <- train_edx %>% 
         left_join(movies, by='movieId') %>% 
         group_by(userId) %>% 
         summarize(b_u = mean(rating - mu_hat - b_i))

# Calculate the average by genre
genres <- train_edx %>%
          left_join(movies, by='movieId') %>% 
          left_join(users, by = 'userId') %>% 
          group_by(genres) %>% 
          summarize(b_r = mean(rating - mu_hat - b_i - b_u))

# Compute the predicted ratings on test data set

RMSE_movies_users_genre_model <- test_edx %>% 
                                 left_join(movies, by='movieId') %>% 
                                 left_join(users, by='userId') %>% 
                                 left_join(genres, by='genres') %>% 
                                 mutate(pred = mu_hat + b_i + b_u + b_r) %>% 
                                 pull(pred)

RMSE_movies_users_genre_result <- RMSE(test_edx$rating, RMSE_movies_users_genre_model)

# Adding the results to the results data set
rmse_results <- bind_rows(rmse_results
                , data_frame(Model ="Movie + User + Genre Effect Model"
                , RMSE = RMSE_movies_users_genre_result
                , Difference_from_goal = round(goal_rmse - RMSE_movies_users_genre_result,4)))
rmse_results %>% kable()


## Regularized Movie & User Effect Model
# Calculate the average of all movies
mu_hat <- mean(train_edx$rating)

# Define a table of lambdas
lambdas <- seq(0, 10, 0.1)

# Compute the predicted ratings on final_holdout_test data set using different values of lambda
rmses <- sapply(lambdas, function(lambda){
  
  # Calculate the average by movie
  b_i <- train_edx %>%
         group_by(movieId) %>%
         summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
  
  # Calculate the average by user
  b_u <- train_edx %>%
         left_join(b_i, by='movieId') %>%
         group_by(userId) %>%
         summarize(b_u = sum(rating - mu_hat - b_i) / (n() + lambda))
  
  # Calculate the average by genre
  b_r <- train_edx %>%
         left_join(movies, by='movieId') %>% 
         left_join(users, by = 'userId') %>% 
         group_by(genres) %>% 
         summarize(b_r = sum(rating - mu_hat - b_i - b_u)/ (n() + lambda))
  
  # Compute the predicted ratings on test data set
  RMSE_regularized_movies_users_model <- test_edx %>%
                                         left_join(b_i, by='movieId') %>%
                                         left_join(b_u, by='userId') %>%
                                         left_join(b_r, by='genres') %>% 
                                         mutate(pred = mu_hat + b_i + b_u + b_r) %>% 
                                         pull(pred)
  
  # Predict the RMSE on the test data set
  return(RMSE(test_edx$rating, RMSE_regularized_movies_users_model))
})

# Get the lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(rmses)]

# plot the result of lambdas
df <- data.frame(RMSE = rmses, Lambdas = lambdas)
      ggplot(df, aes(lambdas, rmses)) +
      geom_point() +
      theme_classic()  +
      labs(y = "RMSEs",x = "Lambdas")

# Predict the RMSE on the final_holdout_test set
RMSE_regularized_movies_users_result <- min(rmses)
RMSE_regularized_movies_users_result

best_lambda <- lambdas[which.min(rmses)]
best_lambda

# Adding the results to the results data set
rmse_results <- bind_rows(rmse_results
                , data_frame(Model ="Regularized Movie + User Based Model"
                , RMSE = RMSE_regularized_movies_users_result
                , Difference_from_goal = round(goal_rmse - RMSE_regularized_movies_users_result,4)))
rmse_results %>% kable()


# Final Model
# Calculate the average of all movies
mu_hat <- mean(edx$rating)

# Calculate the average by movie
b_i <- edx %>%
       group_by(movieId) %>%
       summarize(b_i = sum(rating - mu_hat) / (n() + best_lambda))

# Calculate the average by user
b_u <- edx %>%
       left_join(b_i, by='movieId') %>%
       group_by(userId) %>%
       summarize(b_u = sum(rating - b_i - mu_hat) / (n() + best_lambda))

# Compute the predicted ratings on final_holdout_test data set
Final_RMSE_regularized_movies_users_genre_model <- final_holdout_test %>%
                                                   left_join(b_i, by='movieId') %>%
                                                   left_join(b_u, by='userId') %>%
                                                   mutate(pred = mu_hat + b_i + b_u) %>%
                                                   pull(pred)

# Predict the RMSE on the final_holdout_test data set
Final_RMSE_regularized_movies_users_genre_result <- (RMSE(final_holdout_test$rating, Final_RMSE_regularized_movies_users_genre_model))

# Adding the results to the results data set
rmse_results <- bind_rows(rmse_results
                , data_frame(Model ="Final regularized Movie + User + Genre Based Model"
                , RMSE = Final_RMSE_regularized_movies_users_genre_result
                , Difference_from_goal = round(goal_rmse - Final_RMSE_regularized_movies_users_genre_result,4)))
rmse_results %>% knitr::kable()