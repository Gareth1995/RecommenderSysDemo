## Recommender system using user-based collaborative filtering

library(tidyverse)

# data wrangling to get data into ideal structure

# downloading the movies data
download.file(url = "http://files.grouplens.org/datasets/movielens/ml-latest-small.zip",
              destfile = "data/ml-latest-small.zip")
unzip("data/ml-latest-small.zip", exdir = "data")

# saving csv data into a Rdata object
movies <- read.csv("data/ml-latest-small/movies.csv")
ratings <- read.csv("data/ml-latest-small/ratings.csv")
links <- read.csv("data/ml-latest-small/links.csv")
tags <- read.csv("data/ml-latest-small/tags.csv")

save(movies, ratings, links, tags, file = "data/movielens-small.RData")
load("data/movielens-small.RData")

ratings <- as_tibble(ratings) # more efficient way of using and viewing datasets
ratings
str(ratings)
glimpse(ratings) # more detailed summary of the tibble

# joining the movies and ratings
ratings <- left_join(ratings, movies) # joined using movieId

# extracting 20 movies and 15 users to build the datset used for the recommender system
users.freq <- ratings %>%
  group_by(userId) %>%
  summarise(count = n()) %>% # number of observations which has the specific user ID
  arrange(desc(count))

my.users <- users.freq$userId[101:115] # obtaining 15 userIds

# selecting the 20 movies we want to use (ordering the movies by most watched)
movies.freq <- ratings %>%
  group_by(movieId) %>%
  summarise(count = n()) %>% # number of observations which has the specific user ID
  arrange(desc(count))

my.movies <- movies.freq$movieId[101:120]

# create the dataset with only 15 users and 20 movies
ratings.red <- ratings %>%
  filter(userId %in% my.users, movieId %in% my.movies)

# checking dataset
n_users <- length(unique(ratings.red$userId)) # unique() returns only unique observations
n_movies <- length(unique(ratings.red$movieId))

paste("Number of users are ", n_users)
paste("Number of movies are ", n_movies)

# Checking what the 20 movies are
movies %>% 
  filter(movieId %in% ratings.red$movieId)

# changing the shape of the data frame using pivot_wider()
ratings.red %>% pivot_wider(names_from = movieId, values_from = rating) # names from determines the column values
# end up with more than one row per user so it isn't right

# improvment
ratings.red %>%
  select(userId, title, rating) %>%
  pivot_wider(names_from = title, values_from = rating)

# creating a seen variable to tell if a user has seen a movie or not
viewed.movies <- ratings.red %>%
  complete(userId, title) %>% # alters missing values based on combonation of variables
  mutate(seen = ifelse(is.na(rating), 0, 1)) %>%
  select(userId, title, seen) %>%
  pivot_wider(names_from = title, values_from = seen)
viewed.movies  # shows which users haven't seen which movies

# saving the output
save(ratings.red, viewed.movies, file = "data/recommender.RData")


