## Building the recommender system

library(tidyverse)

load("data/recommender.RData")
viewed.movies
ratings.red

# convert the data frame to matrix form for later functions
sorted_my_user <- as.character(unlist(viewed.movies[,1])) # extract char list of userId's
viewed.movies <- as.matrix(viewed.movies[,-1]) # creating matrix of data frame without the userId col
row.names(viewed.movies) <- sorted_my_user # naming the rows using the userId's

# user collaborative filtering

# simple approach: recommend most watched movies to those who haven't yet watched it
sort(apply(viewed.movies, 2, sum), decreasing = TRUE)

# better approach is to measure similarity between users
# and then recommend movies to similar users who haven't yet been seen by them

# measuring similarity between two users (cosine similarity)
cosine.sim <- function(a, b){
  crossprod(a, b)/(sqrt(crossprod(a) * crossprod(b)))
}

# maximum similarity
x1 <- c(1,1,1,0,0)
x2 <- c(1,1,1,0,0)
cosine.sim(x1, x2)

# max dissimilarity
x3 <- c(0,0,0,1,1)
cosine.sim(x1, x3)

# using example from the data set
as.numeric(viewed.movies[1,]) # first user
as.numeric(viewed.movies[2,]) # second user

cosine.sim(as.numeric(viewed.movies[1,]), as.numeric(viewed.movies[2,]))

# finding similarities between each user pair
user.similarities <- matrix(0, nrow = 15, ncol = 15)
for(i in 1:14){
  for(j in (i + 1):15){
    user.similarities[i,j] <- cosine.sim(viewed.movies[i,], viewed.movies[j,])
  }
}

user.similarities <- user.similarities + t(user.similarities)
diag(user.similarities) <- 0
row.names(user.similarities) <- row.names(viewed.movies)
colnames(user.similarities) <- row.names(viewed.movies)
round(user.similarities, 3)

# who is most similar to user 222
sort(user.similarities["222",])


# recommending a movie for a single user
# idea: recommend a movie to a user based on which similar user has watched it.
# i.e weight the movies from similar users higher

# showed using one movie (2001: A space odyssey)
viewed.movies["222",]

seen.movie <- viewed.movies[, "2001: A Space Odyssey (1968)"]
sim.to.user <- user.similarities["222",]
cbind(seen.movie, sim.to.user)

# working out the overall recommendation score for 2001: a space odyssey for user 222
# crossproduct between seen and similarities
crossprod(viewed.movies[,"2001: A Space Odyssey (1968)"], user.similarities["222",])

# now compare the score to all the movies for 222 and see which movies have the highest score
t(user.similarities["222",] %*% viewed.movies)

# now filter out the movies user 222 has already seen and sort in descending order
user.scores <- data.frame(title = colnames(viewed.movies),
                          score = as.vector(user.similarities["222",] %*% viewed.movies),
                          seen = viewed.movies["222",],
                          row.names = c(1:20))
user.scores %>%
  filter(seen == 0) %>%
  arrange(desc(score))

# function to generate CF score for any user
user_based_rec <- function(user, user.sim, viewed.mov){
  
  # ensuring user is type character
  user <- ifelse(is.character(user), user, as.character(user))
  
  user.scores <- data.frame(title = colnames(viewed.mov),
                            score = as.vector(user.sim[user,] %*% viewed.mov),
                            seen = as.vector(viewed.mov[user,]))
  
  # sort unseen movies by score and remove the seen
  user.scores %>%
    filter(seen == 0) %>%
    arrange(desc(score)) %>%
    select(-seen)
}

# running function on user we've seen before
user_based_rec("222", user.similarities, viewed.movies)

# apply to all users using lapply
lapply(sorted_my_user, user_based_rec, user.similarities, viewed.movies)

#### recommender system using item-based collaborative filtering ####

# compute which movies are similar to each other (same as before using the cosine similarity method)
movies.user <- t(viewed.movies)

# finding similarities between each movie pair
movie.similarities <- matrix(0, nrow = 20, ncol = 20)
for(i in 1:19){
  for(j in (i + 1):20){
    movie.similarities[i,j] <- cosine.sim(movies.user[i,], movies.user[j,])
  }
}

movie.similarities <- movie.similarities + t(movie.similarities)
diag(movie.similarities) <- 0
row.names(movie.similarities) <- colnames(viewed.movies)
colnames(movie.similarities) <- colnames(viewed.movies)

# checking which movies are most similar to apocolypse now
sort(movie.similarities[,"Apocalypse Now (1979)"], decreasing = T)

# recommend a movie for a single user
ratings.red %>%
  filter(userId == 372) %>%
  select(userId, title) # shows movies watched by user 372

# finding similarities between the movies watched by 372 and other movies
# sum up the similarities to get the similarity score.
# most similar is best to recommend
user.seen <- ratings.red %>%
  filter(userId == 372) %>%
  select(title) %>%
  unlist() %>%
  as.character() # obtaining movies seen by user 372

sort(movie.similarities[,user.seen[1]], decreasing = T) # checking most similar to first of the movies of 372

# getting similarity score for 372
sort(apply(movie.similarities[,user.seen], 1, sum), decreasing = T)

# putting it in a function
item_based_rec <- function(user, movie.sim, viewed.mov){
  
  user <- ifelse(is.character(user), user, as.character(user))
  
  # get scores
  user_seen <- row.names(movie.sim)[viewed.mov[user,] == T] #getting seen movies
  user.scores <- tibble(title = row.names(movie.sim),
                        score = apply(movie.sim[,user_seen], 1, sum),
                        seen = viewed.mov[user,])
  
  # sort unseen movies by score and remove the seen
  user.scores %>%
    filter(seen == 0) %>%
    arrange(desc(score)) %>%
    select(-seen)
}

# testing with user 222
item_based_rec("222", movie.similarities, viewed.movies)

# for all users
lapply(sorted_my_user, item_based_rec, movie.similarities, viewed.movies)

