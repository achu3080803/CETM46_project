###############################################################################################
# File:         global.R
# Description:  This file contains the intialization steps and some common functions of the application.
# Name:         Chu Siu Kay Alan
# Student No.:  189222006
###############################################################################################
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(proxy)
library(dplyr)

###############################################################################################
# Load the data sets to data frames (START)
###############################################################################################
#setwd("C:/Users/user/MScDataScience/CETM46/Assignment2")
cb_movies_df<-read.csv("ml-latest-small/cb_movies.csv", encoding="UTF8")
cf_als_imp_movies_df<-read.csv("ml-latest-small/cf_als_imp_movies.csv", encoding="UTF8")
cf_popular_movies_df<-read.csv("ml-latest-small/cf_popular_movies.csv", encoding="UTF8")
movies_df<-read.csv("ml-latest-small/movies1.csv", encoding="UTF8")
tags_df<-read.csv("ml-latest-small/tags.csv", encoding="UTF8")
ratings_df<-read.csv("ml-latest-small/ratings.csv", encoding="UTF8")
links_df<-read.csv("ml-latest-small/links.csv", encoding="UTF8")
###############################################################################################
# Load the data sets to data frames (END)
###############################################################################################




# Add an average rating column to movies_df
ratings_avg_df <- ratings_df %>%
  group_by(movieId) %>%
  dplyr::summarize(avgRating = mean(rating, na.rm=TRUE))
movies_df <- movies_df %>% inner_join(ratings_avg_df, by="movieId")

# Compose genres_df
genres_df <- movies_df %>%
  separate_rows(genres, sep = "\\|") %>%
  select(genres,year,avgRating)

genres_df <- genres_df %>%
  mutate(year = as.numeric(year))

# Clear all NAs
genres_df <- na.omit(genres_df)

# Add average rating to tags_df
tags_df <- tags_df %>% left_join(ratings_avg_df, by='movieId')

# Add movie production year column to tags_df
year_df<-movies_df %>%
  select(movieId, year)
tags_df <- tags_df %>% left_join(year_df, by="movieId")

# Derive year of rating from timestamp column
ratings_df <- ratings_df %>%
  mutate(ratedYear=substr(as.POSIXct(ratings_df$timestamp, origin='1970-01-01', tz="UTC"),1,4))
ratings_df <- ratings_df %>% left_join(year_df, by="movieId")
ratings_avg_df  <- ratings_avg_df %>% left_join(year_df, by="movieId")


###############################################################################################
# Function getTermMatrix
#
# Description:  Return a matrix that contains genres of the movies that satisfies the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting movie production year
#               to_year   - Ending movie production year
#               min_rating - Minimum average ratings that the movie should have
#               max_rating - Maximum average ratings that the movie should have
# Output:       Return a matrix that contains genres of the movies that satisfies the input criteria.
###############################################################################################
getTermMatrix <- memoise(function(from_year, to_year, min_rating, max_rating) {
  
  print("global.R")
  print(from_year)
  print(to_year)
  print(min_rating)
  print(max_rating)
  genres_df1 <- genres_df %>%
    filter(between(genres_df$year, from_year, to_year), between(genres_df$avgRating, min_rating, max_rating))
  
  print("# of genre_df1")
  print(nrow(genres_df1))
  
  # print(min(genres_df$year))
  # print(max(genres_df$year))
  if (nrow(genres_df1) == 0 ){
    text <- c("NOTHING")
  }
  else {
    text <- as.vector(genres_df1$genres)
  }
  
  
  myCorpus = Corpus(VectorSource(text))
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

###############################################################################################
# Function getTermMatrix
#
# Description:  Return a matrix that contains tags of the movies that satisfies the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting movie production year
#               to_year   - Ending movie production year
#               min_rating - Minimum average ratings that the movie should have
#               max_rating - Maximum average ratings that the movie should have
# Output:       Return a matrix that contains tags of the movies that satisfies the input criteria.
###############################################################################################
getTagMatrix <- memoise(function(from_year, to_year, min_rating, max_rating) {
  
  print("global.R")
  print(from_year)
  print(to_year)
  print(min_rating)
  print(max_rating)
  tags_df1 <- tags_df %>%
    filter(between(year, from_year, to_year), between(tags_df$avgRating, min_rating, max_rating))
  
  print("# of tags_df1")
  print(nrow(tags_df1))
  
  # print(min(genres_df$year))
  # print(max(genres_df$year))
  if (nrow(tags_df1) == 0 ){
    text <- c("NOTHING")
  }
  else {
    text <- as.vector(tags_df1$tag)
  }
  
  
  myCorpus = Corpus(VectorSource(text))
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})


###############################################################################################
# Function getRatingHistogramDF
#
# Description:  Return a data frame that contains ratings of the movies that satisfies the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting movie production year
#               to_year   - Ending movie production year
# Output:       Return a data frame that contains ratings of the movies that satisfies the input criteria. 
###############################################################################################
getRatingHistogramDF <- memoise(function(from_year, to_year) {
  
  print("global.R - ratingHistogram")
  print(from_year)
  print(to_year)
  ratings_df1 <- ratings_df %>%
    filter(between(year, from_year, to_year)) 
  
  print("# of ratings_avg_df1")
  print(nrow(ratings_df1))
  
  return(ratings_df1)
})


###############################################################################################
# Function getAvgRatingHistogramDF
#
# Description:  Return a data frame that contains average ratings of the movies that satisfies the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting movie production year
#               to_year   - Ending movie production year
# Output:       Return a data frame that contains average ratings of the movies that satisfies the input criteria. 
###############################################################################################
getAvgRatingHistogramDF <- memoise(function(from_year, to_year) {
  
  print("global.R - avgRatingHistogram")
  print(from_year)
  print(to_year)
  ratings_avg_df1 <- ratings_avg_df %>%
    filter(between(year, from_year, to_year)) 
  
  print("# of ratings_avg_df1")
  print(nrow(ratings_avg_df1))
  
  return(ratings_avg_df1)
})

###############################################################################################
# Function getUserRatingHistogramDF
#
# Description:  Return a data frame that contains histogram information about number of ratings given by each user. 
#               Using "memoise" to automatically cache the results
# Input:        
# Output:       Return a data frame that contains histogram information about number of ratings given by each user.
###############################################################################################
getUserRatingHistogramDF <- memoise(function() {
  
  print("global.R - userRatingHistogram")

  user_ratings_df1 <- ratings_df %>%
    group_by(ratings_df$userId, ratings_df$ratedYear) %>%
    summarise(r_cnt_by_user=n())
  colnames(user_ratings_df1) <- c("userId","ratedYear","cntByUser")
  user_ratings_df2 <- aggregate(user_ratings_df1[,3], list(user_ratings_df1$ratedYear), mean)
  colnames(user_ratings_df2)<- c("year","avgCntPerUser")
  #user_ratings_df2$year <- as.integer(user_ratings_df2$year)
  return(user_ratings_df2)
})

###############################################################################################
# Function getTop10MovieDF
#
# Description:  Return a data frame that contains top 10 movies that satisfy the input criteria. 
#               Using "memoise" to automatically cache the results
# Input:        from_year - Starting year of the ratings
#               to_year   - Ending year of the ratings
#               minRatingCnt - Minimum number of ratings the movies should have
# Output:       Return a data frame that contains top 10 movies that satisfy the input criteria.
###############################################################################################
getTop10MovieDF <- memoise(function(from_year, to_year, minRatingCnt) {
  
  print("global.R - top10Movie")
  
  top10_df1 <- ratings_df %>%
    filter(between(ratedYear, from_year, to_year))
  
  # Find the average rating for each movie in each year
  top10_df2 <- aggregate(top10_df1[,3], list(top10_df1$movieId), mean)
  colnames(top10_df2)<- c("movieId", "avgRating")
  
  top10_df3 <- top10_df1 %>%
    group_by(movieId) %>%
    summarise(ratingCnt=n())
  
  top10_df4 <- left_join(top10_df2,top10_df3,c("movieId"))
  
  # Remove the movies that has number of ratings less than minRatingCnt
  top10_df5 <- top10_df4 %>%
    filter(ratingCnt>=minRatingCnt)
  
  top10_df6 <- top10_df5 %>%
    arrange(desc(top10_df5$avgRating, top10_df5$ratingCnt))
  
  top10_df7 <- head(top10_df6,10)
  
  return(top10_df7)
})



