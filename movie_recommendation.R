#loading the needed libraries for performing desired computations

library(dslabs)
library(tidyverse)
library(caret)
library(lubridate)
library(class)

#ensuring that the numeric output is easily understandable

format(99999999,scientific = F)
options(scipen=999)

#previewing the edx dataset

head(edx)
summary(edx)

#displaying the size and number of distinct users and distinct movies in the dataset

dim(edx)

edx %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

#taking a look at the histogram of a log of distributions number of ratings movies received

edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

#a look at the amount of times each user gave a rating

edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

#creating a dummy variable for each genre

df_edx = edx %>% mutate(Comedy = ifelse(str_detect(genres, "Comedy"), 1, 0))
df_edx = df_edx %>% mutate(Romance = ifelse(str_detect(genres, "Romance"), 1, 0))
df_edx = df_edx %>% mutate(Action = ifelse(str_detect(genres, "Action"), 1, 0))
df_edx = df_edx %>% mutate(Crime = ifelse(str_detect(genres, "Crime"), 1, 0))
df_edx = df_edx %>% mutate(Thriller = ifelse(str_detect(genres, "Thriller"), 1, 0))
df_edx = df_edx %>% mutate(Drama = ifelse(str_detect(genres, "Drama"), 1, 0))
df_edx = df_edx %>% mutate(Sci_Fi = ifelse(str_detect(genres, "Sci-Fi"), 1, 0))
df_edx = df_edx %>% mutate(Adventure = ifelse(str_detect(genres, "Adventure"), 1, 0))
df_edx = df_edx %>% mutate(Children = ifelse(str_detect(genres, "Children"), 1, 0))
df_edx = df_edx %>% mutate(Fantasy = ifelse(str_detect(genres, "Fantasy"), 1, 0))
df_edx = df_edx %>% mutate(War = ifelse(str_detect(genres, "War"), 1, 0))
df_edx = df_edx %>% mutate(Animation = ifelse(str_detect(genres, "Animation"), 1, 0))
df_edx = df_edx %>% mutate(Musical = ifelse(str_detect(genres, "Musical"), 1, 0))
df_edx = df_edx %>% mutate(Western = ifelse(str_detect(genres, "Western"), 1, 0))
df_edx = df_edx %>% mutate(Mystery = ifelse(str_detect(genres, "Mystery"), 1, 0))
df_edx = df_edx %>% mutate(Film_Noir = ifelse(str_detect(genres, "Film-Noir"), 1, 0))
df_edx = df_edx %>% mutate(Horror = ifelse(str_detect(genres, "Horror"), 1, 0))
df_edx = df_edx %>% mutate(Documentary = ifelse(str_detect(genres, "Documentary"), 1, 0))
df_edx = df_edx %>% mutate(IMAX = ifelse(str_detect(genres, "IMAX"), 1, 0))
df_edx = df_edx %>% mutate(no_genres_listed = ifelse(str_detect(genres, "no_genres_listed"), 1, 0))

#extracting the year from the title variable, in numeric format

df_edx = df_edx %>% mutate(year = str_extract(title, "([\\d]{4})"))
df_edx = df_edx %>% mutate(year = as.numeric(year))
df_edx = df_edx %>% mutate(year = str_extract(title, "\\([\\d]{4}\\)"))
df_edx = df_edx %>% mutate(year = str_extract(year, "[0123456789]{4}"))
df_edx = df_edx %>% mutate(year = as.numeric(year))

#creating a variable time_diff, which denotes the difference in years between when the movie was premiered and when it was rated

df_edx = df_edx %>% mutate(year_rated = year(as_datetime(timestamp)))
df_edx = df_edx %>% mutate(time_diff = year_rated - year)

#removing the tiny percentage of cases when the movie was rated prior to premiering

df_edx = df_edx %>% filter(time_diff >= 0)

#exploring the range of premiering years in the dataset as well as what the newest and oldest movies are

which.max(df_edx$year)
max(df_edx$year)
min(df_edx$year)
df_edx[which.max(df_edx$year),]
df_edx[which.min(df_edx$year),]

#number of movies premiered by year

max(df_edx$year)
class(df_edx$year)
movies_per_year = df_edx %>% group_by(year) %>% summarize(num_of_movies = n_distinct(movieId))
movies_per_year %>% ggplot(aes(year, num_of_movies)) + geom_point() + geom_line() + labs(y = "number of movies premiered")

#splitting the edx dataset into a training and testing set

set.seed(755)
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- df_edx[-test_index,]
test_set <- df_edx[test_index,]

#looking at the distributions of ratings and premiering years

hist(train_set$rating, xlab = "rating", main = "Histogram of Ratings")

train_set %>% group_by(rating) %>% summarize(percent = 100*n()/nrow(train_set))

year_count = train_set %>% group_by(year) %>% summarize(count = n())

year_count %>% ggplot(aes(year, count)) + geom_point()

#looking at views by genre

genres = train_set[,7:26]

sums_of_genres = apply(genres, 2, sum)

genre_names = c("Comedy", "Romance", "Action", "Crime", "Thriller", "Drama", "Sci_Fi", "Adventure", "Children", "Fantasy", "War", "Animation", "Musical", "Western", "Mystery", "Film_Noir", "Horror", "Documentary", "IMAX", "No Genres Listed")

genre_sums = c(2832623, 1369430, 2047801, 1062303, 1860727, 3128554, 1072313, 1526413, 590470, 740434, 409431, 373977, 346699, 151311, 455013, 94810, 553406, 74571, 6569, 0)

views_by_genre = tibble(genre_names, genre_sums)
views_by_genre = views_by_genre %>% mutate(percent = 100*genre_sums/sum(genre_sums))

views_by_genre %>% ggplot(aes(genre_names, percent)) + geom_point() + theme(axis.text.x=element_text(angle=75, hjust=1))

#ensuring that you have all the edx observations with the userIds and movieIds selected in the test_set

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#creating a function that calculates MSE

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Simplest recommendation system using the average rating only

head(train_set)

mu <- mean(train_set$rating)
mu

model_1_rmse <- RMSE(test_set$rating, mu)
model_1_rmse

rmse_results <- data_frame(method = "Model (1)", RMSE = model_1_rmse)

#calculating SSE using a model that includes the overall average and movie effect 

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("black"))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model (2)",
                                     RMSE = model_2_rmse ))

rmse_results %>% knitr::kable()

#calculating SSE using a model that includes overall average, movie effect and user effect

user_avgs <- train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

max(train_set$rating)
summary(train_set$rating)

user_avgs %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))

df_edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model (3)",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

#looking at the time_diff distribution

time_diff_count = df_edx %>% group_by(time_diff) %>% summarize(count = n())

time_diff_count %>% ggplot(aes(time_diff, count)) + geom_point() + scale_y_continuous(labels = scales::comma) + theme(axis.text.y = element_text(angle = 45))

#looking at the average rating by time_diff

rating_by_time_diff = df_edx %>% group_by(time_diff) %>% summarize(avg_rating = mean(rating))
rating_by_time_diff %>% ggplot(aes(time_diff, avg_rating)) + geom_point()

#removing observations from train set where time_diff < 0

train_set = train_set %>% filter(time_diff >= 0)

#calculating the average difference between rating and predicted rating by time_diff, where the predicted rating now includes the movie effect and the user effect  

time_diff_avgs = train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>% group_by(time_diff) %>% summarize(b_t = mean(rating - mu - b_i - b_u))

#distribution of average rating differences by time difference between rating and predicted ratings  

time_diff_avgs %>% qplot(b_t, geom ="histogram", bins = 30, data = ., color = I("black"))

#predicting ratings for the test set using all three effects: user effect, movie effect and time_diff effect

predicted_ratings = test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(time_diff_avgs, by = 'time_diff') %>%
  mutate(pred = mu + b_i + b_u + b_t) %>% pull(pred)

#ensuring that the given ratings and predicted ratings are both numeric so that they can be inserted into the RMSE function

class(test_set$rating)
class(predicted_ratings)

#calculating the RMSE for the fourth model with user effect, movie effect and time_diff effect and putting all the models with their results into a table

model_4_rmse = RMSE(test_set$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model (4)",  
                                     RMSE = model_4_rmse ))
rmse_results %>% knitr::kable()                                                                                                                                                                                                     
#adding regularization to the fourth model and determining the best lambda

lambdas = seq(0, 10, 0.25) 

rmses = sapply(lambdas, function(lambda){
  mu <- mean(train_set$rating) 
  movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n() + lambda))
  
  user_avgs <- train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + lambda))
  
  time_diff_avgs = train_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>% 
    group_by(time_diff) %>% 
    summarize(b_t = sum(rating - mu - b_i - b_u)/(n() + lambda))
  
  predicted_ratings_reg = test_set %>%
    left_join(movie_avgs, by = 'movieId') %>%
    left_join(user_avgs, by = 'userId') %>%
    left_join(time_diff_avgs, by = 'time_diff') %>%
    mutate(pred = mu + b_i + b_u + b_t) %>% pull(pred)
  
  RMSE(test_set$rating, predicted_ratings_reg)
})


lambda_rmses = data.frame(lambdas, rmses)

best_lambda = lambda_rmses %>% filter(rmses == min(rmses)) %>% pull(lambdas)

lambda_5_rmse = lambda_rmses %>% filter(lambdas == 5) %>% pull(rmses)

lambda_rmses %>% ggplot(aes(lambdas, rmses)) + geom_point()


rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model (5)",  
                                     RMSE = lambda_5_rmse))
rmse_results %>% knitr::kable() 


#editing validation dataset like the edx dataset 

val = validation %>% mutate(Comedy = ifelse(str_detect(genres, "Comedy"), 1, 0))
val = val %>% mutate(Romance = ifelse(str_detect(genres, "Romance"), 1, 0))
val = val %>% mutate(Action = ifelse(str_detect(genres, "Action"), 1, 0))
val = val %>% mutate(Crime = ifelse(str_detect(genres, "Crime"), 1, 0))
val = val %>% mutate(Thriller = ifelse(str_detect(genres, "Thriller"), 1, 0))
val = val %>% mutate(Drama = ifelse(str_detect(genres, "Drama"), 1, 0))
val = val %>% mutate(Sci_Fi = ifelse(str_detect(genres, "Sci-Fi"), 1, 0))
val = val %>% mutate(Adventure = ifelse(str_detect(genres, "Adventure"), 1, 0))
val = val %>% mutate(Children = ifelse(str_detect(genres, "Children"), 1, 0))
val = val %>% mutate(Fantasy = ifelse(str_detect(genres, "Fantasy"), 1, 0))
val = val %>% mutate(War = ifelse(str_detect(genres, "War"), 1, 0))
val = val %>% mutate(Animation = ifelse(str_detect(genres, "Animation"), 1, 0))
val = val %>% mutate(Musical = ifelse(str_detect(genres, "Musical"), 1, 0))
val = val %>% mutate(Western = ifelse(str_detect(genres, "Western"), 1, 0))
val = val %>% mutate(Mystery = ifelse(str_detect(genres, "Mystery"), 1, 0))
val = val %>% mutate(Film_Noir = ifelse(str_detect(genres, "Film-Noir"), 1, 0))
val = val %>% mutate(Horror = ifelse(str_detect(genres, "Horror"), 1, 0))
val = val %>% mutate(Documentary = ifelse(str_detect(genres, "Documentary"), 1, 0))
val = val %>% mutate(IMAX = ifelse(str_detect(genres, "IMAX"), 1, 0))
val = val %>% mutate(no_genres_listed = ifelse(str_detect(genres, "no_genres_listed"), 1, 0))

val = val %>% mutate(year = str_extract(title, "([\\d]{4})"))
val = val %>% mutate(year = as.numeric(year))
val = val %>% mutate(year = str_extract(title, "\\([\\d]{4}\\)"))
val = val %>% mutate(year = str_extract(year, "[0123456789]{4}"))
val = val %>% mutate(year = as.numeric(year))

val = val %>% mutate(year_rated = year(as_datetime(timestamp)))
val = val %>% mutate(time_diff = year_rated - year) 



#calculating the RMSE using the single value of the rating mean of the training dataset, just for comparison purposes

RMSE(mu, val$rating)

#incorporating regularization with the previously selected optimal lambda 

lambda = 5 


mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n() + lambda))

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + lambda))

time_diff_avgs = train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>% 
  group_by(time_diff) %>% 
  summarize(b_t = sum(rating - mu - b_i - b_u)/(n() + lambda))

predicted_ratings_reg = val %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(time_diff_avgs, by = 'time_diff') %>%
  mutate(pred = mu + b_i + b_u + b_t) %>% pull(pred)

predicted_ratings_reg = replace_na(predicted_ratings_reg, mu)

model_5_rmse_val = RMSE(val$rating, predicted_ratings_reg)

#running model 4 on validation set

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/n())

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/n())

time_diff_avgs = train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>% 
  group_by(time_diff) %>% 
  summarize(b_t = sum(rating - mu - b_i - b_u)/n())

predicted_ratings_no_reg = val %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(time_diff_avgs, by = 'time_diff') %>%
  mutate(pred = mu + b_i + b_u + b_t) %>% pull(pred)

predicted_ratings_no_reg = replace_na(predicted_ratings_no_reg, mu)

model_4_rmse_val = RMSE(val$rating, predicted_ratings_no_reg)



#putting the results on the validation set into a table

final_result_table = data.frame(Method = "Model (1)", RMSE = RMSE(mu, val$rating))
final_result_table = bind_rows(final_result_table, data.frame(Method = "Model (4)", RMSE = model_4_rmse_val))
final_result_table = bind_rows(final_result_table, data.frame(Method = "Model (5)", RMSE = model_5_rmse_val))

final_result_table %>% knitr::kable()



#just for interest trying to run knn on a small portion of the training set

set.seed(755)

nrow(train_set)

time_diff_avgs = train_set %>% group_by(movieId) %>% summarize(avg_time_diff = mean(time_diff))
movie_id_avgs = train_set %>% group_by(movieId) %>% summarize(avg_rating = mean(rating))

dups = duplicated(train_set$movieId)

train_set_no_dups = train_set[!dups]

train_set_no_dups = left_join(train_set_no_dups, time_diff_avgs, by = "movieId")
train_set_no_dups = left_join(train_set_no_dups, movie_id_avgs, by = "movieId")


test_ind_knn = createDataPartition(y = train_set_no_dups$avg_rating, times = 1, p = 0.2, list = FALSE)


test_set_knn = train_set_no_dups[test_ind_knn,]
train_set_knn = train_set_no_dups[-test_ind_knn,]

head(train_set_knn)
head(test_set_knn)

train_set_knn = train_set_knn %>% select(Drama, Comedy, Romance, Action, Crime, Thriller, Sci_Fi, Adventure, Children, Fantasy, War, Animation, Musical, Western, Mystery, Horror, Documentary, avg_time_diff, avg_rating)
test_set_knn = test_set_knn %>% select(Drama, Comedy, Romance, Action, Crime, Thriller, Sci_Fi, Adventure, Children, Fantasy, War, Animation, Musical, Western, Mystery, Horror, Documentary, avg_time_diff, avg_rating)

head(train_set_knn)
head(test_set_knn)

dim(train_set_knn)
dim(test_set_knn)

tr_set_predictors = train_set_knn[,1:18]
ts_set_predictors = test_set_knn[,1:18]

normalize = function(x){
  (x - mean(x))/sd(x)
}

tr_set_predictors = apply(tr_set_predictors, MARGIN = 2, FUN = normalize)
ts_set_predictors = apply(ts_set_predictors, MARGIN = 2, FUN = normalize)

train_set_knn_norm = cbind(tr_set_predictors, avg_rating = train_set_knn$avg_rating)
test_set_knn_norm = cbind(ts_set_predictors, avg_rating = test_set_knn$avg_rating)

length(preds_knn)
length(test_set_knn$avg_rating)

class(preds_knn)
class(as.factor(test_set_knn$avg_rating))

preds_knn = knn(train_set_knn_norm, test_set_knn_norm, train_set_knn$avg_rating, k = 3)

#trying knn for different values of k

k = 1:50

rmses = sapply(k, function(x){
  preds_knn = knn(train_set_knn_norm, test_set_knn_norm, train_set_knn$avg_rating, k = x)
  RMSE(as.numeric(as.character(preds_knn)), test_set_knn$avg_rating)
})

rmses_by_k = tibble(K = k, RMSE = rmses)

rmses_by_k %>% ggplot(aes(K, rmses)) + geom_point()
