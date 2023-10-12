# Weather Prediction by Luke, Kavi

# main
weather_prediction <- function(df, x, y, seas, year, lag=6, norm=T, split=0.7){
  df <- df[, append(x, y)]
  if(norm==T){df <- scale(df[, x])}
  df <- train_test(df, split)
  df$x_train <- delay_map(df$x_train, lag)
  df$y_train <- delay_map(df$y_train, lag)
  nums <- random_nums(length(x), 1000, ncol(df$x_train)) # find n= samples for each season of each year
  # nearest neighbor, lars
  # prob distribution
}

# splits data
train_test <- function(df, split){
  sample <- sample(c(TRUE, FALSE), nrow(df), replace=F, prob=c(split,1-split))
  y <- ncol(df)
  x_train <- df[sample, -y]
  y_train <- df[sample, y]
  x_test <- df[!sample, -y]
  y_test <- df[!sample, y]
  my_list <- list(x_train=x_train, y_train=y_train, x_test=x_test, y_test=y_test)
  return(my_list)
}

# makes delay map
delay_map <- function(df, lag){
  delay <- df
  for(i in 1:lag){
    delay <- rbind(0, delay)
    df <- cbind(df, delay)
  }
  df <- df[lag:(nrow(df)-lag), ]
  return(df)
}

# picks x random values n times from 1:k
random_nums <- function(x, n, k){
  nums <- NULL
  for(i in 1:n){
    sample <- sample(c(1:k), x)
    nums <- rbind(nums, sample)
  }
  return(nums)
}
