# Weather Prediction by Luke, Kavi

# df with 7 rows of random ints 0:5 1000 times
data_fake <- data.frame(replicate(7,sample(0:5,1000,rep=TRUE)))

# clean data before

# main
weather_prediction <- function(data, x, y, seas, year, lag=6, norm=F, split=0.7){
  df <- data[, append(x, y)]
  if(norm==T){df <- scale(df[, x])}
  df <- delay_map(df, lag)
  
  # split data
  split <- floor(split*nrow(df))
  df_train <- df[1:split,]
  df_test <- df[split+1:nrow(df),]

  # calculate attractor dimension, runs on LuValle's code
  # dim <- 2.5*dim.est.calc(rev(df_test))
  # print(dim/2.5)
  
  # random samples to pull from columns
  delay_sample <- delay_sample(1200, ncol(df_train), 7)
  
  # seperate seasons
  season1_train <- df_train[seq(1, nrow(df_train), 4), ]
  season2_train <- df_train[seq(2, nrow(df_train), 4), ]
  season3_train <- df_train[seq(3, nrow(df_train), 4), ]
  season4_train <- df_train[seq(4, nrow(df_train), 4), ]
  
  season1_test <- df_test[seq(1, nrow(df_train), 4), ]
  season2_test <- df_test[seq(2, nrow(df_train), 4), ]
  season3_test <- df_test[seq(3, nrow(df_train), 4), ]
  season4_test <- df_test[seq(4, nrow(df_train), 4), ]
  
  
  
  # nearest neighbor, lars
  # prob distribution
}

# makes delay map
delay_map <- function(df, lag){
  delay <- df
  names <- colnames(delay)
  colnames(df) <- paste(as.character(0), names, sep = "")
  for(i in 1:lag){
    colnames(delay) <- paste(as.character(i), names, sep = "")
    delay <- rbind(0, delay)
    delay <- delay[-nrow(delay),]
    df <- cbind(df, delay)
  }
  df <- df[lag+1:(nrow(df)-lag), ]
  return(df)
}

# nsel samples of ncol values, ntrial times
delay_sample <- function(ntrial=1200, ncol, nsel){
  nums <- list()
  for(i in 1:ntrial){
    sample <- sample(1:ncol, nsel)
    nums[[i]] <- sample
  }
  return(nums)
}

weather_prediction(data_fake, x=1:6, y=7, seas=4, year=1, lag=6, norm=F, split=0.7)

