# Weather Prediction by Luke, Kavi

library(glmnet)

# df with 7 rows of random ints 0:5 200 times
data_fake <- data.frame(replicate(7,sample(0:5,200,rep=TRUE)))

# clean data before

# main
weather_prediction <- function(data, x, y, seas, year, lag=6, nn, norm=F, split=0.7){
  df <- data[, append(x, y)]
  if(norm==T){df <- scale(df[, x])}
  df <- delay_map(df, lag)
  
  # split data
  split <- floor(split*nrow(df))
  df_train <- df[1:split,]
  df_test <- df[split+1:nrow(df),]

  # calculate attractor dimension, runs on LuValle's code
  dim = 7
  # dim <- 2.5*dim.est.calc(rev(df_test))
  # print(dim/2.5)
  
  # random samples to pull from columns
  sample_cols <- delay_sample(1, (ncol(df_train)-1), dim)
  
  for(i in seas){
    # seperate seasons
    season_train <- df_train[seq(i, nrow(df_train), 4), ]
    season_test <- df_test[seq(i, nrow(df_train), 4), ]
    rownames(season_train) <- rownames(season_test) <- NULL
    for(j in year){
      # distance matrix
      nn_rows <- nn_finder(season_train, season_test[j,], nn)
      nn_train <- as.matrix(season_train[nn_rows,])
      y_test <- as.matrix(season_test[j, y])
      for(k in 1:length(sample_cols)){
        lars_x <- nn_train[, sample_cols[[k]]]
        lars_y <- nn_train[, y]
        print(lars_x)
        print('hey')
        print(lars_y)
        print('hey')
      }
      
    }
  }
  
  # prob distribution
}

### Below are the functions called above ###
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

# finds euclidian distance between x rows and y row
# IDEA - if too many dimensions, find distance of t-0, then t-1, then t-2...lower dimensions
nn_finder <- function(x, y, nn){
  df <- rbind(y, x)
  df <- scale(df)
  y_dist <- as.matrix(dist(df))[,1]
  nn_cols <- as.integer(names(sort(y_dist)[2:(nn+1)]))-1
  return(nn_cols)
}

weather_prediction(data_fake, x=1:6, y=7, seas=4, year=1, lag=6, nn=30, norm=F, split=0.7)

