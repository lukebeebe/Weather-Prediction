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
  delay_sample <- delay_sample(1200, ncol(df_train), 7) # SAMPLE FROM nn_cols BEFORE LARS?
  
  # seperate seasons
  season_train <- df_train[seq(seas, nrow(df_train), 4), ]
  season_test <- df_test[seq(seas, nrow(df_train), 4), ]
  
  # distance matrix
  nn_cols <- dist_matrix(season_train, season_test, year)
  
  # lars
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

dist_matrix <- function(x, y, year, nn=30){
  x <- scale(x)
  y <- scale(y)
  y <- y[year,]
  df <- rbind(y, x)
  y_dist <- as.matrix(dist(df))[,1]
  nn_cols <- order(y_dist)[2:nn]
  return(nn_cols)
}

weather_prediction(data_fake, x=1:6, y=7, seas=4, year=1, lag=6, norm=F, split=0.7)

