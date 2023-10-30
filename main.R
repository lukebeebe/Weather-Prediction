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
  dim <- dim.est.calc(rev(df_test))
  print(dim)
  
  nsel <- ceiling(2.5*dim)
  delay_maps <- disjoint.delaymap.make1(1200, ncol(df_train), nsel)
  
  
  # seperate seasons
  # turn nums into values for next step <- write into random_nums function?
  # nearest neighbor, lars
  # prob distribution
}

# makes delay map
delay_map <- function(df, lag){
  delay <- df
  names <- colnames(delay)
  for(i in 1:lag){
    colnames(delay) <- paste(as.character(i), names, sep = "")
    delay <- rbind(0, delay)
    delay <- delay[-nrow(delay),]
    df <- cbind(df, delay)
  }
  df <- df[lag+1:(nrow(df)-lag), ]
  return(df)
}

<- function(ntrial, ncol, nsel){
  for(i in 1:ntrial){ #gives ntrial samples
    nums <- sample(2:ncol, replace=F)
    for(j in 1:nmid-1){
      selvec <- (j-1)*nsel
    }
  }
}

weather_prediction(data_fake, x=1:6, y=7, seas=4, year=1, lag=6, norm=F, split=0.7)
