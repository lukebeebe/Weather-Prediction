# Weather Prediction by Luke, Kavi

# main
weatherPrediction <- function(df, x, y, seas, year, lag=6, norm=T, split=0.7){
  df <- df[, append(x, y)]
  if(norm==T){df <- scale(df[, x])}
  df.split <- trainTest(df, split)
  df.split$x.train <- delayMap(df.split$x.train, lag)
  # pick RVs from delay map, num = length(x)?
  # nearest neighbor, lars
  # prob distribution
}

# splits data
trainTest <- function(df, split){
  sample <- sample(c(TRUE, FALSE), nrow(df), replace=F, prob=c(split,1-split))
  y <- ncol(df)
  x.train <- df[sample, -y]
  y.train <- df[sample, y]
  x.test <- df[!sample, -y]
  y.test <- df[!sample, y]
  my_list <- list(x.train=x.train, y.train=y.train, x.test=x.test, y.test=y.test)
  return(my_list)
}

# makes delay map
delayMap <- function(df, lag){
  delay <- df
  for(i in 1:lag){
    delay <- rbind(0, delay)
    df <- cbind(df, delay)
  }
  df <- df[lag:(nrow(df)-lag), ]
  return(df)
}
