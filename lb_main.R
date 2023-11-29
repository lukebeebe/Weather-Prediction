# Weather Prediction by Luke, Kavi
library(lars)

# NORMALIZE DATA PER SEASON
# MAKE DELAY MAP CORRECT

# main
weather_prediction <- function(data, x, y, seas, year, lag=6, nn, norm=F){
  df <- data[, append(x, y)]
  rownames(df) <- seq(1, nrow(df), 1)
  # if(norm==T){df <- scale(df[, x])} MOVE THIS TO RESPECTIVE SEASONS
  df <- delay_map(df, lag)

  # split data into past/future 9 seasons
  split <- nrow(df)-9
  df_train <- df[1:split,]
  df_test <- df[(split+1):nrow(df),]
  
  # calculate attractor dimension, runs on LuValle's code
  dim = 7
  # dim <- 2.5*dim.est.calc(rev(df_test))
  # print(dim/2.5)
  
  # random samples to pull from columns
  sample_cols <- delay_sample(ntrial=1200, (ncol(df_train)-1), dim)
  par(mfrow=c(length(seas),length(year)))
  ypreds <- resids <- paths <- NULL
  
  for(i in seas){
    # seperate seasons
    season_train <- df_train[seq(i, nrow(df_train), 4), ]
    season_test <- df_test[seq(i, nrow(df_test), 4), ]
    for(j in year){
      # distance matrix
      remove_col <- length(x) + length(y)
      nn_rows <- nn_finder(season_train[,-(1:(remove_col*j))], season_test[j,-(1:(remove_col*j))], nn)
      nn_train <- season_train[nn_rows,]
      y_test <- as.matrix(season_test[j, y])
      for(k in 1:length(sample_cols)){
        X <- nn_train[, sample_cols[[k]]]
        Y <- nn_train[, y]
        lars_output <- lars(X, Y)
        model_col <- match(TRUE, lars_output$Cp==min(lars_output$Cp))
        if(is.na(model_col)){
          #TROUBLESHOOT NA VALUES
          next
        }
        else{
          ypred <- predict(lars_output, X[1:5,])$fit[, model_col]
        }
        ypreds <- append(ypreds, ypred)
        resid <- Y[1:5] - ypred
        resids <- append(resids, resid)
        paths <- append(paths, sample_cols[[k]])
      }
      resids <- na.omit(resids)
      ypreds <- na.omit(ypreds)
      preds <- sample(ypreds, 6000, replace=T) + sample(resids, 6000, replace=T)
      print(paste("NA values:", (5*1200-length(resids))/5))
      den <- density(preds)
      plot(den, frame=T, col='blue',main=paste(j,i), xlim=c(-3,3))
      abline(v=y_test, col='red')
      ypreds <- resids <- paths <- NULL
    }
  }
  
  # prob distribution
}

### Below are the functions called above ###
# makes delay map
delay_map <- function(df, lag){
  delay <- df
  names <- colnames(delay)
  for(i in 1:lag){
    colnames(delay) <- paste(names, as.character(-i), sep = " t")
    delay <- rbind(0, delay)
    delay <- delay[-nrow(delay),]
    df <- cbind(df, delay)
  }
  df <- df[lag+1:(nrow(df)-lag), ]
  print(head(df))
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
  dist_order <- order(as.matrix(dist(df))[,1])
  nn_cols <- (match(2:6, dist_order)-1)
  nn_cols <- append(nn_cols, (match(7:31, dist_order)-1))
  return(nn_cols)
}

weather_prediction(hawaii_data, x=2:7, y=1, seas=c(1,2,3,4), year=c(1,2), lag=6, nn=30, norm=F)

