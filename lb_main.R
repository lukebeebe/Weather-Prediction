# Weather Prediction by Luke, Kavi
library(lars)

# NORMALIZE DATA PER SEASON

# main
weather_prediction <- function(data, x, y, split=36, seas, year, lag=6, nn, sd=1){
  year <- year+1
  df <- data[, append(x, y)]
  y <- ncol(df)
  rownames(df) <- seq(1, nrow(df), 1) %% 4
  df <- delay_map(df, lag)
  
  # split data into past/future 9 years
  split <- nrow(df)-split
  df_train <- df[1:split,]
  df_test <- df[(split+1):nrow(df),]
  
  # calculate attractor dimension, runs on LuValle's code
  dim = 7
  # dim <- 2.5*dim.est.calc(rev(df_test))
  # print(dim/2.5)
  
  # random samples to pull from columns
  set.seed(12345)
  sample_cols <- delay_sample(ntrial=1200, (ncol(df_train)-1), dim)
  par(mfrow=c(length(seas),length(year)))
  ypreds <- resids <- paths <- NULL
  
  for(i in seas){
    # separate seasons LOOK OVER SEASONS COMPARED TO DF
    season_train <- df_train[rownames(df_train)==as.character(i%%4), ]
    season_test <- df_test[rownames(df_test)==as.character(i%%4), ]
    
    for(j in year){
      # distance matrix
      remove_col <- length(x) + length(y)
      nn_rows <- nn_finder(season_train[,-(1:(remove_col*j))], season_test[j,-(1:(remove_col*j))], nn)
      nn_train <- season_train[nn_rows,]
      y_test <- as.matrix(season_test[j, y])
      
      for(k in 1:length(sample_cols)){
        X <- nn_train[, sample_cols[[k]]]
        Y <- nn_train[, y]
        ### 
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
      plot(den, frame=T, col='blue', main=paste(j-1,i))
      abline(v=y_test, col='red')
      abline(v=y_test+stats$sd[i]*sd, col='grey', lty='dashed')
      abline(v=y_test-stats$sd[i]*sd, col='grey', lty='dashed')
      
      ypreds <- resids <- paths <- NULL
      
      # OUTPUT LIST
    }
  }
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

# finds euclidian distance between x rows and y row. returns 5 closest, then 6 to 30 closest
nn_finder <- function(x, y, nn){
  df <- rbind(y, x)
  df <- scale(df)
  dist_order <- order(as.matrix(dist(df))[,1])
  print(head(sort(as.matrix(dist(df))[,1])))
  nn_cols <- (match(2:6, dist_order)-1)
  nn_cols <- append(nn_cols, (match(7:31, dist_order)-1))
  return(nn_cols)
}

head(fresno_season)
weather_prediction(as.matrix(fresno_season), x=c(3:9), y=10, seas=c(1,2,3,4), year=c(0,1), lag=6, nn=30, sd=0.5)
 