# Weather Prediction by Luke Beebe

library(lars)
library(plyr)

# main
weather_prediction <- function(data, x, y, split=36, seas, year, lag=6, dim=7, nn=30, ntrial=1200, sd=1){
  year <- year+1
  df <- data[, append(x, y)]
  y <- ncol(df)
  rownames(df) <- seq(1, nrow(df), 1) %% 4
  df <- delay_map(df, lag*max(year)+1)
  
  # split data into past/future 9 years
  split <- nrow(df)-split
  df_train <- df[1:split,]
  df_test <- df[(split+1):nrow(df),]
  
  # calculate attractor dimension, runs on LuValle's code
  # dim = 7
  # dim <- 2.5*dim.est.calc(rev(df_test))
  # print(dim/2.5)
  
  # set window
  par(mfrow=c(length(seas),length(year)*2))
  ypreds <- resids <- paths <- betas <- NULL
  
  for(i in seas){
    # separate seasons LOOK OVER SEASONS COMPARED TO DF
    season_train <- df_train[rownames(df_train)==as.character(i%%4), ]
    season_test <- df_test[rownames(df_test)==as.character(i%%4), ]
    sd_adj <- sqrt(var(season_train[,y]))*sd
    
    for(j in year){
      # random samples to pull from columns, set years of delay map info
      remove_col <- (length(x)+length(y)) * (j-1) * 4
      print(remove_col)
      sample_cols <- delay_sample(ntrial, ((remove_col+1):((length(x)+length(y))*lag+remove_col)), dim)
      
      # distance matrix
      "nn_rows <- nn_finder(season_train[,-(1:(remove_col*j))], season_test[j,-(1:(remove_col*j))], nn)
      nn_train <- season_train[nn_rows,]"
      
      y_test <- as.matrix(season_test[j, y])
      
      for(k in 1:length(sample_cols)){
        nn_rows <- nn_finder(season_train[,sample_cols[[k]]], season_test[j,sample_cols[[k]]], nn)
        nn_train <- season_train[nn_rows,]
        
        X <- scale(nn_train[, sample_cols[[k]]]) ### SCALE COMPONENT
        Y <- nn_train[, y]
        lars_output <- lars(X, Y)
        model_col <- match(TRUE, lars_output$Cp==min(lars_output$Cp))
        if(is.na(model_col)){
          # PRINCIPLE COMPONENTS
          # OR INVESTIGATE lars_output$Cp and use output that isn't min()
        }
        else{
          beta <- lars_output$beta[model_col,]
          ypred <- predict(lars_output, X[1:5,])$fit[, model_col]
        }
        ypreds <- append(ypreds, ypred)
        resid <- Y[1:5] - ypred
        resids <- rbind(resids, resid)
        paths <- rbind(paths, sample_cols[[k]])
        betas <- rbind(betas, beta)
      }
      
      resids_paths <- cbind(resids,  paths)
      rownames(resids_paths) <- sig_paths <- rp_indexes <- sig_betas <- colnames(betas) <- rownames(betas) <- NULL
    
      for(l in 1:5){ # 5 predictions, saves for each one that is correct
        rp_index <- resids_paths[,l]>(-1)*sd_adj & resids_paths[,l]<sd_adj
        sig_path <- resids_paths[rp_index, -c(1:5)]
        sig_beta <- betas[rp_index,]
        sig_paths <- append(sig_paths, sig_path)
        sig_betas <- append(sig_betas, sig_beta)
        rp_indexes <- append(rp_indexes, rp_index)
        # get beta to sig_paths, rp_index =2500, dim(betas)=2500x7
      }

      beta_path <- data.frame(sig_paths, sig_betas)
      path_beta_df <- NULL
      
      for(num in sort(unique(sig_paths))){
        beta_sum <- sum(abs(beta_path$sig_betas[beta_path$sig_paths==num]))
        path_beta_df <- rbind(path_beta_df, data.frame(num, beta_sum))
      }
      
      path_beta_df <- path_beta_df[order(path_beta_df$beta_sum, decreasing=T),]
      
      prob <- sum(rp_indexes==T)/length(rp_indexes)
      print(paste("Probability:", prob))
      
      sig_mean <- mean(path_beta_df$beta_sum)
      
      print("Significant Variable, Index")
      print(paste(colnames(nn_train)[path_beta_df$num[1:10]], path_beta_df$num[1:10], sep=", #"))
      
      plot(path_beta_df$num, path_beta_df$beta_sum, type='h', ylab='beta_sum', xlab='column index', col=)
      abline(h=sig_mean, col='red')
      
      # MAKE BARS INDIVIDUAL, SHOW MEAN VALUE AS RED LINE ACCROSS, ONLY SHOW SIG VALUES
      
      # xlab=paste("Pr =", sum(rp_index)/length(rp_index)) Calculate Probability?
      resids <- na.omit(resids)
      ypreds <- na.omit(ypreds)
      preds <- sample(ypreds, 6000, replace=T) + sample(resids, 6000, replace=T)
      # print(paste("NA values:", (5*1200-length(resids))/5))
      
      den <- density(preds)
      plot(den, frame=T, col='blue', main=paste(j-1,i))
      abline(v=y_test, col='red')
      abline(v=y_test+sd_adj, col='grey', lty='dashed')
      abline(v=y_test-sd_adj, col='grey', lty='dashed')
      
      ypreds <- resids <- paths <- betas <- rp_indexes <- NULL

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
delay_sample <- function(ntrial, col, nsel){
  nums <- list()
  for(i in 1:ntrial){
    sample <- sample(col, nsel)
    nums[[i]] <- sample
  }
  return(nums)
}

# finds euclidian distance between x rows and y row. returns 5 closest, then 6 to 30 closest
nn_finder <- function(x, y, nn){
  df <- rbind(y, x)
  df <- scale(df)
  dist_order <- order(as.matrix(dist(df))[,1])
  nn_cols <- (match(2:6, dist_order)-1)
  nn_cols <- append(nn_cols, (match(7:31, dist_order)-1))
  return(nn_cols)
}

weather_prediction(as.matrix(fresno_season), x=c(3:9), y=10,
                   seas=c(1), year=c(1,2,3),
                   lag=6, nn=30, dim=7, ntrial=1200,
                   sd=0.5)

