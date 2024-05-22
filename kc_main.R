  library(lars)
  library(dplyr)
  source("C:/Users/user00/Desktop/LuValle/SOI Rainfall and the solar cycle/datamake.pck")
  source("C:/Users/user00/Desktop/LuValle/SOI Rainfall and the solar cycle/maketable.pck")
  source("C:/Users/user00/Desktop/LuValle/SOI Rainfall and the solar cycle/reproduce.pck")
  
  ################################################################################
  # Function 1 - Creates the Delay Maps 
  
  DelayMapCreator <- function(df, lags, exclude.vars=NULL){
    df.lag <- df
    
    for(i in 1:lags){
      df.excl <- df[, !(names(df) %in% exclude.vars)]
      df.lag.n <- df.excl %>% lag(i)
      colnames(df.lag.n) <- paste0(colnames(df.lag.n), ".lag", i)
      df.lag <- cbind(df.lag, df.lag.n)
    }
    return(df.lag)
  }
  
  ################################################################################
  # Function 2 - Main Body of the Function
  
  # df, 6, c("season", "year"), "F_PRCP"
  
  WeatherPredFunc <- function(df, lags, non.rep.var, pred, year.start, year.forward, nn.amnt) {
    
  #### Part 1 - Setting up Delay Maps
    
    # - create the delaymap
    df.lag <- DelayMapCreator(df, lags, non.rep.var)
    df.lag <- df.lag %>% na.omit 
    
    # - relocate the untoached variables
    df.lag <- df.lag %>% relocate(non.rep.var, .after = last_col())
    df.lag <- df.lag %>% relocate(pred, .after = last_col())
    df.lag <- df.lag[,-c(1:(ncol(df)-1-length(non.rep.var)))] 
    
    # - split the test/train
    df.lag.train <- df.lag %>% filter(year < year.start) %>% arrange(year)
    df.lag.test <- df.lag %>% filter(year >= year.start) %>% arrange(year)
    
    # - calculate attractor dimension and create delay maps 
    dim <- dim.est.calc(df.lag.train)
    nsel<-ceiling(2.5*dim)
    delay.maps <- disjoint.delaymap.make1(30,(ncol(df.lag)-length(non.rep.var)-1),nsel)
    
  
  #### Part 2 - Looping to calculate the variables 
    
    dmp.year.list <- list()
    
    for(i in 1:year.forward){
      
      dmp.season.list <- list()

      for(j in c(4,1,2,3)){
        
        delay.map.pred.list <- list()
        
        for(k in 1:length(delay.maps)){
          
          df.lt.delay.train <- cbind(
            df.lag.train[, delay.maps[[k]]],
            df.lag.train[, (ncol(df.lag.train)-length(non.rep.var)):ncol(df.lag.train)]
          )
          
          df.lt.delay.test <- cbind(
            df.lag.test[, delay.maps[[k]]],
            df.lag.test[, (ncol(df.lag.test)-length(non.rep.var)):ncol(df.lag.test)]
          )
          
          pred <- WeatherPredLars(df.lt.delay.train, df.lt.delay.test, i, j, year.start, nn.amnt, non.rep.var)
          delay.map.pred.list <- append(delay.map.pred.list, pred)
        }
        
        #dmp.season.list <- append(dmp.season.list, as.list(delay.map.pred.list))
        dmp.season.list[[j]] <- delay.map.pred.list
        
      }
      
      dmp.year.list[[i]] <- dmp.season.list
      
    }
    
    resid.list.year <- list()
    
    for(i in 1:length(dmp.year.list)){
      
      resid.list.seas <- list()

      year.preds <- dmp.year.list[[i]] 
      
      for(j in c(4,1,2,3)){
        
        actual.val <- fresno_season %>% filter(year==(year.start+i-1), season==j) %>% select(F_PRCP)
        season.preds <- dmp.season.list[[j]]
        seasons.preds <- as.vector(unlist(season.preds, use.names=FALSE))
        seas.pred <- mean(na.omit(seasons.preds))

        resid <- actual.val - seas.pred
        resid.vec <- list(as.numeric(actual.val), as.numeric(seas.pred), as.numeric(resid), as.vector(seasons.preds))
        resid.list.seas[[j]] <- resid.vec

      }
      
      resid.list.year[[i]] <- resid.list.seas
      
    }
    
    return(resid.list.year)
    
  }
  
  ################################################################################
  # Function 3 - does the actual weather prediction
  
  WeatherPredLars <- function(df.lag.train, df.lag.test, year.loop, month.loop, year.start, nn.amnt, non.rep.var){
    
    #current date that is being predicted 
    df.pred <- df.lag.test %>% filter(year == year.start + year.loop - 1, season == month.loop)
    df.full <- rbind(df.lag.train, df.pred)
    
    df.full <- df.full[-c((ncol(df.lag.train)-length(non.rep.var)):(ncol(df.lag.train)-1))]
    
    #distance matrix
    DI <- diag(1/(apply(df.full[,-nrow(df.full)], 2, sd)))
    dist0 <- as.matrix(dist(as.matrix(df.full[,-nrow(df.full)])%*%as.matrix(DI)))
    dist0 <- (as.data.frame(dist0))[nrow(dist0), ]
    dist0.rank <- as.numeric(dist0)
    dist0.rank <- rank(dist0)
    
    #find obs with nearest neighbors
    dist0.rank.bind <- rbind(dist0, dist0.rank, c(1:ncol(dist0)))
    dist0.rank.bind <- dist0.rank.bind[,-ncol(dist0.rank.bind)]
    dist0.rank.bind <- dist0.rank.bind[3, dist0.rank.bind[2,] %in% c(1:(nn.amnt+1))]
    dist0.rank.bind <- as.numeric(as.data.frame(dist0.rank.bind))
    
    df.nn <- df.full[dist0.rank.bind, ]
    
    #make estimation
    x.mat.resid <- df.full[-nrow(df.full), -ncol(df.full)]
    y.mat.resid <- df.full[-nrow(df.full), ncol(df.full)]

    strloc <- lars(as.matrix(x.mat.resid), as.matrix(y.mat.resid))
    Iloc<-(strloc$Cp==min(strloc$Cp))
    s1loc<-(c(1:length(Iloc))[Iloc])[1]
    
    if(is.na(s1loc)){
      test.pred <- NA
      
    } else {
      
      #residuals on training data
      pred.mat.resid <- predict(strloc,x.mat.resid,s1loc)$fit
      resid.mat <- rbind(y.mat.resid, pred.mat.resid)
      resid.train <- sample(resid.mat[1,] - resid.mat[2,], 5)
      
      #test predictions
      x.mat <- df.pred[, 1:(ncol(df.pred)-1-length(non.rep.var))]
      test.pred <- predict(strloc, as.matrix(x.mat), s1loc)$fit
      test.pred <- test.pred + sample(resid.train, 5, replace=T)

    }
    
    return(test.pred)
  }
  
  ################################################################################
  # Non-function part of the code, where users can alter it
  
  weather.preds.result <- WeatherPredFunc(fresno_season, 6, c("season", "year"), "F_PRCP", 1990, 6, 30)
  # actual value, pred value, residual, all preds 
  
  
