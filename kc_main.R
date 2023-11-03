#data_fake <- data.frame(replicate(7,sample(0:5,1000,rep=TRUE)))
data_fake <- as.data.frame(F.nat0full0[,c(1,3:7)])
source("C:/Users/user00/Desktop/LuValle/SOI Rainfall and the solar cycle/datamake.pck")
source("C:/Users/user00/Desktop/LuValle/SOI Rainfall and the solar cycle/maketable.pck")
source("C:/Users/user00/Desktop/LuValle/SOI Rainfall and the solar cycle/reproduce.pck")
library(dplyr)
library(lars)

# main
weather_prediction <- function(df, x, y, seas, year, lags=6, norm=F, split=0.7){

  df_lag <- delay.map.creator(df, lags)
  df_lag_omit <- na.omit(df_lag)
  # seperate seasons
  # turn nums into values for next step <- write into random_nums function?
  # nearest neighbor, lars
  # prob distribution
  
  #training and test data
  oos <- floor(nrow(df_lag_omit)-(split*nrow(df_lag_omit)))
  df_train <- df_lag_omit[-c( (nrow(df_lag_omit)-oos):nrow(df_lag_omit)) ,]
  df_test <- df_lag_omit
  
  #calculate attractor dimension
  dim <- dim.est.calc(rev(df_test))
  print(dim)
  
  #get delay maps 
  nsel<-ceiling(2.5*dim)
  delay_maps<-disjoint.delaymap.make2(1200,42,nsel)
  
  #determines which season is the first obs given amount of lags
  season <- case_when(
    (lags - 1)/4==1 ~ '1',
    (lags - 2)/4==1 ~ '2',
    (lags - 3)/4==1 ~ '3',
    TRUE ~ '4')
  
  season1df_test <- df_test[seq(1, nrow(df), 4), ]
  season2df_test <- df_test[seq(2, nrow(df), 4), ]
  season3df_test <- df_test[seq(3, nrow(df), 4), ]
  season4df_test <- df_test[seq(4, nrow(df), 4), ]
  
  season1df_train <- df_train[seq(1, nrow(df), 4), ]
  season2df_train <- df_train[seq(2, nrow(df), 4), ]
  season3df_train <- df_train[seq(3, nrow(df), 4), ]
  season4df_train <- df_train[seq(4, nrow(df), 4), ]
  
  return(list(df_train = df_train, df_test = df_test, delaymaps = delay_maps))
}

delay.map.creator <- function(df, lags){
  df_lagged <- df
  count=ncol(df)
  for(i in 1:lags){
    for(j in 1:ncol(df)){
      count = count+1
      df_col <- df[,j]
      df_lagcol <- lag(df_col, i)
      print(names(df[j]))
      name = paste(names(df[j]),i,sep='')
      df_lagged <- cbind(df_lagged, df_lagcol)
      names(df_lagged)[count] = as.character(name)
    }
  }
  return(df_lagged)
}

disjoint.delaymap.make2 <-
  function(ntrial, ncol, nsel)
  {
    nmod<-ntrial*ceiling(ncol/nsel)
    nmid<-ceiling(ncol/nsel)
    outlist <- rep(list(), nmod)
    k<-1
    for(i in 1:ntrial) {
      dum <- sample(c(2:ncol),  replace = F)
      dum0<-dum
      for(j in 1:nmid){
        if(j<nmid){
          selvec<-((j-1)*nsel)+c(1:nsel)
          outlist[[k]] <- dum[selvec]
          dum0<-dum0[-c(1:nsel)]
          k<-k+1
        }
        else{
          if(length(dum0)<nsel){
            nplus<-nsel-length(dum0)
            
            dum0<-c(dum0,sample(c(1:ncol)[-dum0],nplus,replace=F))
          }
          outlist[[k]]<-dum0
          k<-k+1
        }
      }
    }
    outlist
  }

#Questions for LuValle:
  #1.) is the nearest neighbors generated on the test data? does this make sense
      # verify each model generated using training data with diff combos of vars and nearest neighbors on test
      # data
  #2.) how are nearest neighbors found? euclidean distance in dist matrix?
  #3.) why the 1/sqrt(sd) weight to data. should it just be sd?
  #4.) how are residuals calculated?
       # - y g
  #5.) how to store lists of nums

output <- weather_prediction(data_fake, x=2:7, y=1, seas=1, year=1, lag=6, norm=F, split=0.7)

#
#obj_train <- output[[1]]
#obj_test <- output[[2]]
#obj_pred1 <- obj_train[,-1]
#obj_pred2 <- obj_test[,-1]

#as.matrix(obj_pred1) %*% as.matrix(DI)

#DI <- diag(1/sqrt(apply(obj_pred1,2,sd)))
#sample distance matrix
#dist0<-as.matrix(dist(rbind(as.matrix(obj_pred2)%*%as.matrix(DI),as.matrix(obj_pred1)%*%as.matrix(DI))))

obj_train <- output[[1]]
DI <- diag(1/sqrt(apply(obj_train, 2, sd)))
dist0 <- as.matrix(dist(as.matrix(obj_train)%*%as.matrix(DI)))

nearest_neighbor_amount <- 30
nnvec_mat <- matrix(, nrow=nrow(dist0), ncol=nearest_neighbor_amount)

for(i in 1:nrow(dist0)){
  nnvec <- c()
  minid <- 0

  for(j in 1:nearest_neighbor_amount){
    mindist <- .Machine$integer.max

    for(k in 1:ncol(dist0)){
      if(k!=i){
        if(!(k %in% nnvec)==TRUE){

          if(mindist > dist0[i, k]){
            
            mindist = dist0[i, k]
            minid <- k
          }
        }
      }
    }
    nnvec <- append(nnvec, minid)
  }
  nnvec_mat[i,] <- nnvec
}
nnvec_mat <- cbind(1:nrow(nnvec_mat), nnvec_mat)

#testing lasso func
df_train_y <- obj_train[,1]
df_train_x <- obj_train[,-1]

df_train_x_reg <- df_train_x[, delay_maps[[1]]-1]

df_train_x_reg <- as.matrix(df_train_x_reg[nnvec_mat[1,],])
df_train_y_reg <- df_train_y[nnvec_mat[i,]]
strloc <- lars(df_train_x_reg,df_train_y_reg)
summary(strloc)




#running lasso regression
for(i in 1:1){
  
  df_train_reg <- obj_train[,c(1,delay_maps[[i]])]
  
  for(j in 1:nrow(nnvec_mat)){
    
    nnvec_reg <- nnvec_mat[1,]
    df_train_reg <- df_train_reg[nnvec_reg, ]
    
  }
}


#output <- delay_map_creator(data_fake, lags=6)
#output <- rev(output)
#output <- na.omit(output)
#dim.est.calc(output)

