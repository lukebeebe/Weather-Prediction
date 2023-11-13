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
  delay_maps<-disjoint.delaymap.make2(15,42,nsel)

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
  
  #conduct the regression 
  lars_pred_list <- cp.lars.reg(df_train, df_test, delay_maps, 30, 30)
  
  #get residuals for each neighborhood within each delay map
  df_test_outofsample <- df_test[((nrow(df_train)+1):nrow(df_test)),]

  #residlist <- resid.list(df_train, df_test_outofsample, lars_pred_list, delay_maps)
  
  return(list(df_train = df_train, df_test = df_test, delaymaps = delay_maps, lars_pred_list=lars_pred_list))
}


######################################################################################################
#delay.map.creator
#creates a specified amount of lags for each variable and assigns a appropriate var name

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


######################################################################################################
#disjoint.delaymap.make2

#code is the same as Dr. LuValle's

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


######################################################################################################
#cp.lars.reg

cp.lars.reg <- function(df_train, df_test, delay_maps, nn_amount, delaymap_num){
  
  ### Find nearest neighbors:
  nnvec_mat <- nn.finder(df_train, nn_amount)
  print(nnvec_mat)
  
  
  #### Find each path in the LARs Regression
  delay_map_paths_list <- list()
  
  for(i in 1:delaymap_num){
    df_train_y <- df_train[,1]
    df_train_x <- df_train[, -1]
    df_train_x_reg <- df_train_x[, delay_maps[[i]]-1]
    
    output <- lars.reg(df_train_x_reg, df_train_y, nnvec_mat)
    delay_map_paths_list <- append(delay_map_paths_list, list(output))
  }
  
  return(list(delay_map_paths_list))
}


######################################################################################################
#nn.finder

nn.finder <- function(df, nn_amount){
  
  #Divide x vars by sd and create distance matrix
  DI <- diag(1/(apply(df, 2, sd)))
  dist0 <- as.matrix(dist(as.matrix(df)%*%as.matrix(DI)))
  
  #create distance mat
  nnvec_mat <- matrix(, nrow=nrow(dist0), ncol=nn_amount)
  
  #for each obs, find the 30 nearest neighbors (smallest dist in dist matrix)
  for(i in 1:nrow(dist0)){
    nnvec <- c()
    minid <- 0
    
    #loop for the size of the neighborhood (1:30 nearest neighbors, for example)
    for(j in 1:nn_amount){
      mindist <- .Machine$integer.max
      
      #find obs with lowest distance, if not already in the vector of nearest neighbors (nnvec)
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
  
  return(nnvec_mat)
}


######################################################################################################
#lars.reg

lars.reg <- function(df_train_x, df_train_y, nnvec_mat){
  #using df_train with only variables of a certain delay map selected
  #for each set of nearest neighbors, create a model
  
  lars_model_list <- list()
  
  for(i in 1:nrow(df_train_x)){
    #take only nearest neighbor rows in training data
    df_train_x_reg <- as.matrix(df_train_x[nnvec_mat[i,],])
    df_train_y_reg <- as.matrix(df_train_y[nnvec_mat[i,]])
    
    #lars regression
    strloc <- lars(df_train_x_reg, df_train_y_reg)
    Iloc<-(strloc$Cp==min(strloc$Cp))
    s1loc<-(c(1:length(Iloc))[Iloc])[1]
    
    #predictions w/ training data using the model on the nearest neighborhood 
    df_train_x_reg_resid <- as.matrix(df_train_x[nnvec_mat[i,],])
    ypredfull <- predict(strloc,df_train_x_reg_resid,s1loc)$fit
    
    #residuals for top n nearest neighbors in neighhborhood
    df_train_x_reg_resid <- as.matrix(df_train_x[nnvec_mat[i,1:5],])
    ypred <- predict(strloc,df_train_x_reg_resid,s1loc)$fit
    resid <- df_train_y[nnvec_mat[i,1:5]] - ypred

    #return list of model outputs
    lars_mod <- list(strloc, s1loc, ypredfull, resid)
    lars_model_list <- append(lars_model_list, list(lars_mod))
    
  }
  return(lars_model_list)
}

######################################################################################################
#resid.list
#this function is not utilized as of right now, all residual calculations are in the previous function

resid.list <- function(df_train, df_test, chaos_model_list, delay_maps){
  
  resid_list <- list()
  
  for(i in 1:length(chaos_model_list[[1]])){
    xmat_fit <- df_test[, delay_maps[[i]]]
    delaymap_models <- chaos_model_list[[1]][[i]]

    delaymap_residlist <- list()
    
    for(j in 1:length(delaymap_models)){
       model_list <- delaymap_models[[j]]
       model <- model_list[[1]]
       bestmodel <- model_list[[2]]

       ypred <- predict(model,xmat_fit,bestmodel)$fit
       resid <- df_test[1] - ypred
    
       delaymap_residlist <- append(delaymap_residlist, list(resid))
    }
    resid_list <- append(resid_list, list(delaymap_residlist))
  }
  
  return(resid_list)
}

######################################################################################################



################################################################################
# run code below 
################################################################################
output <- weather_prediction(data_fake, x=2:7, y=1, seas=1, year=1, lag=6, norm=F, split=0.7)

#WIP - Histogram of nonparametric Density Function  
model <- output[[4]][[1]] 
density_vec <- c()

for(i in 1:length(model)){
  print(i)
  
  #extract models for jth delay map
    model_delaymap <- model[[i]]
    
    #loop through each neighborhood
    for(j in 1:length(model_delaymap)){
      model_nnhood <- model_delaymap[[j]]
      
      model_pred <- model_nnhood[[3]]   #predictions using model for specific delaymap/ neighborhood
      model_resid <- model_nnhood[[4]]  #residuals of the specified model
      
      model_predresid <- model_pred + sample(model_resid, 1, replace=T)
      density_vec <- c(density_vec,model_predresid)

    }
}

hist(density_vec)
  


