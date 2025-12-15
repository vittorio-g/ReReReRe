####WORKSPACE SETTING AND DATA INPUT####

library(dplyr)
library(magrittr)
library(lavaan)
library(psych)
library(mice)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ReReReRe <- function(data, #any dataset with questionnaire data
                    corProp=0.05, # the proportion of highest correlations to use
                    cutOff=0.99, # the severity of the evaluation 
                    iterations=100){

  #keep only numeric values
  data <- data %>%
    select(where(is.numeric))
  
  #computing correlations (absolute values)
  corMat <- cor(data,use="pairwise.complete.obs") %>% abs
  
  #substiting upper triangle (diagonal included) with NAs
  corMat[upper.tri(corMat,diag = T)] <- NA
  
  #getting the threshold correlation based on the proportion of highest correlation we decided to include.
  corThreshold <- quantile(corMat,1-corProp, na.rm=T)
  k <- (corMat>=corThreshold) %>% sum(na.rm=T)
  
  #getting the row and col names with values higher than threshold
  couples <- which(corMat>=corThreshold, arr.ind = T)
  
  #getting the correlation for each individual (for cycle below)
  rowCors <- rep(NA,nrow(data))
  
  for (i in 1:nrow(data)){
    rowCors[i] <-  tryCatch( 
      cor(
        data[i,couples[,1]] %>% as.numeric,
        data[i,couples[,2]] %>% as.numeric,
        use = "pairwise.complete.obs"
        ) %>% abs,
      error = function(e) return(0) # here for variance zero we return 0 so that longstring will be more easily spotted
    )
  }
  

  
  #needed in for cycle below
  e <- 1
  all_RIC <- matrix(nrow=nrow(data),ncol=iterations)
  
  #all possible combinations
  combinations <- combn(names(data),2) %>% t
  
  
  #computing random correlations
  for (i in 1:iterations){
    
    #Sampling k*2 random columns from the dataset so that they are the same
    # number of columns as the ordered correlation and the variance of the estimate will be the same
    sampled <- sample(1:nrow(combinations),k)
    col_random_order <- combinations[sampled,]
    RIC <- rep(NA,nrow(data))
    
    #computing the random individual correlation between the two halves
    for (j in 1:nrow(data)){
      RIC[j] <- tryCatch(
                   cor(data[j,col_random_order[,1]] %>% as.numeric,
                       data[j,col_random_order[,2]] %>% as.numeric,
                       use="pairwise.complete.obs"),
                   error = function(e) return(1) #here for variance 0 we return 1 so that longstring will be more easily spotted
                 )
}
    
    #storing results inside a data.frame case x iteration
    all_RIC[,e] <- RIC %>% abs
    e <- e+1
  }
  
  corComparedIndex <- rep(NA,nrow(data))
  
  corComparedIndex <- rowMeans(all_RIC < rowCors, na.rm=T)
  
  cbind.data.frame(
    result = corComparedIndex,
    indCors = rowCors, 
    flagged = corComparedIndex<=cutOff)%>%
  return
}

