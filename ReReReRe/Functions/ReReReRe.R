####WORKSPACE SETTING AND DATA INPUT####

library(dplyr)
library(magrittr)
library(lavaan)
library(psych)
library(mice)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ReReReRe <- function(data,
                    corThreshold=.70, # testa da .10 a .99
                    cutOff=0.95, # testa da .80 a .99
                    iterations=100, # nel test fissa a 1000
                    progress = F){ 
  
  #keep only numeric values
  data <- data %>%
    select(where(is.numeric))
  
  #computing correlations (absolute values)
  corMat <- cor(data,use="complete.obs") %>% abs
  
  #substiting upper triangle (diagonal included) with NAs
  corMat[upper.tri(corMat,diag = T)] <- NA
  
  #getting the row and col names with values higher than threshold
  coupples <- which(corMat>corThreshold,arr.ind = T)
  
  #getting the correlation for each individual (for cycle below)
  rowCors <- c()
  
  #progress update
  if (progress){
    cat("\n getting the correlation for each individual \n")
    pb <- txtProgressBar(min=0,max=1,style=3)
  }
  
  for (i in 1:nrow(data)){
    rowCors <- cor(
      data[i,coupples[,1]] %>% as.numeric,
      data[i,coupples[,2]] %>% as.numeric,
      use = "complete.obs"
      ) %>%
      append(rowCors,.)
    
    if (progress){
    #updating progress bar
    setTxtProgressBar(pb,i/nrow(data))
      }
    }
  
  #small function to transform the numbers to the smallest even number
  make_even <- function(x) {
    ifelse(x %% 2 == 0, x, x - 1)
  }
  
  #needed in for cycle below
  e <- 1
  all_RIC <- matrix(nrow=nrow(data),ncol=iterations)
  
  #progress update
  if (progress){
    cat("\n computing random correlations \n")
    pb <- txtProgressBar(min=0,max=1,style=3)
  }
  
  #computing random correlations
  for (i in 1:iterations){
    
    #Giving the columns a random order
    col_random_order <- sample(1:ncol(data),make_even(ncol(data)))
    
    #splitting the columns in two halves
    indexes_half1 <- col_random_order[1:(length(col_random_order)/2)]
    indexes_half2 <- col_random_order[(1+length(col_random_order)/2):length(col_random_order)]
    
    #computing the random individual correlation between the two halves
    RIC <- apply(data,
                 1,
                 \(x){
                   cor(x[indexes_half1] %>% as.numeric,
                       x[indexes_half2] %>% as.numeric,
                       use="pairwise.complete.obs") %>%
                     suppressWarnings()
                 })  
    
    #storing results inside a data.frame case x iteration
    all_RIC[,e] <- RIC %>% abs
    e <- e+1
    if (progress){setTxtProgressBar(pb,i/iterations)}
  }
  
  corComparedIndex <- c()
  
  for (i in 1:nrow(data)){
    corComparedIndex <- mean(all_RIC[i,]<rowCors[i]) %>%
      append(corComparedIndex,.)
  }
  
  if (progress){
    cat("\n FINISHED \n The index is based on",nrow(coupples),"coupples of items \n")
  }
  
  cbind.data.frame(
    result = corComparedIndex,
    indCors = rowCors, 
    flagged = corComparedIndex<=cutOff)%>%
  return
}

















