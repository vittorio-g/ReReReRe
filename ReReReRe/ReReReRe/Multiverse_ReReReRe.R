#### Workspace Setting ####

library(multiverse)
library(dplyr)
library(purrr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#loading functions
source("Functions/Careless_machine_2.R")
source("Functions/A_good_careless_dataset.R")
source("Functions/ReReReRe.R")


#loading dataset
spi <- data(spi)

#### Creating Careless Dataset ####

M <- multiverse()

inside(M,{
  corruption <- careless_corruption(dat=spi, 
                                    pct_careless = branch(pct_careless, 
                                                           ".05" ~ .05,
                                                           .options = seq(.1,.5,.05)) ,
                                    pct_types = c(random = 1/3, longstring = 1/3, mixed = 1/3),
                                    careless_levels = c(1,.9,.8,.7,.6,.5))
  
  #The corrupted spi dataset
  corruptSpi <- corruption[[1]]
  
  #The careless labels
  carelessLabels <- corruption[[2]]
  
  
  flagged <- ReReReRe(
    corruptSpi,
    corThreshold = branch(
      corThreshold,
      ".40" ~ .40,
      .options = seq(.45,.80,.05)
    ),
    cutOff = branch(
      cutOff,
      ".80" ~ .80,
      .options = seq(.81,.99,0.01)
    ),
    iterations = 100
  ) %$%
    flagged
  
  
  
  })









#### Comandi utili ####

M %>%
  expand()
  
M %>%
  size

M %>%
  parameters

M %>%
  conditions

M %>%
  print

execute_multiverse(M, progress = T)


M$corruptSpi

extract_variable_from_universe(M, idx = 2, name = "corruptSpi")

a <- extract_variables(M,corruptSpi)

a$corruptSpi[[1]]





