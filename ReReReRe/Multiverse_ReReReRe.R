#### Workspace Setting ####
rm(list=ls())
library(multiverse)
library(dplyr)
library(purrr)
library(tidyverse)
library(magrittr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#loading functions
source("Functions/Careless_machine_2.R")
source("Functions/A_good_careless_dataset.R")
source("Functions/ReReReRe.R")


library(psychTools)

#loading dataset
data("spi")
get("spi")

#### Creating Careless Dataset ####

M <- multiverse()

#parameters of the multiverse

options_pct_careless <- seq(.1,.5,.05)
options_corProp <- c(.02, .05, .1, .15, .20, .25, .30)
options_cutOff <- seq(.81,.99,0.01)

# options_pct_careless <- seq(.1,.2,.05)
# options_corThreshold <- seq(.45,.55,.05)
# options_cutOff <- seq(.81,.83,0.01)

inside(M,{
  corruption <- careless_corruption(dat=spi, 
                                    pct_careless = branch(pct_careless, 
                                                           ".05" ~ .05,
                                                           .options = options_pct_careless) ,
                                    pct_types = c(random = 1/3, longstring = 1/3, mixed = 1/3),
                                    careless_levels = c(1,.9,.8,.7,.6,.5))
  
  #The corrupted spi dataset
  corruptSpi <- corruption[[1]]
  
  #The careless labels
  carelessLabels <- corruption[[2]]
  
  
  flagged <- ReReReRe(
    corruptSpi,
    corProp = branch(
      corProp,
      ".01" ~ .01,
      .options = options_corProp
    ),
    cutOff = branch(
      cutOff,
      ".80" ~ .80,
      .options = options_cutOff
    ),
    iterations = 100
  ) %$%
    flagged
  
  
  
  })


execute_multiverse(M, progress = T) %>% suppressWarnings

a <- extract_variables(M, flagged, carelessLabels)






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

execute_multiverse(M, progress = T) %>% suppressWarnings


M$corruptSpi

extract_variable_from_universe(M, idx = 2, name = "corruptSpi")

a <- extract_variables(M,flagged)

a$corruptSpi[[1]]





