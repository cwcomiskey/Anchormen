# Packages ====
library(foreign)
library(dplyr)
library(plsgenomics)
library(tidyr)
library(randomForest)
library(neuralnet)
library(nnet)
library(brnn)
library(kernlab)

# Data ====
bcam <- readRDS('bcam.rds') %>%
  drop_na() 

# Response dataframes ====
Res <- bcam %>%
  select(class) # n x 1 -- factor

Res01 <- class.ind(Res$class) # n x 3 -- indicator

# Covariate data frames ====
Covs <- bcam %>%
  select(-class)

# table(sapply(Covs[1,], class)) 

# 1: n x 3045 factors
Covs3045 <- Covs %>%
  dplyr::select_if(is.factor)

# 2: n x 9135 numeric
Covs9135 <- Covs %>%
  dplyr::select_if(is.numeric)

# 3: n x 9135 indicators
Covs01 <- Covs9135 %>%
  sapply(round) %>% 
  as.data.frame() %>%
  setNames(names(Covs9135)) 