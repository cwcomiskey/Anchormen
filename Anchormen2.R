# Anchormen2

library(foreign)
library(dplyr)
library(plsgenomics)
library(tidyr)
library(neuralnet)
library(nnet)

bcam <- readRDS('bcam.rds')

# Response ==
Res <- bcam %>%
  select(class) # n x 1 -- factor

Res01 <- class.ind(Res$class) # n x 3 -- indicator

# Covariates ==
Covs <- bcam %>%
  select(-class)

table(sapply(Covs[1,], class)) 

  # 1: n x 3045 factors
  Covs3045 <- Covs %>%
    dplyr::select_if(is.factor)

  # 2: n x 9135 numeric
  Covs9135 <- Covs %>%
    dplyr::select_if(is.numeric)

  # 3: n x 9000 indicators
  Covs0101 <- Covs9135 %>%
    sapply(round) %>% 
    as.data.frame() %>%
    setNames(names(Covs9135)) 

# multinom.spls() =====
# Res, Covs9135
dat <- cbind.data.frame(Res, Covs9135) %>%
    drop_na() %>%
    mutate(class = as.numeric(class) - 1)
samp <- sample(1:155, 140)

train <- dat[samp,] 
  Xtrain <- train %>%
    select(-class) %>%
    as.matrix()
  Ytrain <- train %>%
    select(class) %>%
    as.matrix()

test <- dat[-samp,]
  Xtest <- test %>%
    select(-class) %>%
    as.matrix()
  Ytest <- test %>%
    select(class) %>%
    as.matrix()
  
fit <- multinom.spls(Xtrain = Xtrain, Ytrain = Ytrain, ncomp = 5,
                     lambda.ridge = 2, lambda.l1 = 0.5, adapt = TRUE,
                     maxIter = 25, svd.decompose = TRUE, Xtest = Xtest)
fit$converged # [1] 1 Bingo.
sum(fit$hatYtest == Ytest) / length(Ytest) 
