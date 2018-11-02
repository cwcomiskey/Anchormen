# Load/install packages
# install.packages("foreign")
library(foreign)
library(dplyr)
library(plsgenomics)
library(tidyr)

?read.arff
bcam <- read.arff("./assignmentDS/BreastCancerAll.missing.arff")
bcar <- read.arff("./assignmentDS/BreastCancerAll.reduced.using.cfs.missing.arff")

# EDA ====
"class" %in% names(bcam)
table(bcam$class)
# HER2+   HR+    TN 
#    50    54    53 

table(sapply(bcam[1,], class))
# factor numeric 
# 3047    9135

# One factor (three levels) derived from three numeric columns; all the way
# cfs = Chronic Fatigue Syndrome

# Reformat, save =====
bc_num <- bcam %>%
  mutate(class = as.numeric(class) - 1) %>%
  dplyr::select_if(is.numeric) %>%
  tidyr::drop_na()
  # saveRDS(bc_num, file = "bc_num.rds")

bc_fac <- bcam %>% 
  # mutate(class = as.numeric(class) - 1) %>%
  dplyr::select_if(is.factor) %>%
  tidyr::drop_na() 
  # saveRDS(bc_fac, file = "bc_fac.rds")

# Multinomial partial least squares regression =====
#   Requires numeric, matrices
?multinom.spls
index <- sample(1:155, 140)

train <- bc_num[index,] 
Xtrain <- train %>%
  select(-class) %>%
  as.matrix()
Ytrain <- train %>%
  select(class) %>%
  as.matrix()

test <- bc_num[-index,]
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
# fit$proba.test

# randomForest ======
# bc_fac <- readRDS("bc_fac.rds")
names(bc_fac) <- make.names(bc_fac)

index <- sample(1:157, 140)

train <- bc_fac[index,] 
Xtrain <- train %>%
  dplyr::select(-'class') 
Ytrain <- train %>%
  select(class) 

test <- bc_fac[-index,]
Xtest <- test %>%
  select(-class) 
Ytest <- test %>%
  select(class) 



train <- bc_fac[index,] 
test <- bc_fac[-index,]

  xtest <- test %>%
    select(-class)
  ytest <- test %>%
    select(class) %>%
    mutate(class = as.factor(class))
model_rf <- randomForest::randomForest(
  as.factor(class) ~ ., data=train, importance=TRUE, ntree=50, 
  xtest = xtest, ytest = ytest$class
)
model_rf$test$confusion

# SNPassoc - NA =====
?setupSNP
?WGassociation
# NO GO

# Bayesian Regularization (Feed-Forward) Neural Net - brnn ====
?brnn
b_fit <- brnn(class ~ ., data = bc_fac)
predict.brnn
# Neural net w/ neuralnet::neuralnet() =====

# SVM w/ kernlab::ksvm() =====

# Algorithm ======
bc_fac <- readRDS("bc_fac.rds")
nsim <- 15
n_train <- 140
for(i in 1:nsim){
  if(i == 1){
    container <- data.frame(Iteration = NA, Method = NA, Error_Rate = NA)
  }
  
  # Create iterative train/test set
  index <- sample(1:155, n_train)
  
  train <- bc_fac[index,] 
  Xtrain <- train %>%
    select(-class) %>%
    as.matrix()
  Ytrain <- train %>%
    select(class) %>%
    as.matrix()
  
  test <- bc_fac[-index,]
  Xtest <- test %>%
    select(-class) %>%
    as.matrix()
  Ytest <- test %>%
    select(class) %>%
    as.matrix()
  
  # Method 1 - most common class
  
  # Method 2 - Multinomial Partial Least Squares
  #    Requires matrix datasets
  fit <- multinom.spls(Xtrain = Xtrain, Ytrain = Ytrain, ncomp = 5,
                       lambda.ridge = 2, lambda.l1 = 0.5, adapt = TRUE,
                       maxIter = 25, svd.decompose = TRUE, Xtest = Xtest)
  err <- sum(fit$hatYtest != Ytest) / length(Ytest) # [1] 0.6
  container2i <- data.frame(Iteration = i, Method = "MPLS", Error_Rate = err)
  container <- rbind.data.frame(container, container2i)

  
}