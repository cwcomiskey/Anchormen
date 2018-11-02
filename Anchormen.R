# Load/install packages
# install.packages("foreign")
library(foreign)
library(dplyr)
library(plsgenomics)
library(tidyr)
library(neuralnet)

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
bc_num <- readRDS("bc_num.rds")
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

index <- sample(1:157, 140)

train <- bc_fac[index,] 
test <- bc_fac[-index,]
  xtest <- test %>%
    select(-class)
  ytest <- test %>%
    select(class) %>%
    mutate(class = as.factor(class))
  
names(train) <- make.names(names(train))
names(xtest) <- make.names(names(xtest))

model_rf <- randomForest::randomForest(
  as.factor(class) ~ ., data=train, importance=TRUE, ntree=50, 
  xtest = xtest, ytest = ytest$class
)
model_rf$test$confusion

# SNPassoc - NA =====
?setupSNP
?WGassociation
# NO GO (req's binary)

# Bayesian Regularization (Feed-Forward) Neural Net - brnn ====
?brnn
# bc_fac <- readRDS("bc_fac.rds")
bc_fac <- sapply(bc_fac, as.numeric)

b_fit <- brnn(x = bc_fac[,1:100], y = bc_fac[,'class'])

predict(b_fit, newdata = bc_fac[1:10,1:100])

mean(as.numeric(round(predict(b_fit, newdata = bc_fac[1:10,1:100])) == bc_fac[1:10,'class']))



# Neural net w/ neuralnet::neuralnet() =====
# bc_fac <- readRDS("bc_fac.rds")

names(bc_fac) <- make.names(names(bc_fac))
bc_fac <- sapply(bc_fac, as.numeric)

n <- names(bc_fac)
f <- as.formula(paste("class ~", paste(n[!n %in% "class"], collapse = " + ")))
f

nn <- neuralnet(formula = f, data = bc_fac, hidden = 2)
test <- as.data.frame(bc_fac) %>% 
  sample_n(10)
  xtest <- test %>% select(-class)
  ytest <- test %>% select(class)
preds <- round(compute(nn, xtest)$net.result)

plot()

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