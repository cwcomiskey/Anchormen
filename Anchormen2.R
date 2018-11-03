# Anchormen2

library(foreign)
library(dplyr)
library(plsgenomics)
library(tidyr)
library(randomForest)
library(neuralnet)
library(nnet)
library(brnn)
library(kernlab)

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

  # 3: n x 9135 indicators
  Covs01 <- Covs9135 %>%
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

# randomForest() =====
samp <- sample(1:157, 140)

dat <- cbind.data.frame(Res, Covs3045) 

names(dat) <- make.names(names(dat))

train <- dat[samp,] 

test <- dat[-samp,]
  Xtest <- test %>%
    select(-class) 
  Ytest <- test %>%
    select(class) 

rf <- randomForest(class ~ ., data = train, xtest = Xtest, ytest = Ytest$class)  
rf$test$confusion

# brnn() (computationally infeasible??) =====
dat <- cbind.data.frame(Res, Covs9135) 
samp <- sample(1:157, 140)

# names(dat) <- make.names(names(dat))

train <- dat[samp,] 
  Xtrain <- train %>%
    select(-class) 
  Ytrain <- train %>%
    select(class) 

test <- dat[-samp,]
  Xtest <- test %>%
    select(-class) 
  Ytest <- test %>%
    select(class) 

b_fit <- brnn(as.numeric(Ytrain$class) ~ as.matrix(Xtrain))
predict(b_fit, newdata = as.matrix(Xtest))
mean(as.numeric(round(predict(b_fit, newdata = as.matrix(Xtest))) == as.numeric(Ytest$class)))

# neuralnet::neuralnet() ====
dat <- cbind.data.frame(Res01, Covs9135) %>%
  tidyr::drop_na()

names(dat) <- make.names(names(dat))

samp <- sample(1:157, 140)

train <- dat[samp,] 
  # Xtrain <- train %>%
  #   select(-c("HER2.", "HR.", "TN")) 
  # Ytrain <- train %>%
  #   select(c("HER2.", "HR.", "TN")) 

test <- dat[-samp,]
  Xtest <- test %>%
    select(-c("HER2.", "HR.", "TN")) 
  Ytest <- test %>%
    select(c("HER2.", "HR.", "TN")) 

n <- names(dat)
f <- as.formula(paste("HER2. + HR. + TN ~", paste(n[!n %in% c("HER2.","HR.","TN")], collapse = " + ")))

nn <- neuralnet(formula = f, data = train, hidden = 3)

neuralnet::compute(x = nn, Xtest)$net.result

# plot(nn)

# kernlab::ksvm() =====
dat <- cbind.data.frame(Res, Covs9135) %>%
  tidyr::drop_na()
names(dat) <- make.names(names(dat))

samp <- sample(1:155, 140)

train <- dat[samp,] 

test <- dat[-samp,]
  Xtest <- test %>%
    select(-class) 
  Ytest <- test %>%
    select(class) 

svm <- ksvm(class ~ ., data = train, kernel="rbfdot",
            kpar=list(sigma=0.05), cross=3)

predict(svm, newdata = Xtest); Ytest
