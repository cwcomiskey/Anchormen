# Algorithm

source("PackData.R")

nsim = 15
ntrain = 135
container <- data.frame(Iteration = NA, Method = NA, Success = NA)

for(i in 1:nsim){
  
  # Current iteration sample
  samp <- sample(1:155, 140)

  # Method 1 - Multinomial Partial Least Squares ====
  dat <- cbind.data.frame(Res, Covs9135) %>%
    mutate(class = as.numeric(class) - 1)

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
  s <- sum(fit$hatYtest == Ytest) / length(Ytest) # [1] 0.6
  
  container1i <- data.frame(Iteration = i, Method = "MPLS", Success = s)
  container <- rbind.data.frame(container, container1i)
  
  # Method 2 - Random Forest =====
  dat <- cbind.data.frame(Res, Covs3045) 
  names(dat) <- make.names(names(dat))
  
  train <- dat[samp,] 
  
  test <- dat[-samp,]
  Xtest <- test %>%
    select(-class) 
  Ytest <- test %>%
    select(class) 
  
  rf <- randomForest(class ~ ., data = train)  
  s <- mean(predict(rf, newdata = test) == Ytest$class)
  
  container2i <- data.frame(Iteration = i, Method = "RF", Success = s)
  container <- rbind.data.frame(container, container2i)
  
  # Method 3 - Neural Net ====
  
  dat <- cbind.data.frame(Res01, Covs9135) 
  names(dat) <- make.names(names(dat))
  
  train <- dat[samp,] 
  
  test <- dat[-samp,]
    Xtest <- test %>%
      select(-c("HER2.", "HR.", "TN")) 
    Ytest <- test %>%
      select(c("HER2.", "HR.", "TN")) 
  
  n <- names(dat)
  f <- as.formula(paste("HER2. + HR. + TN ~", paste(n[!n %in% c("HER2.","HR.","TN")], collapse = " + ")))
  
  nn <- neuralnet(formula = f, data = train, hidden = 3)
  
  probs <- neuralnet::compute(x = nn, Xtest)$net.result 
  s <- mean(apply(probs, 1, which.max) == apply(Ytest, 1, which.max))
  
  container3i <- data.frame(Iteration = i, Method = "NN", Success = s)
  container <- rbind.data.frame(container, container3i)
  
  # Method 4 - Support Vector Machine ====
  dat <- cbind.data.frame(Res, Covs9135) 
  names(dat) <- make.names(names(dat))
  
  train <- dat[samp,] 
  
  test <- dat[-samp,]
    Xtest <- test %>%
      select(-class) 
    Ytest <- test %>%
      select(class) 
  
  svm <- ksvm(class ~ ., data = train, kernel="rbfdot",
              kpar=list(sigma=0.05), cross=3)
  
  s <- mean(predict(svm, newdata = Xtest) == Ytest$class)
  
  container4i <- data.frame(Iteration = i, Method = "SVM", Success = s)
  container <- rbind.data.frame(container, container4i)
  
  
}