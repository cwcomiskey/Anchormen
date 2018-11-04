
# Load packages and data 
source("PackData.R")

nsim = 15 # number of simulations
ntrain = 135 # size of training set, of 155
container <- data.frame(Iteration = NA, Method = NA, Success = NA)

# Algorithm ====
for(i in 1:nsim){
  
  # track progress
  print(i)
  print(dim(container))
  
  # Current iteration sample
  samp <- sample(1:155, 140)

  # Method 1 - Multinomial Partial Least Squares ====
  
  # Create dataframe with model-appropriate classes
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
  
  # fit model
  fit <- multinom.spls(Xtrain = Xtrain, Ytrain = Ytrain, ncomp = 5,
                       lambda.ridge = 2, lambda.l1 = 0.5, adapt = TRUE,
                       maxIter = 25, svd.decompose = TRUE, Xtest = Xtest)
  # Predictions
  s <- sum(fit$hatYtest == Ytest) / length(Ytest) # [1] 0.6
  
  # Store in iterative container
  container1i <- data.frame(Iteration = i, Method = "MPLS", Success = s)
  
  # Add to global container
  container <- rbind.data.frame(container, container1i)
  
  # Method 2 - Random Forest =====
  
  # Create dataframe with model-appropriate classes
  dat <- cbind.data.frame(Res, Covs3045) 
  names(dat) <- make.names(names(dat))
  
  train <- dat[samp,] 
  
  test <- dat[-samp,]
  Xtest <- test %>%
    select(-class) 
  Ytest <- test %>%
    select(class) 
  
  # Fit, predict, store, add
  rf <- randomForest(class ~ ., data = train)  
  s <- mean(predict(rf, newdata = test) == Ytest$class)
  container2i <- data.frame(Iteration = i, Method = "RF", Success = s)
  container <- rbind.data.frame(container, container2i)
  
  # Method 3 - Neural Net ====
  
  # Create dataframe with model-appropriate classes
  dat <- cbind.data.frame(Res01, Covs9135) 
  names(dat) <- make.names(names(dat))
  
  train <- dat[samp,] 
  
  test <- dat[-samp,]
    Xtest <- test %>%
      select(-c("HER2.", "HR.", "TN")) 
    Ytest <- test %>%
      select(c("HER2.", "HR.", "TN")) 
  
  # Create 'formula' object
  n <- names(dat)
  f <- as.formula(paste("HER2. + HR. + TN ~", paste(n[!n %in% c("HER2.","HR.","TN")], collapse = " + ")))
  
  # Fit
  nn <- neuralnet(formula = f, data = train, hidden = 3)
  
  # Two step prediction
  probs <- neuralnet::compute(x = nn, Xtest)$net.result 
  s <- mean(apply(probs, 1, which.max) == apply(Ytest, 1, which.max))
  
  # Store iterative, add to global
  container3i <- data.frame(Iteration = i, Method = "NN", Success = s)
  container <- rbind.data.frame(container, container3i)
  
  # Method 4 - Support Vector Machine ====
  
  # Create dataframe with model-appropriate classes
  dat <- cbind.data.frame(Res, Covs9135) 
  names(dat) <- make.names(names(dat))
  
  train <- dat[samp,] 
  
  test <- dat[-samp,]
    Xtest <- test %>%
      select(-class) 
    Ytest <- test %>%
      select(class) 
  
  # Fit, predict, store, add
  svm <- ksvm(class ~ ., data = train, kernel="rbfdot",
              kpar=list(sigma=0.05), cross=3)
  
  s <- mean(predict(svm, newdata = Xtest) == Ytest$class)
  
  container4i <- data.frame(Iteration = i, Method = "SVM", Success = s)
  container <- rbind.data.frame(container, container4i)
  
  
}

# Save results
# saveRDS(container, "container.rds")

# Calculate 4 method means ====
container %>%
  group_by(Method) %>%
  summarise(mean(Success))

# Plot ====
library(ggplot2)

ggplot(data = container) +  
  geom_jitter(aes(x = Method, y = Success, color = Method), width = 0.025, height = 0.025) +
  # geom_point(aes(x = Method, y = Success)) +
  ggtitle("Toy Comparison Study of Predictive Models for SNP Genetic Data") +
  ylab("Empirical Success Rate") 
