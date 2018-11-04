source("PackData.R") # Load packages and data

dat <- cbind.data.frame(Res, Covs3045) 

# set.seed(101513)

names(dat) <- make.names(names(dat))

# Train/test, random selection ====
samp <- sample(1:155, 140)

train <- dat[samp,] 

test <- dat[-samp,]
  Xtest <- test %>%
    select(-class) 
  Ytest <- test %>%
    select(class) 

rf <- randomForest(class ~ ., data = train, importance = TRUE,
                   mtry = 100, ntree = 500)  

# Importance measures ====
rf_imp <- rf$importance
  rf_imp_MDA <- rf_imp %>%
    as.data.frame() %>%
    mutate(name = rownames(rf_imp)) %>%
    arrange(desc(MeanDecreaseAccuracy)) %>%
    .[1:5,c(4,6)]
  rf_imp_MDG <- rf_imp %>%
    as.data.frame() %>%
    mutate(name = rownames(rf_imp)) %>%
    arrange(desc(MeanDecreaseGini)) %>%
    .[1:5, c(5,  6)]
  
# Plot ====
randomForest::varImpPlot(rf, type=1, pch=19, col=1, cex=.5, main="Variable Importance", n.var = 10)
randomForest::varImpPlot(rf, type=2, pch=19, col=1, cex=.5, main="")

# Prediction =====
predict(rf, newdata = test) 
mean(predict(rf, newdata = test) == Ytest$class) 

# replicate =====
# mean(replicate(20, f())) # must wrap above in function "f"

# LIME ======
rf_fit <- caret::train(
  class ~ ., data = train, method = "rf", # *****
  importance=TRUE, ntree=500,
  tuneGrid = data.frame(mtry = 100)
) 

explainer <- lime::lime(train, rf_fit) 

explanation <- lime::explain(train[1,], explainer = explainer, 
                             n_labels = 1, n_features = 5)

explanation[,c(1:4, 7:10, 13)]
lime::plot_features(explanation)
