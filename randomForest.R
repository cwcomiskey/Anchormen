dat <- cbind.data.frame(Res, Covs3045) 
names(dat) <- make.names(names(dat))

samp <- sample(1:155, 140)

train <- dat[samp,] 

test <- dat[-samp,]
  Xtest <- test %>%
    select(-class) 
  Ytest <- test %>%
    select(class) 

rf <- randomForest(class ~ ., data = train, importance = TRUE)  

rf_imp <- rf$importance
rownames(rf_imp)
  rf_imp_MDA <- rf_imp %>%
    as.data.frame() %>%
    mutate(name = rownames(rf_imp)) %>%
    arrange(desc(MeanDecreaseAccuracy))
  rf_imp_MDG <- rf_imp %>%
    as.data.frame() %>%
    arrange(desc(MeanDecreaseGini))
  
# Plot  
randomForest::varImpPlot(rf, type=1, pch=19, col=1, cex=.5, main="")
randomForest::varImpPlot(rf, type=2, pch=19, col=1, cex=.5, main="")

mean(predict(rf, newdata = test) == Ytest$class) # Success prob!

?replicate
mean(replicate(100, mean(rnorm(25))))
