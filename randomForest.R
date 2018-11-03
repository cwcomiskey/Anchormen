dat <- cbind.data.frame(Res, Covs3045) 

names(dat) <- make.names(names(dat))

samp <- sample(1:155, 140)

train <- dat[samp,] 

test <- dat[-samp,]
  Xtest <- test %>%
    select(-class) 
  Ytest <- test %>%
    select(class) 

rf <- randomForest(class ~ ., data = train, importance = TRUE,
                   mtry = 100, ntree = 500)  

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
  
# Plot  
randomForest::varImpPlot(rf, type=1, pch=19, col=1, cex=.5, main="")
randomForest::varImpPlot(rf, type=2, pch=19, col=1, cex=.5, main="")

mean(predict(rf, newdata = test) == Ytest$class) # Success prob!

?replicate
# mean(replicate(100, mean(rnorm(25))))

# Key question: are variable important measure persistent (correlated) across random samples of train/test?
for(i in 1:nsim){
  # Random train/test sample
  # Fit model
  # Var Imp rank
}
  # Cor()
  # i.e. Do relative ranks tend to remain similar?
  # i.e. Do more important variables tend to remain more important across samples, less important remain less?