# Random Forest Model for Breast Cancer Classification in SNP Genetic Variants Dataset

source("PackData.R") # Load packages and data

dat <- cbind.data.frame(Res, Covs3045) # ([155 x 1], [155 x 3045])

# set.seed(101513)

names(dat) <- make.names(names(dat)) # reformat names for 'formula'

# Random train/test split ====
samp <- sample(1:155, 140)

train <- dat[samp,] 

test <- dat[-samp,]
  Xtest <- test %>%
    select(-class) 
  Ytest <- test %>%
    select(class) 

# Fit model ====
rf <- randomForest(class ~ ., data = train, importance = TRUE,
                   mtry = 100, ntree = 500)  

# Collect variable importance measures ====
rf_imp <- rf$importance
  
  # Mean Decrease in Accuracy
  rf_imp_MDA <- rf_imp %>%
    as.data.frame() %>%
    mutate(name = rownames(rf_imp)) %>%
    arrange(desc(MeanDecreaseAccuracy)) %>%
    .[1:5,c(4,6)]
  
  # Mean Decrease in Gini Index
  rf_imp_MDG <- rf_imp %>%
    as.data.frame() %>%
    mutate(name = rownames(rf_imp)) %>%
    arrange(desc(MeanDecreaseGini)) %>%
    .[1:5, c(5,  6)]
  
# Plot importance measures ====
randomForest::varImpPlot(rf, type=1, pch=19, col=1, cex=.5, 
                         main="Variable Importance", n.var = 10)
randomForest::varImpPlot(rf, type=2, pch=19, col=1, cex=.5, main="")

# Predictions =====
predict(rf, newdata = test) 
mean(predict(rf, newdata = test) == Ytest$class) # success rate

# replicate =====
# mean(replicate(20, f())) # first wrap split-fit-predict in function "f()"

# LIME ======

# LIME requires random forest fit via caret::train()
rf_fit <- caret::train(
  class ~ ., data = train, method = "rf", # *****
  importance=TRUE, ntree=500,
  tuneGrid = data.frame(mtry = 100)
) 

# Create lime object
explainer <- lime::lime(train, rf_fit) 

# Explain one observation
explanation <- lime::explain(train[1,], explainer = explainer, 
                             n_labels = 1, n_features = 5)

# Result
explanation[,c(1:4, 7:10, 13)]

# Plot
lime::plot_features(explanation)
