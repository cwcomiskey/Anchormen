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

# --- one factor every third numeric column
# cfs = Chronic Fatigue Syndrome

bc_num <- bcam %>% 
  mutate(class = as.numeric(class) - 1) %>%
  dplyr::select_if(is.numeric) %>%
  tidyr::drop_na() 


# Multinomial regression for genomics =====
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
sum(fit$hatYtest == Ytest) / length(Ytest) # [1] 0.6 HUZZAH!!
fit$proba.test

