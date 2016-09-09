# Libraries ---------------------------------------------------------------

#library(FeatureHashing)
library(Matrix)
library(xgboost)
library(randomForest)
library(rBayesianOptimization)
library(caret)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
#library(dummies)
library(Metrics)
library(readr)
library(Boruta)
#library(kernlab)
#library(data.table)


# Constants ---------------------------------------------------------------

# incorporate results of Boruta analysis (using ordinal data set)
CONFIRMED_ATTR <- c("MSSubClass","MSZoning",     "LotArea",      "LandContour",  "Neighborhood", "BldgType",     "HouseStyle",   "OverallQual", 
                    "OverallCond",  "YearBuilt",    "YearRemodAdd", "Exterior1st",  "Exterior2nd",  "MasVnrType",   "MasVnrArea",   "ExterQual",   
                    "Foundation",   "BsmtQual",     "BsmtCond",     "BsmtExposure", "BsmtFinType1", "BsmtFinSF1",   "BsmtUnfSF",    "TotalBsmtSF", 
                    "HeatingQC",    "CentralAir",   "GrLivArea",    "BsmtFullBath", "FullBath",     "HalfBath",     "BedroomAbvGr", "KitchenAbvGr",
                    "KitchenQual",  "TotRmsAbvGrd", "Functional",   "Fireplaces",   "FireplaceQu",  "GarageType",   "GarageYrBlt",  "GarageFinish",
                    "GarageCars",   "GarageArea",   "GarageQual",   "GarageCond",   "PavedDrive",   "WoodDeckSF",   "OpenPorchSF",  "SF2ndFlr",    
                    "SF1stFlr", "Fence")

TENTATIVE_ATTR <- c("Alley","LotShape","LandSlope","Condition1","RoofStyle","Electrical","SaleCondition")

REJECTED_ATTR <- c("LotFrontage",   "Street",        "Utilities",     "LotConfig",     "Condition2",    "RoofMatl",      "ExterCond",     "BsmtFinType2", 
                   "BsmtFinSF2",    "Heating",       "LowQualFinSF",  "BsmtHalfBath",  "EnclosedPorch", "ScreenPorch",   "PoolArea",      "PoolQC",       
                   "MiscFeature",   "MiscVal",       "MoSold",        "YrSold",        "SaleType",     "Porch3Ssn")

PREDICTOR_ATTR <- c(CONFIRMED_ATTR,TENTATIVE_ATTR)
ALL_ATTR <- c(CONFIRMED_ATTR,TENTATIVE_ATTR,REJECTED_ATTR)

XGB_PARS <- list(objective = "reg:linear",
                 eval_metric = "rmse",
                 nrounds=600, 
                 max_depth=5, 
                 eta=0.05, 
                 gamma=0.1, 
                 colsample_bytree=0.5, 
                 min_child_weight=1)

# Functions ---------------------------------------------------------------

trainOneFold <- function(fold, predictors, y, ID) {
  # fit model based on all training data except for current fold
  set.seed(825)
  # fit <- do.call(train,
  #                c(list(x = predictors[-fold, ],
  #                       y = y[-fold]),
  #                  missing = NaN,
  #                  CARET.TRAIN.PARMS,
  #                  MODEL.SPECIFIC.PARMS,
  #                  CARET.TRAIN.OTHER.PARMS))
  fit <- do.call(xgboost,
                     c(list(data = predictors[-fold, ],
                            label = y[-fold]),
                       XGB_PARS))
  
  pred <- predict(fit,newdata = predictors[fold, ])
  
  score <- logRMSE(y[fold], pred)
  
  ans <- list(fitted.mdl = fit,
              score=score,
              predictions = data.frame(ID = ID[fold], yhat = pred, y = y[fold]))
  
  return(ans)
}

# consistent with kaggle leaderboard
logRMSE <- function(actual, predicted) {
  return (sqrt(mean((log(actual) - log(predicted))^2)))
}

runBoruta <- function(){
  
  # ordinal encoding
  response <- y.train
  sample <- select(dat.ord, everything(), -Id, -SalePrice)
  set.seed(69)
  bor.results <- Boruta(sample[train, ], 
                        response,
                        maxRuns = 200,
                        doTrace = 2)
  
  results <- as.data.frame(bor.results$finalDecision)
  names <- rownames(test)
  final <- data.frame(cbind(names, results[[1]]))
  
  boruta.ord.tentative <- as.character(final$names[which(final$V2 == 1)])
  boruta.ord.confirmed <- as.character(final$names[which(final$V2 == 2)])
  boruta.ord.rejected <- as.character(final$names[which(final$V2 == 3)])
  ans <- list(bor.results,
              boruta.ord,tentative,
              boruta.confirmed,
              boruta.rejected)
  return(ans)
}





