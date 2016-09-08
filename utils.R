# Libraries ---------------------------------------------------------------

#library(FeatureHashing)
library(Matrix)
library(xgboost)
library(randomForest)
library(caret)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
#library(dummies)
library(Metrics)
library(readr)
#library(kernlab)
#library(data.table)


# Constants ---------------------------------------------------------------

# incorporate results of Boruta analysis
CONFIRMED_ATTR <- c("MSSubClass","MSZoning","LotArea","LotShape","LandContour","Neighborhood",
                    "BldgType","HouseStyle","OverallQual","OverallCond","YearBuilt",
                    "YearRemodAdd","Exterior1st","Exterior2nd","MasVnrArea","ExterQual",
                    "Foundation","BsmtQual","BsmtCond","BsmtFinType1","BsmtFinSF1",
                    "BsmtFinType2","BsmtUnfSF","TotalBsmtSF","HeatingQC","CentralAir",
                    "X1stFlrSF","X2ndFlrSF","GrLivArea","BsmtFullBath","FullBath","HalfBath",
                    "BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Functional",
                    "Fireplaces","FireplaceQu","GarageType","GarageYrBlt","GarageFinish",
                    "GarageCars","GarageArea","GarageQual","GarageCond","PavedDrive","WoodDeckSF",
                    "OpenPorchSF","Fence")

TENTATIVE_ATTR <- c("Alley","LandSlope","Condition1","RoofStyle","MasVnrType","BsmtExposure",
                    "Electrical","EnclosedPorch","SaleCondition")

REJECTED_ATTR <- c("LotFrontage","Street","Utilities","LotConfig","Condition2","RoofMatl",
                   "ExterCond","BsmtFinSF2","Heating","LowQualFinSF","BsmtHalfBath",
                   "X3SsnPorch","ScreenPorch","PoolArea","PoolQC","MiscFeature","MiscVal",
                   "MoSold","YrSold","SaleType")

PREDICTOR_ATTR <- c(CONFIRMED_ATTR,TENTATIVE_ATTR,REJECTED_ATTR)

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