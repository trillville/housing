# Parameter Constants -----------------------------------------------------

# BURUTA ANALYSIS RESULTS - ORDINAL DATA SET
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


# Hyperparameter Constants ------------------------------------------------

### GENERAL
CARET_TRAIN_CTRL <- trainControl(method="repeatedCV",
                                 number = 5,
                                 repeats = 5,
                                 verboseIter=FALSE,
                                 classProbs=FALSE)

### XGBOOST
# XGB_PARS <- list(objective = "reg:linear",
#                  eval_metric = "rmse",
#                  nrounds=857,
#                  max_depth=4,
#                  eta=0.1,
#                  colsample_bytree=0.9,
#                  subsample = 0.9,
#                  min_child_weight=5)

XGB_PARS <- list(objective = "reg:linear",
                 eval_metric = "rmse",
                 nrounds=1000,
                 max_depth=5,
                 eta=0.03,
                 gamma=0.1,
                 subsample = 0.9,
                 colsample_bytree=0.5,
                 min_child_weight=1)


XGB_CARET_TUNE_GRID <-  expand.grid(nrounds=seq(100,1200, by = 100), 
                                    eta=c(0.01,0.03,0.05,0.1,0.3),
                                    max_depth = c(1, 3, 5, 7, 10, 15),
                                    colsample_bytree = c(.33,.66,.90),
                                    min_child_weight = c(1, 5, 10, 20),
                                    gamma = c(0,0.1,1))


