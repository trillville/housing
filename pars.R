
# Libraries ---------------------------------------------------------------

#library(FeatureHashing)
library(Matrix)
library(xgboost)
library(randomForest)
library(rBayesianOptimization)
library(caret)
library(Rtsne)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
#library(dummies)
library(Metrics)
library(readr)
#library(rknn)
library(Boruta)
#library(rPython)
#library(kernlab)
#library(data.table)


# Parameter Constants -----------------------------------------------------

# boruta analysis to variables prior to OHE encoding
# (old)
# CONFIRMED_ATTR <- c("MSSubClass","MSZoning",     "LotArea",      "LandContour",  "Neighborhood", "BldgType",     "HouseStyle",   "OverallQual", 
#                     "OverallCond",  "YearBuilt",    "YearRemodAdd", "Exterior1st",  "Exterior2nd",  "MasVnrType",   "MasVnrArea",   "ExterQual",   
#                     "Foundation",   "BsmtQual",     "BsmtCond",     "BsmtExposure", "BsmtFinType1", "BsmtFinSF1",   "BsmtUnfSF",    "TotalBsmtSF", 
#                     "HeatingQC",    "CentralAir",   "GrLivArea",    "BsmtFullBath", "FullBath",     "HalfBath",     "BedroomAbvGr", "KitchenAbvGr",
#                     "KitchenQual",  "TotRmsAbvGrd", "Functional",   "Fireplaces",   "FireplaceQu",  "GarageType",   "GarageYrBlt",  "GarageFinish",
#                     "GarageCars",   "GarageArea",   "GarageQual",   "GarageCond",   "PavedDrive",   "WoodDeckSF",   "OpenPorchSF",  "SF2ndFlr",    
#                     "SF1stFlr", "Fence")
# 
# TENTATIVE_ATTR <- c("Alley","LotShape","LandSlope","Condition1","RoofStyle","Electrical","SaleCondition")
# 
# REJECTED_ATTR <- c("LotFrontage",   "Street",        "Utilities",     "LotConfig",     "Condition2",    "RoofMatl",      "ExterCond",     "BsmtFinType2", 
#                    "BsmtFinSF2",    "Heating",       "LowQualFinSF",  "BsmtHalfBath",  "EnclosedPorch", "ScreenPorch",   "PoolArea",      "PoolQC",       
# "MiscFeature",   "MiscVal",       "MoSold",        "YrSold",        "SaleType",     "Porch3Ssn")

# boruta analysis to OHE encoded variables
CONFIRMED_ATTR <- c("LotFrontage"            ,"LotArea"                ,"OverallQual"            ,"OverallCond"           ,
                    "YearBuilt"              ,"YearRemodAdd"           ,"MasVnrArea"             ,"BsmtFinSF1"            ,
                    "BsmtUnfSF"              ,"TotalBsmtSF"            ,"GrLivArea"              ,"BsmtFullBath"          ,
                    "FullBath"               ,"HalfBath"               ,"BedroomAbvGr"           ,"KitchenAbvGr"          ,
                    "TotRmsAbvGrd"           ,"Fireplaces"             ,"GarageYrBlt"            ,"GarageCars"            ,
                    "GarageArea"             ,"WoodDeckSF"             ,"OpenPorchSF"            ,"MSSubClass.120"        ,
                    "MSSubClass.160"         ,"MSSubClass.20"          ,"MSSubClass.30"          ,"MSSubClass.60"         ,
                    "MSSubClass.90"          ,"MSZoning.C..all."       ,"MSZoning.FV"            ,"MSZoning.RL"           ,
                    "MSZoning.RM"            ,"LotShape.Reg"           ,"LandContour.Bnk"        ,"Neighborhood.BrDale"   ,
                    "Neighborhood.ClearCr"   ,"Neighborhood.CollgCr"   ,"Neighborhood.Crawfor"   ,"Neighborhood.Edwards"  ,
                    "Neighborhood.Gilbert"   ,"Neighborhood.MeadowV"   ,"Neighborhood.NAmes"     ,"Neighborhood.NoRidge"  ,
                    "Neighborhood.NridgHt"   ,"Neighborhood.NWAmes"    ,"Neighborhood.OldTown"   ,"Neighborhood.Somerst"  ,
                    "BldgType.1Fam"          ,"BldgType.Duplex"        ,"BldgType.Twnhs"         ,"BldgType.TwnhsE"       ,
                    "HouseStyle.1Story"      ,"HouseStyle.2Story"      ,"Exterior1st.MetalSd"    ,"Exterior1st.VinylSd"   ,
                    "Exterior2nd.MetalSd"    ,"Exterior2nd.VinylSd"    ,"MasVnrType.None"        ,"ExterQual"             ,
                    "Foundation.CBlock"      ,"Foundation.PConc"       ,"BsmtQual"               ,"BsmtCond"              ,
                    "BsmtExposure"           ,"BsmtFinType1"           ,"HeatingQC"              ,"CentralAir.N"          ,
                    "CentralAir.Y"           ,"KitchenQual"            ,"FireplaceQu"            ,"GarageType..MISSING."  ,
                    "GarageType.Attchd"      ,"GarageType.Detchd"      ,"GarageFinish..MISSING." ,"GarageFinish.Fin"      ,
                    "GarageFinish.RFn"       ,"GarageFinish.Unf"       ,"GarageQual"             ,"GarageCond"            ,
                    "PavedDrive"             ,"SF2ndFlr"               ,"SF1stFlr"  )

TENTATIVE_ATTR <- c("MSSubClass.180"       ,"MSSubClass.50"        ,"MSSubClass.70"        ,"LandContour.Lvl"     ,
                    "Neighborhood.NPkVill" ,"HouseStyle.1.5Fin"    ,"RoofStyle.Gable"      ,"RoofStyle.Hip"       ,
                    "Exterior1st.BrkFace"  ,"Exterior1st.Wd.Sdng"  ,"MasVnrType.BrkFace"   ,"Foundation.BrkTil"   ,
                    "Functional"           ,"Fence.MnPrv"          ,"SaleType.New"  )

REJECTED_ATTR <-c("BsmtFinSF2"            ,"LowQualFinSF"          ,"BsmtHalfBath"          ,"EnclosedPorch"        ,
                  "ScreenPorch"           ,"PoolArea"              ,"MiscVal"               ,"MoSold"               ,
                  "YrSold"                ,"MSSubClass.190"        ,"MSSubClass.40"         ,"MSSubClass.75"        ,
                  "MSSubClass.80"         ,"MSSubClass.85"         ,"MSZoning..MISSING."    ,"MSZoning.RH"          ,
                  "Street.Grvl"           ,"Street.Pave"           ,"Alley..MISSING."       ,"Alley.Grvl"           ,
                  "Alley.Pave"            ,"LotShape.IR1"          ,"LotShape.IR2"          ,"LotShape.IR3"         ,
                  "LandContour.HLS"       ,"LandContour.Low"       ,"Utilities"             ,"LotConfig.Corner"     ,
                  "LotConfig.CulDSac"     ,"LotConfig.FR2"         ,"LotConfig.FR3"         ,"LotConfig.Inside"     ,
                  "LandSlope"             ,"Neighborhood.Blmngtn"  ,"Neighborhood.Blueste"  ,"Neighborhood.BrkSide" ,
                  "Neighborhood.IDOTRR"   ,"Neighborhood.Mitchel"  ,"Neighborhood.Sawyer"   ,"Neighborhood.SawyerW" ,
                  "Neighborhood.StoneBr"  ,"Neighborhood.SWISU"    ,"Neighborhood.Timber"   ,"Neighborhood.Veenker" ,
                  "Condition1.Artery"     ,"Condition1.Feedr"      ,"Condition1.Norm"       ,"Condition1.PosA"      ,
                  "Condition1.PosN"       ,"Condition1.RRAe"       ,"Condition1.RRAn"       ,"Condition1.RRNe"      ,
                  "Condition1.RRNn"       ,"Condition2.Artery"     ,"Condition2.Feedr"      ,"Condition2.Norm"      ,
                  "Condition2.PosA"       ,"Condition2.RRAe"       ,"Condition2.RRAn"       ,"Condition2.RRNn"      ,
                  "BldgType.2fmCon"       ,"HouseStyle.1.5Unf"     ,"HouseStyle.2.5Fin"     ,"HouseStyle.2.5Unf"    ,
                  "HouseStyle.SFoyer"     ,"HouseStyle.SLvl"       ,"RoofStyle.Flat"        ,"RoofStyle.Gambrel"    ,
                  "RoofStyle.Mansard"     ,"RoofStyle.Shed"        ,"RoofMatl.ClyTile"      ,"RoofMatl.CompShg"     ,
                  "RoofMatl.Membran"      ,"RoofMatl.Metal"        ,"RoofMatl.Roll"         ,"RoofMatl.Tar.Grv"     ,
                  "RoofMatl.WdShake"      ,"RoofMatl.WdShngl"      ,"Exterior1st..MISSING." ,"Exterior1st.AsbShng"  ,
                  "Exterior1st.AsphShn"   ,"Exterior1st.BrkComm"   ,"Exterior1st.CBlock"    ,"Exterior1st.CemntBd"  ,
                  "Exterior1st.HdBoard"   ,"Exterior1st.ImStucc"   ,"Exterior1st.Plywood"   ,"Exterior1st.Stone"    ,
                  "Exterior1st.Stucco"    ,"Exterior1st.WdShing"   ,"Exterior2nd..MISSING." ,"Exterior2nd.AsbShng"  ,
                  "Exterior2nd.AsphShn"   ,"Exterior2nd.Brk.Cmn"   ,"Exterior2nd.BrkFace"   ,"Exterior2nd.CBlock"   ,
                  "Exterior2nd.CmentBd"   ,"Exterior2nd.HdBoard"   ,"Exterior2nd.ImStucc"   ,"Exterior2nd.Other"    ,
                  "Exterior2nd.Plywood"   ,"Exterior2nd.Stone"     ,"Exterior2nd.Stucco"    ,"Exterior2nd.Wd.Sdng"  ,
                  "Exterior2nd.Wd.Shng"   ,"MasVnrType..MISSING."  ,"MasVnrType.BrkCmn"     ,"MasVnrType.Stone"     ,
                  "ExterCond"             ,"Foundation.Slab"       ,"Foundation.Stone"      ,"Foundation.Wood"      ,
                  "BsmtFinType2"          ,"Heating.Floor"         ,"Heating.GasA"          ,"Heating.GasW"         ,
                  "Heating.Grav"          ,"Heating.OthW"          ,"Heating.Wall"          ,"Electrical.Other"     ,
                  "Electrical.SBrkr"      ,"GarageType.2Types"     ,"GarageType.Basment"    ,"GarageType.BuiltIn"   ,
                  "GarageType.CarPort"    ,"PoolQC"                ,"Fence..MISSING."       ,"Fence.GdPrv"          ,
                  "Fence.GdWo"            ,"Fence.MnWw"            ,"MiscFeature..MISSING." ,"MiscFeature.Gar2"     ,
                  "MiscFeature.Othr"      ,"MiscFeature.Shed"      ,"MiscFeature.TenC"      ,"SaleType..MISSING."   ,
                  "SaleType.COD"          ,"SaleType.Con"          ,"SaleType.ConLD"        ,"SaleType.ConLI"       ,
                  "SaleType.ConLw"        ,"SaleType.CWD"          ,"SaleTyep.Oth"          ,"SaleType.WD"          ,
                  "SaleCondition.Abnorml" ,"SaleCondition.AdjLand" ,"SaleCondition.Alloca"  ,"SaleCondition.Family" ,
                  "SaleCondition.Normal"  ,"SaleCondition.Partial" ,"Porch3Ssn"            )

PREDICTOR_ATTR <- c(CONFIRMED_ATTR,TENTATIVE_ATTR)
ALL_ATTR <- c(CONFIRMED_ATTR,TENTATIVE_ATTR,REJECTED_ATTR)


OUTLIERS <- c(4   ,14   ,31   ,39  , 67 ,  98 , 143 , 154 , 224 , 239 , 278 , 314 , 329 , 411 , 
              432 , 463 , 561  ,582  ,589 , 590, 608  ,629 , 633  ,659  ,682 , 689 , 715 , 729 ,
              773 , 775 , 813 , 865 , 875  ,969 , 971 , 973, 1123 ,1144, 1212 ,1325,
              1416 ,1424, 1433 ,1454)


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
                 nrounds=1300,
                 max_depth=5,
                 eta=0.01,
                 gamma=0.0,
                 subsample = 0.9,
                 colsample_bytree=0.5,
                 min_child_weight=1)

XGB_PARS2 <- list(objective = "reg:linear",
                 eval_metric = "rmse",
                 nrounds=1300,
                 max_depth=3,
                 eta=0.01,
                 gamma=0.0,
                 colsample_bytree=0.5,
                 min_child_weight=5)


LAS_PARS <- list(lambda = 0.2)

RF_PARS <- list(ntree = 800,
                mtry = 29)

KNN5_PARS <- expand.grid(k = 5)
KNN10_PARS <- expand.grid(k = 10)

XGB_CARET_TUNE_GRID <-  expand.grid(nrounds=seq(100,1200, by = 100), 
                                    eta=c(0.01,0.03,0.05,0.1,0.3),
                                    max_depth = c(1, 3, 5, 7, 10, 15),
                                    colsample_bytree = c(.33,.66,.90),
                                    min_child_weight = c(1, 5, 10, 20),
                                    gamma = c(0,0.1,1))

RF_CARET_TUNE_GRID <- expand.grid(mtry = c(15, 20, 25, 30, 40, 50, 60, 80, 100, 130, 160))


KNN_CARET_TUNE_GRID <- expand.grid(k = c(1, 2, 3, 5, 7, 10, 15))
