
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
library(rknn)
library(Boruta)
#library(rPython)
#library(kernlab)
#library(data.table)


# Parameter Constants -----------------------------------------------------

# BURUTA RF ANALYSIS RESULTS - ORDINAL DATA SET USING OHE
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

# (new)
CONFIRMED_ATTR <- c("MSSubClass.20"         , "MSSubClass.30"          ,"MSSubClass.60"         , "MSSubClass.90"         , "MSSubClass.160"        ,
                    "MSZoning.C..all."      , "MSZoning.RL"            ,"MSZoning.RM"           , "LotArea"               , "Neighborhood.BrDale"   ,
                    "Neighborhood.ClearCr"  , "Neighborhood.CollgCr"   ,"Neighborhood.Crawfor"  , "Neighborhood.Edwards"  , "Neighborhood.Gilbert"  ,
                    "Neighborhood.MeadowV"  , "Neighborhood.NAmes"     ,"Neighborhood.NoRidge"  , "Neighborhood.NridgHt"  , "Neighborhood.NWAmes" ,  
                    "Neighborhood.OldTown"  , "Neighborhood.Somerst"   ,"BldgType.1Fam"         , "BldgType.Duplex"       , "BldgType.Twnhs"      ,  
                    "BldgType.TwnhsE"       , "HouseStyle.1Story"      ,"HouseStyle.2Story"     , "OverallQual"           , "OverallCond"         ,  
                    "YearBuilt"             , "YearRemodAdd"           ,"Exterior1st.MetalSd"   , "Exterior1st.VinylSd"   , "Exterior2nd.MetalSd" ,  
                    "Exterior2nd.VinylSd"   , "MasVnrType.None"        ,"MasVnrArea"            , "ExterQual"             , "Foundation.CBlock"   ,  
                    "Foundation.PConc"      , "BsmtQual"               ,"BsmtCond"              , "BsmtExposure"          , "BsmtFinType1"        ,  
                    "BsmtFinSF1"            , "BsmtUnfSF"              ,"TotalBsmtSF"           , "HeatingQC"             , "CentralAir.N"        ,  
                    "CentralAir.Y"          , "GrLivArea"              ,"BsmtFullBath"          , "FullBath"              , "HalfBath"            ,  
                    "BedroomAbvGr"          , "KitchenAbvGr"           ,"KitchenQual"           , "TotRmsAbvGrd"          , "Fireplaces"          ,  
                    "FireplaceQu"           , "GarageType..MISSING."   ,"GarageType.Attchd"     , "GarageType.Detchd"     , "GarageYrBlt"         ,  
                    "GarageFinish..MISSING.", "GarageFinish.Fin"       ,"GarageFinish.RFn"      , "GarageFinish.Unf"      , "GarageCars"          ,  
                    "GarageArea"            , "GarageQual"             ,"GarageCond"            , "PavedDrive"            , "WoodDeckSF"          ,  
                    "OpenPorchSF"           , "SF2ndFlr"               ,"SF1stFlr"   )

TENTATIVE_ATTR <- c("MSSubClass.50"        ,"MSSubClass.70"      ,  "MSSubClass.120"      , "MSSubClass.180"       ,"MSZoning.FV"         ,
                    "LotShape.Reg"         ,"LandContour.Bnk"    ,  "LandContour.Lvl"     , "Neighborhood.NPkVill" ,"Neighborhood.Sawyer" ,
                     "HouseStyle.1.5Fin"  ,  "RoofStyle.Gable"   ,   "RoofStyle.Hip"      ,  "Exterior1st.BrkFace" , "Exterior1st.Wd.Sdng" ,
                     "Exterior2nd.CmentBd",  "MasVnrType.BrkFace" ,  "Foundation.BrkTil"  ,  "BsmtFinType2"        , "Functional"          ,
                     "Fence.MnPrv"        ,  "SaleType.New"   )

REJECTED_ATTR <- c( "MSSubClass.40"         ,"MSSubClass.75"         ,"MSSubClass.80"         ,"MSSubClass.85"         ,"MSSubClass.190"       ,
                    "MSZoning..MISSING."    ,"MSZoning.RH"           ,"LotFrontage"           ,"Street.Grvl"           ,"Street.Pave"          ,
                    "Alley..MISSING."       ,"Alley.Grvl"            ,"Alley.Pave"            ,"LotShape.IR1"          ,"LotShape.IR2"         ,
                    "LotShape.IR3"          ,"LandContour.HLS"       ,"LandContour.Low"       ,"Utilities"             ,"LotConfig.Corner"     ,
                    "LotConfig.CulDSac"     ,"LotConfig.FR2"         ,"LotConfig.FR3"         ,"LotConfig.Inside"      ,"LandSlope"            ,
                    "Neighborhood.Blmngtn"  ,"Neighborhood.Blueste"  ,"Neighborhood.BrkSide"  ,"Neighborhood.IDOTRR"   ,"Neighborhood.Mitchel" ,
                    "Neighborhood.SawyerW"  ,"Neighborhood.StoneBr"  ,"Neighborhood.SWISU"    ,"Neighborhood.Timber"   ,"Neighborhood.Veenker" ,
                    "Condition1.Artery"     ,"Condition1.Feedr"      ,"Condition1.Norm"       ,"Condition1.PosA"       ,"Condition1.PosN"      ,
                    "Condition1.RRAe"       ,"Condition1.RRAn"       ,"Condition1.RRNe"       ,"Condition1.RRNn"       ,"Condition2.Artery"    ,
                    "Condition2.Feedr"      ,"Condition2.Norm"       ,"Condition2.PosA"       ,"Condition2.RRAe"       ,"Condition2.RRAn"      ,
                    "Condition2.RRNn"       ,"BldgType.2fmCon"       ,"HouseStyle.1.5Unf"     ,"HouseStyle.2.5Fin"     ,"HouseStyle.2.5Unf"    ,
                    "HouseStyle.SFoyer"     ,"HouseStyle.SLvl"       ,"RoofStyle.Flat"        ,"RoofStyle.Gambrel"     ,"RoofStyle.Mansard"    ,
                    "RoofStyle.Shed"        ,"RoofMatl.ClyTile"      ,"RoofMatl.CompShg"      ,"RoofMatl.Membran"      ,"RoofMatl.Metal"       ,
                    "RoofMatl.Roll"         ,"RoofMatl.Tar.Grv"      ,"RoofMatl.WdShake"      ,"RoofMatl.WdShngl"      ,"Exterior1st..MISSING.",
                    "Exterior1st.AsbShng"   ,"Exterior1st.AsphShn"   ,"Exterior1st.BrkComm"   ,"Exterior1st.CBlock"    ,"Exterior1st.CemntBd"  ,
                    "Exterior1st.HdBoard"   ,"Exterior1st.ImStucc"   ,"Exterior1st.Plywood"   ,"Exterior1st.Stone"     ,"Exterior1st.Stucco"   ,
                    "Exterior1st.WdShing"   ,"Exterior2nd..MISSING." ,"Exterior2nd.AsbShng"   ,"Exterior2nd.AsphShn"   ,"Exterior2nd.Brk.Cmn"  ,
                    "Exterior2nd.BrkFace"   ,"Exterior2nd.CBlock"    ,"Exterior2nd.HdBoard"   ,"Exterior2nd.ImStucc"   ,"Exterior2nd.Other"    ,
                    "Exterior2nd.Plywood"   ,"Exterior2nd.Stone"     ,"Exterior2nd.Stucco"    ,"Exterior2nd.Wd.Sdng"   ,"Exterior2nd.Wd.Shng"  ,
                    "MasVnrType..MISSING."  ,"MasVnrType.BrkCmn"     ,"MasVnrType.Stone"      ,"ExterCond"             ,"Foundation.Slab"      ,
                    "Foundation.Stone"      ,"Foundation.Wood"       ,"BsmtFinSF2"            ,"Heating.Floor"         ,"Heating.GasA"         ,
                    "Heating.GasW"          ,"Heating.Grav"          ,"Heating.OthW"          ,"Heating.Wall"          ,"Electrical.Other"     ,
                    "Electrical.SBrkr"      ,"LowQualFinSF"          ,"BsmtHalfBath"          ,"GarageType.2Types"     ,"GarageType.Basment"   ,
                    "GarageType.BuiltIn"    ,"GarageType.CarPort"    ,"EnclosedPorch"         ,"ScreenPorch"           ,"PoolArea"             ,
                    "PoolQC"                ,"Fence..MISSING."       ,"Fence.GdPrv"           ,"Fence.GdWo"            ,"Fence.MnWw"           ,
                    "MiscFeature..MISSING." ,"MiscFeature.Gar2"      ,"MiscFeature.Othr"      ,"MiscFeature.Shed"      ,"MiscFeature.TenC"     ,
                    "MiscVal"               ,"MoSold"                ,"YrSold"                ,"SaleType..MISSING."    ,"SaleType.COD"         ,
                    "SaleType.Con"          ,"SaleType.ConLD"        ,"SaleType.ConLI"        ,"SaleType.ConLw"        ,"SaleType.CWD"         ,
                    "SaleType.Oth"          ,"SaleType.WD"           ,"SaleCondition.Abnorml" ,"SaleCondition.AdjLand" ,"SaleCondition.Alloca" ,
                    "SaleCondition.Family"  ,"SaleCondition.Normal"  ,"SaleCondition.Partial" ,"Porch3Ssn"     )


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

RF_PARS <- list(ntree = 800,
                mtry = 50)

KNN3_PARS <- expand.grid(k = 3)
KNN7_PARS <- expand.grid(k = 7)
KNN10_PARS <- expand.grid(k = 10)

XGB_CARET_TUNE_GRID <-  expand.grid(nrounds=seq(100,1200, by = 100), 
                                    eta=c(0.01,0.03,0.05,0.1,0.3),
                                    max_depth = c(1, 3, 5, 7, 10, 15),
                                    colsample_bytree = c(.33,.66,.90),
                                    min_child_weight = c(1, 5, 10, 20),
                                    gamma = c(0,0.1,1))

RF_CARET_TUNE_GRID <- expand.grid(mtry = c(15, 20, 25, 30, 40, 50, 60, 80, 100, 130, 160))


KNN_CARET_TUNE_GRID <- expand.grid(k = c(1, 2, 3, 5, 7, 10))


