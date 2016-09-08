# Load Data ---------------------------------------------------------------

# load libraries and functions needed
source("utils.R")

raw.train <- read_csv("train.csv")
raw.test <- read_csv("test.csv")
raw.all <- bind_rows(raw.train, raw.test)

# Data Cleaning -----------------------------------------------------------

#first, get an idea for predictors with lots of missing data
# for (i in 1:ncol(ord.mat)) {
#   na.frac <- sum(is.na(ord.mat[,i]) == TRUE)/nrow(ord.mat)
#   colname <- colnames(ord.mat)[i]
#   print(c(colname,na.frac))
# }

# Replacing missing values (courtesy of JMT5802)
# Determine data types in the data set
data_types <- sapply(PREDICTOR_ATTR,function(x){class(raw.all[[x]])})
unique_data_types <- unique(data_types)

DATA_ATTR_TYPES <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(DATA_ATTR_TYPES) <- unique_data_types

num_attr <- intersect(PREDICTOR_ATTR,DATA_ATTR_TYPES$integer)
for (x in num_attr){
  raw.all[[x]][is.na(raw.all[[x]])] <- -1
}

char_attr <- intersect(PREDICTOR_ATTR,DATA_ATTR_TYPES$character)
for (x in char_attr){
  raw.all[[x]][is.na(raw.all[[x]])] <- "*MISSING*"
  #raw.all[[x]] <- factor(raw.all[[x]])
}

# data modifications based on EDA
dat.all <- raw.all
dat.all$MSSubClass[dat.all$MSSubClass == 150] <- 50
dat.all$MSSubClass[dat.all$MSSubClass == 45] <- 50


#preserving ordinal rankings as much as possible
dat.ord <- mutate(dat.all, 
                  MSSubClass = as.factor(MSSubClass),
                  MSZoning = as.factor(MSZoning),
                  Street = as.factor(Street),
                  Alley = as.factor(Alley),
                  LotShape = as.numeric(factor(LotShape, levels = c("Reg", "IR1", "IR2", "IR3"), ordered = TRUE)),
                  Utilities = as.numeric(factor(Utilities, levels = c("AllPub", "NoSewr", "NoSeWa", "ELO"), ordered = TRUE)),
                  LotConfig = as.factor(LotConfig),
                  LandSlope = as.numeric(factor(LandSlope, levels = c("Gtl", "Mod", "Sev"), ordered = TRUE)),
                  Neighborhood = as.factor(Neighborhood),
                  Condition1 = as.factor(Condition1), # these two should probably be done as one-hot encoding
                  Condition2 = as.factor(Condition2),
                  BldgType = as.factor(BldgType),
                  HouseStyle = as.factor(HouseStyle), #should probably be ordinal
                  OverallQual = as.numeric(factor(OverallQual, ordered = TRUE)),
                  OverallCond = as.numeric(factor(OverallCond, ordered = TRUE)), 
                  RoofStyle = as.factor(RoofStyle),
                  RoofMatl = as.factor(RoofMatl),
                  Exterior1st = as.factor(Exterior1st), # these two should probably be done as one-hot encoding
                  Exterior2nd = as.factor(Exterior2nd),
                  MasVnrType = as.factor(MasVnrType),
                  ExterQual = as.numeric(factor(ExterQual, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE)),
                  ExterCond = as.numeric(factor(ExterCond, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE)),
                  Foundation = as.factor(Foundation),
                  BsmtQual = as.numeric(factor(BsmtQual, levels = c("Ex", "Gd", "TA", "Fa", "Po", "*MISSING*"), ordered = TRUE)),
                  BsmtCond = as.numeric(factor(BsmtCond, levels = c("Ex", "Gd", "TA", "Fa", "Po", "*MISSING*"), ordered = TRUE)),
                  BsmtExposure = as.numeric(factor(BsmtExposure, levels = c("Gd", "Av", "Mn", "No", "*MISSING*"), ordered = TRUE)),
                  BsmtFinType1 = as.numeric(factor(BsmtFinType1, levels = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", "*MISSING*"), ordered = TRUE)),
                  BsmtFinSF1 = ifelse(is.na(BsmtQual) == TRUE, 0, BsmtFinSF1),
                  BsmtFinType2 = as.numeric(factor(BsmtFinType2, levels = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", "*MISSING*"), ordered = TRUE)),
                  BsmtFinSF2 = ifelse(is.na(BsmtQual) == TRUE, 0, BsmtFinSF2),
                  BsmtUnfSF = ifelse(is.na(BsmtQual) == TRUE, 0, BsmtUnfSF),
                  TotalBsmtSF = ifelse(is.na(BsmtQual) == TRUE, 0, TotalBsmtSF),
                  Heating = as.factor(Heating),
                  LandContour = as.factor(LandContour),
                  GarageCond = as.numeric(factor(GarageCond, levels = c("Ex", "Gd", "TA", "Fa", "Po", "*MISSING*"), ordered = TRUE)),
                  HeatingQC = as.numeric(factor(HeatingQC, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE)),
                  CentralAir = as.factor(CentralAir),
                  Electrical = as.numeric(factor(Electrical, levels = c("SBrkr", "FuseA", "FuseF", "FuseP", "Mix", "*MISSING*"), ordered = TRUE)), # may want to revisit (not sure if Mixed should be ranked last)
                  SF2ndFlr = `2ndFlrSF`,
                  SF1stFlr = `1stFlrSF`,
                  BsmtFullBath = ifelse(is.na(BsmtQual) == TRUE, 0, BsmtFullBath),
                  BsmtHalfBath = ifelse(is.na(BsmtQual) == TRUE, 0, BsmtHalfBath),
                  KitchenQual = as.numeric(factor(KitchenQual, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE)),
                  Functional = as.numeric(factor(Functional, levels = c("Typ", "Min1", "Min2", "Mod", "Maj1", "Maj2", "Sev", "Sal"), ordered = TRUE)),
                  FireplaceQu = as.numeric(factor(FireplaceQu, levels = c("Ex", "Gd", "TA", "Fa", "Po", "*MISSING*"), ordered = TRUE)),
                  GarageType = as.factor(GarageType),
                  GarageFinish = as.factor(GarageFinish),
                  GarageArea = ifelse(is.na(GarageType) == TRUE, 0, GarageArea),
                  GarageQual = as.numeric(factor(GarageQual, levels = c("Ex", "Gd", "TA", "Fa", "Po", "*MISSING*"), ordered = TRUE)),
                  PavedDrive = as.numeric(factor(PavedDrive, levels = c("Y", "P", "N"), ordered = TRUE)),
                  PoolQC = as.numeric(factor(PoolQC, levels = c("Ex", "Gd", "TA", "Fa", "*MISSING*"), ordered = TRUE)),
                  Fence = as.numeric(factor(Fence, levels = c("GdPrv", "MnPrv", "GdWo", "MnWw", "*MISSING*"), ordered = TRUE)), #should revisit this for sure
                  MiscFeature = as.factor(MiscFeature),
                  SaleType = as.factor(SaleType),
                  Porch3Ssn = `3SsnPorch`,
                  SaleCondition = as.factor(SaleCondition)) %>%
  select(everything(), -`1stFlrSF`, -`2ndFlrSF`, -`3SsnPorch`)

#applying OHE as much as possible
dat.ohe <- mutate(dat.all, 
                  MSSubClass = as.factor(MSSubClass),
                  MSZoning = as.factor(MSZoning),
                  Street = as.factor(Street),
                  Alley = as.factor(Alley),
                  LotShape = as.factor(LotShape),
                  Utilities = as.factor(Utilities),
                  LotConfig = as.factor(LotConfig),
                  LandSlope = as.factor(LandSlope),
                  Neighborhood = as.factor(Neighborhood),
                  Condition1 = as.factor(Condition1), # these two should probably be done as one-hot encoding
                  Condition2 = as.factor(Condition2),
                  BldgType = as.factor(BldgType),
                  HouseStyle = as.factor(HouseStyle), #should probably be ordinal
                  OverallQual = as.factor(OverallQual),
                  OverallCond = as.factor(OverallCond), 
                  RoofStyle = as.factor(RoofStyle),
                  RoofMatl = as.factor(RoofMatl),
                  Exterior1st = as.factor(Exterior1st), # these two should probably be done as one-hot encoding
                  Exterior2nd = as.factor(Exterior2nd),
                  MasVnrType = as.factor(MasVnrType),
                  ExterQual = as.factor(ExterQual),
                  ExterCond = as.factor(ExterCond),
                  Foundation = as.factor(Foundation),
                  BsmtQual = as.factor(BsmtQual),
                  BsmtCond = as.factor(BsmtCond),
                  BsmtExposure = as.factor(BsmtExposure),
                  BsmtFinType1 = as.factor(BsmtFinType1),
                  BsmtFinSF1 = ifelse(is.na(BsmtQual) == TRUE, 0, BsmtFinSF1),
                  BsmtFinType2 = as.factor(BsmtFinType2),
                  BsmtFinSF2 = ifelse(is.na(BsmtQual) == TRUE, 0, BsmtFinSF2),
                  BsmtUnfSF = ifelse(is.na(BsmtQual) == TRUE, 0, BsmtUnfSF),
                  TotalBsmtSF = ifelse(is.na(BsmtQual) == TRUE, 0, TotalBsmtSF),
                  Heating = as.factor(Heating),
                  LandContour = as.factor(LandContour),
                  GarageCond = as.factor(GarageCond),
                  HeatingQC = as.factor(HeatingQC),
                  CentralAir = as.factor(CentralAir),
                  Electrical = as.factor(Electrical), 
                  SF2ndFlr = `2ndFlrSF`,
                  SF1stFlr = `1stFlrSF`,
                  BsmtFullBath = ifelse(is.na(BsmtQual) == TRUE, 0, BsmtFullBath),
                  BsmtHalfBath = ifelse(is.na(BsmtQual) == TRUE, 0, BsmtHalfBath),
                  KitchenQual = as.factor(KitchenQual),
                  Functional = as.factor(Functional),
                  FireplaceQu = as.factor(FireplaceQu),
                  GarageType = as.factor(GarageType),
                  GarageFinish = as.factor(GarageFinish),
                  GarageArea = ifelse(is.na(GarageType) == TRUE, 0, GarageArea),
                  GarageQual = as.factor(GarageQual),
                  PavedDrive = as.factor(PavedDrive),
                  PoolQC = as.factor(PoolQC),
                  Fence = as.factor(Fence), #should revisit this for sure
                  MiscFeature = as.factor(MiscFeature),
                  SaleType = as.factor(SaleType),
                  Porch3Ssn = `3SsnPorch`,
                  SaleCondition = as.factor(SaleCondition)) %>%
  select(everything(), -`1stFlrSF`, -`2ndFlrSF`, -`3SsnPorch`)


# table(dat.all$HouseStyle)
# 
# ggplot(data = dat.all, aes(x = GrLivArea, y = SalePrice)) +
#   geom_point()






