library(readr)
library(dplyr)
library(caret)
library(ggplot2)


# Functions ---------------------------------------------------------------

# clean up the features from the original dateset
fixFeatures <- function(base.data) {
  new <- mutate(base.data, 
                MSSubClass = as.factor(MSSubClass),
                MSZoning = as.factor(MSZoning),
                Street = as.factor(Street),
                Alley = as.factor(Alley),
                LotShape = factor(LotShape, levels = c("Reg", "IR1", "IR2", "IR3"), ordered = TRUE),
                Utilities = factor(Utilities, levels = c("AllPub", "NoSewr", "NoSeWa", "ELO"), ordered = TRUE),
                LotConfig = as.factor(LotConfig),
                LandSlope = factor(LandSlope, levels = c("Gtl", "Mod", "Sev"), ordered = TRUE),
                Neighborhood = as.factor(Neighborhood),
                Condition1 = as.factor(Condition1), # these two should probably be done as one-hot encoding
                Condition2 = as.factor(Condition2),
                BldgType = as.factor(BldgType),
                HouseStyle = as.factor(HouseStyle), #should probably be ordinal
                OverallQual = factor(OverallQual, ordered = TRUE),
                OverallCond = factor(OverallCond, ordered = TRUE), 
                RoofStyle = as.factor(RoofStyle),
                RoofMatl = as.factor(RoofMatl),
                Exterior1st = as.factor(Exterior1st), # these two should probably be done as one-hot encoding
                Exterior2nd = as.factor(Exterior2nd),
                MasVnrType = as.factor(MasVnrType),
                ExterQual = factor(ExterQual, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE),
                ExterCond = factor(ExterCond, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE),
                Foundation = as.factor(Foundation),
                BsmtQual = factor(BsmtQual, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE),
                BsmtCond = factor(BsmtCond, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE),
                BsmtExposure = factor(BsmtExposure, levels = c("Gd", "Av", "Mn", "No"), ordered = TRUE),
                BsmtFinType1 = factor(BsmtFinType1, levels = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf"), ordered = TRUE),
                BsmtFinSF1 = ifelse(is.na(BsmtQual) == TRUE, NA, BsmtFinSF1),
                BsmtFinType2 = factor(BsmtFinType2, levels = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf"), ordered = TRUE),
                BsmtFinSF2 = ifelse(is.na(BsmtQual) == TRUE, NA, BsmtFinSF2),
                BsmtUnfSF = ifelse(is.na(BsmtQual) == TRUE, NA, BsmtUnfSF),
                TotalBsmtSF = ifelse(is.na(BsmtQual) == TRUE, NA, TotalBsmtSF),
                Heating = as.factor(Heating),
                HeatingQC = factor(HeatingQC, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE),
                CentralAir = as.factor(CentralAir),
                Electrical = factor(Electrical, levels = c("Sbrkr", "FuseA", "FuseF", "FuseP", "Mix"), ordered = TRUE), # may want to revisit (not sure if Mixed should be ranked last)
                SF2ndFlr = ifelse(`2ndFlrSF` == 0, NA, `2ndFlrSF`),
                SF1stFlr = `1stFlrSF`,
                BsmtFullBath = ifelse(is.na(BsmtQual) == TRUE, NA, BsmtFullBath),
                BsmtHalfBath = ifelse(is.na(BsmtQual) == TRUE, NA, BsmtHalfBath),
                KitchenQual = factor(KitchenQual, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE),
                Functional = factor(Functional, levels = c("Typ", "Min1", "Min2", "Mod", "Maj1", "Maj2", "Sev", "Sal"), ordered = TRUE),
                FireplaceQu = factor(FireplaceQu, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE),
                GarageType = as.factor(GarageType),
                GarageFinish = as.factor(GarageFinish),
                GarageArea = ifelse(is.na(GarageType) == TRUE, NA, GarageArea),
                GarageQual = factor(GarageQual, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE),
                PavedDrive = factor(PavedDrive, levels = c("Y", "P", "N"), ordered = TRUE),
                PoolQC = factor(KitchenQual, levels = c("Ex", "Gd", "TA", "Fa"), ordered = TRUE),
                Fence = factor(Fence, levels = c("GdPrv", "MnPrv", "GdWo", "MnWw"), ordered = TRUE), #should revisit this for sure
                MiscFeature = as.factor(MiscFeature),
                SaleType = as.factor(SaleType),
                SaleCondition = as.factor(SaleCondition)
                ) %>%
    select(everything(), -`2ndFlrSF`, -`1stFlrSF`)
}



# Load Data ---------------------------------------------------------------

dat <- read_csv("train.csv")

ohe.feats <- c("MSSubClass")
arp <- select(dat, LotShape, SalePrice, MSSubClass) %>%
  mutate(MSSubClass = as.factor(MSSubClass),
         LotShape = factor(LotShape, levels = c("Reg", "IR1", "IR2", "IR3"), ordered = TRUE))
test <- as.numeric(arp$LotShape) # preserve ordinal encoding

dummies <- dummyVars(~ MSSubClass, data = arp) # one hot encoding


# TODO --------------------------------------------------------------------

# go through all features, update feature cleaning function as needed. Create two datasets to run through xgboost.
# First dataset using all one hot Encoding and second dataset preserving ordinal encoding for ordinal vars. Potential 
# third dataset: use combination of one-hot encoding and ordinals (i.e. has basement then ordinal var corresponding to quality) in order to
# preserve interaction terms (gained from OHE) as well as ordinal pwr from keeping ordinal vars

# then, EDA - consider APM techniques as well as the stuff in forked script. then build and CV xgboost models, then start tuning ensemble



arp.ohe <- as.data.frame(predict(dummies, newdata = arp))
arp.comb <- cbind(arp[, -c(which(colnames(arp) %in% ohe.feats))], arp.ohe)
test <- sparse.model.matrix(~., data = arp.comb)


test <- as.matrix(arp, rownames.force = NA)
test2 <- as(test, "sparseMatrix")



ohe.feats = c('AnimalType', 'SexuponOutcome', 'named', 'is.mix', 'simple.breed', 'simple.color', 
              'kittypalooza', 'month', 'weekday', 'day.of.week')
dummies <- dummyVars(~ AnimalType + SexuponOutcome + named + is.mix + simple.breed + simple.color + 
                       kittypalooza + month + weekday + day.of.week, data = shelter.mod)
shelter.ohe <- as.data.frame(predict(dummies, newdata = shelter.mod))
shelter.combined <- cbind(shelter.mod[, -c(which(colnames(shelter.mod) %in% ohe.feats))], shelter.ohe)
shelter.combined <- mutate(shelter.combined, date = as.integer(date)) %>%
  select(everything(), -OutcomeSubtype, -AgeuponOutcome, -Breed, -Color, -DateTime)

