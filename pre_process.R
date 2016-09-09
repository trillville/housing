# Load Data ---------------------------------------------------------------

raw.train <- read_csv("train.csv")
raw.test <- read_csv("test.csv")
raw.all <- bind_rows(raw.train, raw.test)

# Data Cleaning -----------------------------------------------------------


# Replacing missing values (courtesy of JMT5802)
# characters = *MISSING* 
# numeric = -1
dat.all <- raw.all

data_types <- sapply(ALL_ATTR,function(x){class(dat.all[[x]])})
unique_data_types <- unique(data_types)

DATA_ATTR_TYPES <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(DATA_ATTR_TYPES) <- unique_data_types

num_attr <- DATA_ATTR_TYPES$integer
for (x in num_attr){
  dat.all[[x]][is.na(dat.all[[x]])] <- -1
}

char_attr <- DATA_ATTR_TYPES$character
for (x in char_attr){
  dat.all[[x]][is.na(dat.all[[x]])] <- "*MISSING*"
  #dat.all[[x]] <- factor(dat.all[[x]])
}

# data modifications based on EDA
dat.all$MSSubClass[which(dat.all$MSSubClass == 150)] <- 50
dat.all$MSSubClass[which(dat.all$MSSubClass == 45)] <- 50
dat.all$Condition2[which(dat.all$Condition1 == dat.all$Condition2)] <- "Norm"
dat.all$KitchenQual[which(dat.all$KitchenQual == "*MISSING*")] <- "TA" # only one house, based on overallqual looks like TA most likely value
dat.all$GarageCars[which(dat.all$GarageCars == -1)] <- 0 
dat.all$GarageArea[which(dat.all$GarageArea == -1)] <- 0 
#dat.all$FenceQual <- ifelse(dat.all$Fence == "GdPrv" | dat.all$Fence == "GdWo", "Gd", # need to check if this replacement improves performance
#                            ifelse(dat.all$Fence == "*MISSING*", "*MISSING*", "Po"))
dat.all$Electrical[which(dat.all$Electrical !="SBrkr")] <- "Other"
dat.all$Functional[which(dat.all$Functional =="*MISSING*")] <- "Typ" # may not be correct

dat.all <- mutate(dat.all, SF2ndFlr = `2ndFlrSF`, SF1stFlr = `1stFlrSF`, Porch3Ssn = `3SsnPorch`) %>%
  select(everything(), -`1stFlrSF`, -`2ndFlrSF`, -`3SsnPorch`)

# drop outliers (using https://www.kaggle.com/mshih2/house-prices-advanced-regression-techniques/using-xgboost-for-feature-selection/notebook)
# should check independently
# outlier.ids <- c(30,462,523,632,968,970, 1298, 1324)
# dat.all <- dat.all[-outlier.ids, ]

# preserving ordinal rankings as much as possible
dat.ord <- mutate(dat.all, 
                  Utilities = as.numeric(factor(Utilities, levels = c("AllPub", "NoSewr", "NoSeWa", "ELO", "*MISSING*"), ordered = TRUE)),
                  LandSlope = as.numeric(factor(LandSlope, levels = c("Gtl", "Mod", "Sev"), ordered = TRUE)),
                  OverallQual = as.numeric(factor(OverallQual, ordered = TRUE)),
                  OverallCond = as.numeric(factor(OverallCond, ordered = TRUE)), 
                  ExterQual = as.numeric(factor(ExterQual, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE)),
                  ExterCond = as.numeric(factor(ExterCond, levels = c("Ex", "Gd", "TA", "Fa", "Po"), ordered = TRUE)),
                  BsmtQual = as.numeric(factor(BsmtQual, levels = c("Ex", "Gd", "TA", "Fa", "Po", "*MISSING*"), ordered = TRUE)),
                  BsmtCond = as.numeric(factor(BsmtCond, levels = c("Ex", "Gd", "TA", "Fa", "Po", "*MISSING*"), ordered = TRUE)),
                  BsmtExposure = as.numeric(factor(BsmtExposure, levels = c("Gd", "Av", "Mn", "No", "*MISSING*"), ordered = TRUE)),
                  BsmtFinType1 = as.numeric(factor(BsmtFinType1, levels = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", "*MISSING*"), ordered = TRUE)),
                  BsmtFinType2 = as.numeric(factor(BsmtFinType2, levels = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", "*MISSING*"), ordered = TRUE)),
                  GarageCond = as.numeric(factor(GarageCond, levels = c("Ex", "Gd", "TA", "Fa", "Po", "*MISSING*"), ordered = TRUE)),
                  HeatingQC = as.numeric(factor(HeatingQC, levels = c("Ex", "Gd", "TA", "Fa", "Po", "*MISSING*"), ordered = TRUE)),
                  KitchenQual = as.numeric(factor(KitchenQual, levels = c("Ex", "Gd", "TA", "Fa", "Po", "*MISSING*"), ordered = TRUE)),
                  Functional = as.numeric(factor(Functional, levels = c("Typ", "Min1", "Min2", "Mod", "Maj1", "Maj2", "Sev", "Sal", "*MISSING*"), ordered = TRUE)),
                  FireplaceQu = as.numeric(factor(FireplaceQu, levels = c("Ex", "Gd", "TA", "Fa", "Po", "*MISSING*"), ordered = TRUE)),
                  GarageQual = as.numeric(factor(GarageQual, levels = c("Ex", "Gd", "TA", "Fa", "Po", "*MISSING*"), ordered = TRUE)),
                  PavedDrive = as.numeric(factor(PavedDrive, levels = c("Y", "P", "N"), ordered = TRUE)),
                  PoolQC = as.numeric(factor(PoolQC, levels = c("Ex", "Gd", "TA", "Fa", "*MISSING*"), ordered = TRUE))
                  #FenceQual = as.numeric(factor(FenceQual, levels = c("Gd", "Po", "*MISSING*"), ordered = TRUE)) #should revisit this for sure
) 

# applying OHE as much as possible
dat.ohe <- mutate(dat.all, 
                  OverallQual = as.numeric(factor(OverallQual, ordered = TRUE)),
                  OverallCond = as.numeric(factor(OverallCond, ordered = TRUE))
) 

# convert characters to factors (need to cleanup this part)
data_types <- sapply(ALL_ATTR,function(x){class(dat.ord[[x]])})
unique_data_types <- unique(data_types)

DATA_ATTR_TYPES <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(DATA_ATTR_TYPES) <- unique_data_types
char_attr <- DATA_ATTR_TYPES$character
for (x in char_attr){
  dat.ord[[x]] <- factor(dat.ord[[x]])
}

data_types <- sapply(ALL_ATTR,function(x){class(dat.ohe[[x]])})
unique_data_types <- unique(data_types)

DATA_ATTR_TYPES <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(DATA_ATTR_TYPES) <- unique_data_types
char_attr <- DATA_ATTR_TYPES$character
for (x in char_attr){
  dat.ohe[[x]] <- factor(dat.ohe[[x]])
}

# Prepare data for models -------------------------------------------------
test <- which(is.na(dat.all$SalePrice))
train <- setdiff(1:nrow(dat.all), test)

y.train <- dat.all$SalePrice[train]
Id.train <- dat.all$Id[train]
Id.test <- dat.all$Id[test]

dat.ord <- dat.ord[, PREDICTOR_ATTR]
dat.ohe <- dat.ohe[, PREDICTOR_ATTR]

# workaround to get sparse.model.matrix to work with NAs - FIXED
#previous_na_action <- options('na.action')
#options(na.action='na.pass')

ord.train.s <- sparse.model.matrix(~ . -1, data = dat.ord[train, ])
ord.train.xgb <- xgb.DMatrix(data = ord.train.s, label = y.train)
ord.test.s <- sparse.model.matrix(~ . -1, data = dat.ord[test, ]) 
ord.test.xgb <- xgb.DMatrix(data = ord.train.s)

ohe.train.s <- sparse.model.matrix(~ . -1, data = dat.ohe[train, ])
ohe.train.xgb <- xgb.DMatrix(data = ohe.train.s, label = y.train)
ohe.test.s <- sparse.model.matrix(~ . -1, data = dat.ohe[test, ]) 
ohe.test.xgb <- xgb.DMatrix(data = ohe.train.s)


# TSNE --------------------------------------------------------------------

tsne <- Rtsne(as.matrix(dat.ord), check_duplicates = FALSE, pca = FALSE, 
              perplexity=25, theta=0.5, dims=2)



#options(na.action=previous_na_action$na.action)
# MISC --------------------------------------------------------------------

# check for predictors with missing data
# for (i in 1:ncol(dat.ord)) {
#   na.frac <- sum(is.na(dat.ord[,i]) == TRUE)/nrow(dat.ord)
#   colname <- colnames(dat.ord)[i]
#   print(c(colname,na.frac))
# }