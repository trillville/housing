# Load Data ---------------------------------------------------------------
raw.train <- read_csv("train.csv")
raw.test <- read_csv("test.csv")
raw.all <- bind_rows(raw.train, raw.test)

# Data Cleaning -----------------------------------------------------------


# Replacing missing values (courtesy of JMT5802)
# characters = *MISSING* 
# numeric = -1
dat.all <- raw.all

data_types <- sapply(colnames(raw.all),function(x){class(dat.all[[x]])})
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
                  MSSubClass = as.factor(MSSubClass),
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

# applying OHE as much as possible (doesn't appear to be helpful - dropping for now)
dat.ohe <- mutate(dat.all, 
                  OverallQual = as.numeric(factor(OverallQual, ordered = TRUE)),
                  OverallCond = as.numeric(factor(OverallCond, ordered = TRUE))
) 

# convert characters to factors (need to cleanup this part)
data_types <- sapply(colnames(dat.ord),function(x){class(dat.ord[[x]])})
unique_data_types <- unique(data_types)

DATA_ATTR_TYPES <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(DATA_ATTR_TYPES) <- unique_data_types
char_attr <- DATA_ATTR_TYPES$character
for (x in char_attr){
  dat.ord[[x]] <- factor(dat.ord[[x]])
}

data_types <- sapply(colnames(dat.ohe),function(x){class(dat.ohe[[x]])})
unique_data_types <- unique(data_types)

DATA_ATTR_TYPES <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(DATA_ATTR_TYPES) <- unique_data_types
char_attr <- DATA_ATTR_TYPES$character
for (x in char_attr){
  dat.ohe[[x]] <- factor(dat.ohe[[x]])
}

# Prepare data for models -------------------------------------------------
test <- which(dat.all$SalePrice == -1)
train <- setdiff(1:nrow(dat.all), test)

y.train <- dat.all$SalePrice[train]
Id.train <- dat.all$Id[train]
Id.test <- dat.all$Id[test]

dat.ord <- select(dat.ord, -Id, -SalePrice)
dat.ohe <- select(dat.ohe, -Id, -SalePrice)

ord.dummies <- dummyVars(~ ., dat.ord)
ohe.dummies <- dummyVars(~ ., dat.ohe)
ord.m <- predict(ord.dummies, dat.ord)
ohe.m <- predict(ohe.dummies, dat.ohe)

ord.train.m <- ord.m[train, ]
ord.test.m <- ord.m[test, ]
ohe.train.m <- ohe.m[train, ]
ohe.test.m <- ohe.m[test, ]

# TSNE --------------------------------------------------------------------

use.tsne <- FALSE # makes model worse apparently
if (use.tsne == TRUE) {
  tsne <- Rtsne(as.matrix(ohe.all), check_duplicates = FALSE, pca = FALSE, 
                perplexity=25, theta=0.1, dims=2)
  tsne.df <- data.frame(cbind(tsne$Y[train,], y.train/1000))
  qplot(tsne.df$X1, tsne.df$X2, data = tsne.df, color = tsne.df$X3) +
    scale_colour_gradient(limits=c(34.90, 350))#,low="red",high="white")
  
  colnames(tsne$Y) <- c("TSNE1", "TSNE2")
  # need to determine whether or not it makes sense to do TSNE for both OHE and ORD encodings
  ord.train.m <- cbind(ord.train.m, tsne$Y[train, ])
  ord.test.m <- cbind(ord.test.m, tsne$Y[test, ])
  ohe.train.m <- cbind(ohe.train.m, tsne$Y[train, ])
  ohe.test.m <- cbind(ohe.test.m, tsne$Y[test, ])
}


# Center, scale, and drop outliers
pp <- preProcess(ord.m, method = c("center", "scale"), na.remove = FALSE)
ord.m.pp <- predict(pp, as.matrix(ord.m))
ord.train.pp <- ord.m.pp[train, ]
ord.test.pp <- ord.m.pp[test, ]


# Pointers to xgb matrices
ord.train.xgb <- xgb.DMatrix(data = ord.train.m, label = y.train)
ord.test.xgb <- xgb.DMatrix(data = ord.train.m)

ohe.train.xgb <- xgb.DMatrix(data = ohe.train.m, label = y.train)
ohe.test.xgb <- xgb.DMatrix(data = ohe.train.m)

#options(na.action=previous_na_action$na.action)
# MISC --------------------------------------------------------------------

# check for predictors with missing data
# for (i in 1:ncol(dat.ord)) {
#   na.frac <- sum(is.na(dat.ord[,i]) == TRUE)/nrow(dat.ord)
#   colname <- colnames(dat.ord)[i]
#   print(c(colname,na.frac))
# }