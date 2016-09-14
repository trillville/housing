# Load Data ---------------------------------------------------------------
raw.train <- read_csv("train.csv")
raw.test <- read_csv("test.csv")
raw.all <- bind_rows(raw.train, raw.test)

# Data Cleaning -----------------------------------------------------------

dat.all <- raw.all

dat.all$MSSubClass <- as.character(dat.all$MSSubClass)
date <- dat.all$YrSold + (dat.all$MoSold - 1)/12
dat.all$YrSold <- date

data_types <- sapply(colnames(raw.all),function(x){class(dat.all[[x]])})
unique_data_types <- unique(data_types)

DATA_ATTR_TYPES <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(DATA_ATTR_TYPES) <- unique_data_types

num.attr <- DATA_ATTR_TYPES$integer

# preprocess numeric variables
raw.num <- raw.all[, num.attr]
# col 1 = ID, col 37 = Saleprice (convert this to a simple log)
pp <- preProcess(raw.num[, -c(1,37)], method = c("center", "scale"), na.action = na.pass)
pp.num <- predict(pp, newdata = raw.num)
pp.num$SalePrice <- log(pp.num$SalePrice)
pp.num[is.na(pp.num)] <- -1


char.attr <- DATA_ATTR_TYPES$character
raw.char <- raw.all[, char.attr]
raw.char[is.na(raw.char)] <- "*MISSING*"
dat.all <- cbind(pp.num, raw.char)

# data modifications based on EDA
dat.all$MSSubClass[which(dat.all$MSSubClass == "150")] <- "50"
dat.all$MSSubClass[which(dat.all$MSSubClass == "45")] <- "50"
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


# convert characters to factors (need to cleanup this part)
data_types <- sapply(colnames(dat.ord),function(x){class(dat.ord[[x]])})
unique_data_types <- unique(data_types)

DATA_ATTR_TYPES <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(DATA_ATTR_TYPES) <- unique_data_types
char_attr <- DATA_ATTR_TYPES$character
for (x in char_attr){
  dat.ord[[x]] <- factor(dat.ord[[x]])
}

#impute missing values for LotFrontage
frontage.vars = c('LotArea','LotShape','LandContour','LandSlope','LotConfig','LotFrontage', 'Street', 
                  'Alley', 'PavedDrive')
tmp <- dat.ord[, frontage.vars]
tmp.test <- which(tmp$LotFrontage == -1)
tmp.train <- setdiff(1:nrow(dat.ord), tmp.test)
tmp.y <- dat.ord$LotFrontage
tmp.dummies <- dummyVars(LotFrontage~ ., tmp)
tmp.m <- predict(tmp.dummies, tmp)

tmp.xgb <- xgboost(data = tmp.m[tmp.train, ], label = tmp.y[tmp.train],
                   objective = "reg:linear", eval_metric = "rmse", 
                   nrounds = 50, max_depth = 4, eta = 0.3, gamma = 0,
                   colsample_bytree = 0.8, min_child_weight = 1)
dat.ord$LotFrontage[tmp.test] <- predict(tmp.xgb, newdata = tmp.m[tmp.test, ])

# Prepare data for models -------------------------------------------------
test <- which(dat.all$SalePrice == -1)
train <- setdiff(1:nrow(dat.all), test)

y.train <- dat.all$SalePrice[train]
Id.train <- dat.all$Id[train]
Id.test <- dat.all$Id[test]

dat.ord <- select(dat.ord, -Id, -SalePrice)

ord.dummies <- dummyVars(~ ., dat.ord)
ord.m <- predict(ord.dummies, dat.ord)
ord.b.m <- as.matrix(data.frame(ord.m)[, PREDICTOR_ATTR])

ord.train.m <- ord.m[train, ]
ord.test.m <- ord.m[test, ]
ord.train.b.m <- ord.b.m[train, ]
ord.test.b.m <- ord.b.m[test, ]

# TSNE --------------------------------------------------------------------

use.tsne <- FALSE # makes model worse apparently
if (use.tsne == TRUE) {
  tsne <- Rtsne(ord.m, check_duplicates = FALSE, pca = FALSE, 
                perplexity=25, theta=0.1, dims=2)
  tsne.df <- data.frame(cbind(tsne$Y[train,], y.train))
  qplot(tsne.df$TSNE1, tsne.df$TSNE2, data = tsne.df, color = tsne.df$y.train) +
    scale_colour_gradient(limits=c(34.90, 350))#,low="red",high="white")
  
  colnames(tsne$Y) <- c("TSNE1", "TSNE2")
  # need to determine whether or not it makes sense to do TSNE for both OHE and ORD encodings
  # ord.train.m <- cbind(ord.train.m, tsne$Y[train, ])
  # ord.test.m <- cbind(ord.test.m, tsne$Y[test, ])
}

# Pointers to xgb matrices
ord.train.xgb <- xgb.DMatrix(data = ord.train.m, label = y.train)
ord.test.xgb <- xgb.DMatrix(data = ord.train.m)


#options(na.action=previous_na_action$na.action)
# MISC --------------------------------------------------------------------

# check for predictors with missing data
# for (i in 1:ncol(dat.ord)) {
#   na.frac <- sum(is.na(dat.ord[,i]) == TRUE)/nrow(dat.ord)
#   colname <- colnames(dat.ord)[i]
#   print(c(colname,na.frac))
# }