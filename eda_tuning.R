# Prepare data for models -------------------------------------------------

dat.train <- filter(dat.all, !is.na(SalePrice))
y.train <- dat.train$SalePrice
Id.train <- dat.train$Id

ord.train <- filter(dat.ord, !is.na(SalePrice))
ohe.train <- filter(dat.ohe, !is.na(SalePrice))

# workaround to get sparse.model.matrix to work with NAs - REVIST
previous_na_action <- options('na.action')
options(na.action='na.pass')
ord.mat <- sparse.model.matrix(SalePrice ~ . -1 -Id, data = ord.train) # sparse matrix for xgboost
ohe.mat <- sparse.model.matrix(SalePrice ~ . -1 -Id, data = ohe.train) # sparse matrix for xgboost
options(na.action=previous_na_action$na.action)

set.seed(13)
cv.folds <- createFolds(y.train, k=5)

# Feature Importance Chart ------------------------------------------------------

# start by fitting xgb on the entire dataset (to see var imp)

param <- list("objective" = "reg:linear",
              "eval_metric" = "rmse")
nround <- 50
xg1 <- xgboost(param = param, data = ord.mat, label = y.train, nrounds = nround)

#xg1.info <- xgb.dump(xg1, with.stats = TRUE)
feature.names <- dimnames(mat.train)[[2]]
importance <- xgb.importance(feature.names, model = xg1)
unused.features <- setdiff(colnames(mat.train), importance$Feature)

# variable importance plot
xgb.plot.importance(importance[1:30,])

# Tree graph
xgb.plot.tree(feature_names = feature.names, model = xg1)

pred <- predict(xg1, mat.train)

compare <- cbind(pred, y.train)
lcompare <- log(compare)

# EDA ---------------------------------------------------------------------

library(corrplot)

important.vars <- importance$Feature[1:50]
cor.vars <- as.matrix(mat.train[,colnames(mat.train) %in% important.vars])
cors <- cor(cor.vars, cor.vars)
corrplot(cors, method="circle", type="lower",  sig.level = 0.01, insig = "blank")


library(car)

scatterplot(SalePrice ~ YearBuilt, data=raw.all,  xlab="Year Built", ylab="Sale Price", grid=FALSE)
scatterplot(SalePrice ~ YrSold, data=raw.all,  xlab="Year Sold", ylab="Sale Price", grid=FALSE)
scatterplot(SalePrice ~ `1stFlrSF`, data=raw.all,  xlab="Square Footage Floor 1", ylab="Sale Price", grid=FALSE)

# Feature Testing --------------------------------------------------------------

# basic test of new features

CARET.TRAIN.PARMS <- list(method="xgbTree")   

CARET.TUNE.GRID <-  expand.grid(nrounds=800, 
                                max_depth=10, 
                                eta=0.03, 
                                gamma=0.1, 
                                colsample_bytree=0.4, 
                                min_child_weight=1)

MODEL.SPECIFIC.PARMS <- list(verbose=0) #NULL # Other model specific parameters

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="none",
                                 verboseIter=FALSE,
                                 classProbs=FALSE)

CARET.TRAIN.OTHER.PARMS <- list(trControl=CARET.TRAIN.CTRL,
                                tuneGrid=CARET.TUNE.GRID,
                                metric="RMSE")

xgb.folds.ord <- llply(cv.folds, trainOneFold, ord.mat, y.train, Id.train)
xgb.folds.ohe <- llply(cv.folds, trainOneFold, ohe.mat, y.train, Id.train)

cat("Average ORD CV rmse:",mean(do.call(c,lapply(xgb.folds.ord,function(x){x$score}))))
cat("Average OHE CV rmse:",mean(do.call(c,lapply(xgb.folds.ohe,function(x){x$score}))))
git
# TODO
# 1) fun exercise - see if avg of two models has any boost in performance
# 2) clean up encoding (esp in ordinal case)
# 3) independent feature analysis (check out Boruta)
# 4) improve upon 'accuracy' metric - seems like it is not very stable right now. Maybe use ensemble leaderboard performance?
# 5) start on model ensembling
