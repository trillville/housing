# Tuning --------------------------------------------------------------

# caret xgboost tuning parameters
# note - current thought is that these should be the TUNING parameters,
# a second set (to be stored in utils.R) will correspond to the final tuned
# parameters to be used in model building.

CARET.TRAIN.PARMS <- list(method="xgbTree")   

CARET.TUNE.GRID <-  expand.grid(nrounds=seq(600,1000, by = 200), 
                                max_depth=c(5,10,15), 
                                eta=c(0.01,0.03,0.05,0.1), 
                                gamma=0.1, 
                                colsample_bytree=c(0.5,1), 
                                min_child_weight=c(1))

MODEL.SPECIFIC.PARMS <- list(verbose=0) 

CARET.TRAIN.CTRL <- trainControl(method="repeatedCV",
                                 number = 5,
                                 repeats = 2,
                                 verboseIter=FALSE,
                                 classProbs=FALSE)

CARET.TRAIN.OTHER.PARMS <- list(trControl=CARET.TRAIN.CTRL,
                                tuneGrid=CARET.TUNE.GRID,
                                metric="RMSE")

# tune xgboost hyperparameters
xgb.cv.results <- train(x = ord.train.s,
                        y = y.train,
                        tuneGrid = CARET.TUNE.GRID,
                        trControl = CARET.TRAIN.CTRL,
                        metric = "RMSE",
                        method = "xgbTree")

# basic model using meta bagging
# should inform feature selection and provide a performance baseline

xgb.folds.ord <- llply(cv.folds, trainOneFold, ord.train.s, y.train, Id.train)
xgb.folds.ohe <- llply(cv.folds, trainOneFold, ohe.train.s, y.train, Id.train)

cat("Average ORD CV rmse:",mean(do.call(c,lapply(xgb.folds.ord,function(x){x$score}))))
cat("Average OHE CV rmse:",mean(do.call(c,lapply(xgb.folds.ohe,function(x){x$score}))))


# Basic Tuning - bagged XGBoost -------------------------------------------

# Train the model
set.seed(100)

ord.xg1 <- do.call(xgboost,
                   c(list(data = ord.train.s,
                          label = y.train),
                     XGB_PARS))

# Make prediction
pred <- predict(ord.xg1, ord.test.s)

runs <- 200
train.ind <- 1:length(y.train)
train.length <- length(y.train)
for (n in 1:runs) {
  print(n)
  tmpS1 <- sample(train.ind,size=train.length,replace=T)
  tmpS2 <- setdiff(train.ind,tmpS1)
  
  tmpX2 <- ord.train.s[tmpS2,]
  tmpY2 <- y.train[tmpS2]
  
  cst <- randomForest(x = as.matrix(tmpX2), y = tmpY2, replace=F, ntree=100, do.trace=F, mtry = 90)
  
  tmpX1 <- ord.train.s[tmpS1,]
  tmpY1 <- y.train[tmpS1]
  
  tmpX2 <- predict(cst, as.matrix(tmpX1), type="response")
  tmpX3 <- predict(cst, as.matrix(ord.test.s), type="response")
  
  bst <- do.call(xgboost,
                 c(list(data = cbind(tmpX1,tmpX2),
                        label = tmpY1),
                   XGB_PARS))
  
  # Make prediction
  pred0 = predict(bst,cbind(ord.test.s,tmpX3))
  pred = pred + pred0
}

ohe.xg1 <- do.call(xgboost,
                   c(list(data = ord.train.s,
                          label = y.train),
                     XGB_PARS))

pred0 <- predict(ohe.xg1, ohe.test.s)
pred <- pred + pred0
for (n in 1:runs) {
  print(n)
  tmpS1 <- sample(train.ind,size=train.length,replace=T)
  tmpS2 <- setdiff(train.ind,tmpS1)
  
  tmpX2 <- ohe.train.s[tmpS2,]
  tmpY2 <- y.train[tmpS2]
  
  cst <- randomForest(x = as.matrix(tmpX2), y = tmpY2, replace=F, ntree=100, do.trace=F, mtry = 90)
  
  tmpX1 <- ohe.train.s[tmpS1,]
  tmpY1 <- y.train[tmpS1]
  
  tmpX2 <- predict(cst, as.matrix(tmpX1), type="response")
  tmpX3 <- predict(cst, as.matrix(ohe.test.s), type="response")
  
  bst <- do.call(xgboost,
                 c(list(data = cbind(tmpX1,tmpX2),
                        label = tmpY1),
                   XGB_PARS))
  
  # Make prediction
  pred0 = predict(bst,cbind(ohe.test.s,tmpX3))
  pred = pred + pred0
}

pred.avg = pred/(2*runs+2)
pred.mat <- cbind(Id.test, pred.avg)
write.csv(pred.mat, file = "sub1.csv")

# TODO
# 1) fun exercise - see if avg of two models has any boost in performance
# 2) clean up encoding (esp in ordinal case)
# 3) independent feature analysis (check out Boruta) oR TSNE
# 4) improve upon 'accuracy' metric - seems like it is not very stable right now. Maybe use ensemble leaderboard performance?
# 5) start on model ensembling