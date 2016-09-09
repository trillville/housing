# caret xgboost tuning parameters
# note - current thought is that these should be the TUNING parameters,
# a second set (to be stored in utils.R) will correspond to the final tuned
# parameters to be used in model building.

source("pre_process.R")

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

# tune xgboost hyperparameters using caret
xgb.cv.results <- train(x = ord.train.s,
                        y = y.train,
                        tuneGrid = CARET.TUNE.GRID,
                        trControl = CARET.TRAIN.CTRL,
                        metric = "RMSE",
                        method = "xgbTree")

# tunexgboost hyperparameters using bayesian optimization (TODO)



xgb.folds.ord <- llply(cv.folds, trainOneFold, ord.train.s, y.train, Id.train)
xgb.folds.ohe <- llply(cv.folds, trainOneFold, ohe.train.s, y.train, Id.train)

cat("Average ORD CV rmse:",mean(do.call(c,lapply(xgb.folds.ord,function(x){x$score}))))
cat("Average OHE CV rmse:",mean(do.call(c,lapply(xgb.folds.ohe,function(x){x$score}))))


# Basic Tuning - bagged XGBoost -------------------------------------------

# Train the model

set.seed(100)
runs <- 10
train.ind <- 1:length(y.train)
train.length <- length(y.train)

for (n in 1:runs) {
  cat("Run Number:", n)
  tmpS1 <- sample(train.ind,size=train.length,replace=T)
  tmpS2 <- setdiff(train.ind,tmpS1)
  
  ord.tmpX2 <- ord.train.s[tmpS2,]
  ohe.tmpX2 <- ohe.train.s[tmpS2,]
  tmpY2 <- y.train[tmpS2]
  
  ord.rf1 <- randomForest(x = as.matrix(ord.tmpX2), y = tmpY2, replace=F, ntree=100, do.trace=F, mtry = 90)
  ohe.rf1 <- randomForest(x = as.matrix(ohe.tmpX2), y = tmpY2, replace=F, ntree=100, do.trace=F, mtry = 90)
  
  ord.tmpX1 <- ord.train.s[tmpS1,]
  ohe.tmpX1 <- ohe.train.s[tmpS1,]
  tmpY1 <- y.train[tmpS1]
  
  ord.tmpX2 <- predict(ord.rf1, as.matrix(ord.tmpX1), type="response")
  ord.tmpX3 <- predict(ord.rf1, as.matrix(ord.test.s), type="response")
  ohe.tmpX2 <- predict(ohe.rf1, as.matrix(ohe.tmpX1), type="response")
  ohe.tmpX3 <- predict(ohe.rf1, as.matrix(ohe.test.s), type="response")
  
  ord.bst <- do.call(xgboost,
                 c(list(data = cbind(ord.tmpX1,ord.tmpX2),
                        label = tmpY1),
                   XGB_PARS))
  
  ohe.bst <- do.call(xgboost,
                 c(list(data = cbind(ohe.tmpX1,ohe.tmpX2),
                        label = tmpY1),
                   XGB_PARS))
  
  # Make prediction
  ord.pred0 = predict(ord.bst,cbind(ord.test.s, ord.tmpX3))
  ohe.pred0 = predict(ohe.bst,cbind(ohe.test.s, ohe.tmpX3))
  if (n == 1) {
    ord.pred <- ord.pred0
    ohe.pred <- ohe.pred0
  } else {
    ord.pred <- ord.pred + ord.pred0
    ohe.pred <- ohe.pred + ohe.pred0
  }
}
pred.agg <- ord.pred + ohe.pred
pred.avg = pred.agg/(2*runs)
pred.mat <- cbind(Id.test, pred.avg)
write.csv(pred.mat, file = "sub1.csv")

# TODO
# 1) fun exercise - see if avg of two models has any boost in performance
# 2) clean up encoding (esp in ordinal case)
# 3) independent feature analysis (check out Boruta) oR TSNE
# 4) improve upon 'accuracy' metric - seems like it is not very stable right now. Maybe use ensemble leaderboard performance?
# 5) start on model ensembling