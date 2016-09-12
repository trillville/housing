# Run Ensemble -------------------------------------------
# Using OHE and ORD encodings

set.seed(100)
runs <- 100
train.ind <- 1:length(y.train)
train.length <- length(y.train)

for (n in 1:runs) {
  cat("Run Number:", n)
  tmpS1 <- sample(train.ind,size=train.length,replace=T)
  tmpS2 <- setdiff(train.ind,tmpS1)
  
  ord.tmpX2 <- ord.train.s[tmpS2,]
  ohe.tmpX2 <- ohe.train.s[tmpS2,]
  tmpY2 <- y.train[tmpS2]
  
  ord.tmpX1 <- ord.train.s[tmpS1,]
  ohe.tmpX1 <- ohe.train.s[tmpS1,]
  tmpY1 <- y.train[tmpS1]
  
  # add models here
  ord.rf1 <- randomForest(x = as.matrix(ord.tmpX2), y = tmpY2, replace=F, ntree=100, do.trace=F, mtry = 90)
  ohe.rf1 <- randomForest(x = as.matrix(ohe.tmpX2), y = tmpY2, replace=F, ntree=100, do.trace=F, mtry = 90)
  
  ord.xg1 <- do.call(xgboost,
                     c(list(data = ord.tmpX2,
                            label = tmpY2),
                       XGB_PARS))
  ohe.xg1 <- do.call(xgboost,
                     c(list(data = ohe.tmpX2,
                            label = tmpY2),
                       XGB_PARS))
  
  # predict first model
  ord.tmpX2 <- predict(ord.rf1, as.matrix(ord.tmpX1), type="response")
  ord.tmpX3 <- predict(ord.rf1, as.matrix(ord.test.s), type="response")
  ohe.tmpX2 <- predict(ohe.rf1, as.matrix(ohe.tmpX1), type="response")
  ohe.tmpX3 <- predict(ohe.rf1, as.matrix(ohe.test.s), type="response")
  
  # aggregate model predictions
  ord.tmpX2 <- cbind(ord.tmpX2,predict(ord.xg1, as.matrix(ord.tmpX1)))
  ord.tmpX3 <- cbind(ord.tmpX3,predict(ord.xg1, as.matrix(ord.test.s)))
  ohe.tmpX2 <- cbind(ohe.tmpX2,predict(ohe.xg1, as.matrix(ohe.tmpX1)))
  ohe.tmpX3 <- cbind(ohe.tmpX3,predict(ohe.xg1, as.matrix(ohe.test.s)))
  
  # run xgboost on stacked predictions
  ord.bst <- do.call(xgboost,
                     c(list(data = cbind(ord.tmpX1,ord.tmpX2),
                            label = tmpY1),
                       XGB_PARS))
  
  ohe.bst <- do.call(xgboost,
                     c(list(data = cbind(ohe.tmpX1,ohe.tmpX2),
                            label = tmpY1),
                       XGB_PARS))
  
  # predict test set
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
colnames(pred.mat) <- c("Id", "SalePrice")
write.csv(pred.mat, file = "sub1.csv", row.names = FALSE)

pred.mat2 <- cbind(Id.test, ord.pred/runs)
colnames(pred.mat2) <- c("Id", "SalePrice")
write.csv(pred.mat2, file = "sub2.csv", row.names = FALSE)
