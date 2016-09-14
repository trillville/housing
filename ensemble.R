# Run Ensemble -------------------------------------------
# Using OHE and ORD encodings

set.seed(100)
runs <- 50
train.ind <- 1:length(y.train)
train.length <- length(y.train)

for (n in 1:runs) {
  cat("Run Number:", n)
  tmpS1 <- sample(train.ind,size=train.length,replace=T)
  tmpS2 <- setdiff(train.ind,tmpS1)
  
  ord.tmpX2 <- ord.train.m[tmpS2,]
  ord.tmpX2.b <- ord.train.b.m[tmpS2,]
  tmpY2 <- y.train[tmpS2]
  
  ord.tmpX1 <- ord.train.m[tmpS1,]
  ord.tmpX1.b <- ord.train.b.m[tmpS1,]
  tmpY1 <- y.train[tmpS1]
  
  # add models here
  ord.rf1 <- randomForest(x = as.matrix(ord.tmpX2.b), y = tmpY2, replace=F, ntree=800, do.trace=F, mtry = 50)
  
  ord.xg1 <- do.call(xgboost,
                     c(list(data = ord.tmpX2,
                            label = tmpY2),
                       XGB_PARS))
  
  # predict first model
  ord.tmpX2 <- predict(ord.rf1, as.matrix(ord.tmpX1.b), type="response")
  ord.tmpX3 <- predict(ord.rf1, as.matrix(ord.test.b.m), type="response")
  
  # aggregate model predictions
  ord.tmpX2 <- cbind(ord.tmpX2,predict(ord.xg1, ord.tmpX1))
  ord.tmpX3 <- cbind(ord.tmpX3,predict(ord.xg1, ord.test.m))
  
  # run xgboost on stacked predictions
  ord.bst <- do.call(xgboost,
                     c(list(data = cbind(ord.tmpX1,ord.tmpX2),
                            label = tmpY1),
                       XGB_PARS))
  
  # predict test set
  ord.pred0 = predict(ord.bst,cbind(ord.test.m, ord.tmpX3))
  if (n == 1) {
    ord.pred <- ord.pred0
  } else {
    ord.pred <- ord.pred + ord.pred0
  }
}
pred.avg <- ord.pred/runs
pred.avg.exp <- exp(pred.avg)
pred.mat <- cbind(Id.test, pred.avg.exp)
colnames(pred.mat) <- c("Id", "SalePrice")
write.csv(pred.mat, file = "sub1.csv", row.names = FALSE)

pred.mat2 <- cbind(Id.test, ord.pred/runs)
colnames(pred.mat2) <- c("Id", "SalePrice")
write.csv(pred.mat2, file = "sub2.csv", row.names = FALSE)