# Run Ensemble -------------------------------------------
# only using ORD

set.seed(100)
runs <- 200
train.ind <- 1:length(y.train)
train.length <- length(y.train)
data.train <- as.matrix(data.frame(ord.train.m)[, PREDICTOR_ATTR])
data.train.pp <- as.matrix(data.frame(ord.train.pp)[, PREDICTOR_ATTR])
data.test <- as.matrix(data.frame(ord.test.m)[, PREDICTOR_ATTR])
data.test.pp <- as.matrix(data.frame(ord.test.m)[, PREDICTOR_ATTR])

for (n in 1:runs) {
  cat("Run Number:", n)
  tmpS1 <- sample(train.ind,size=train.length,replace=T)
  tmpS2 <- setdiff(train.ind,tmpS1)
  
  ord.tmpX2 <- data.train[tmpS2,]
  ord.tmpX2.pp <- data.train.pp[tmpS2,]
  tmpY2 <- y.train[tmpS2]
  
  ord.tmpX1 <- data.train[tmpS1,]
  ord.tmpX1.pp <- data.train.pp[tmpS1,]
  tmpY1 <- y.train[tmpS1]
  
  # add models here
  ord.rf1 <- do.call(randomForest,
                     c(list(x = ord.tmpX2,
                            y = tmpY2),
                       RF_PARS))
  
  ord.xg1 <- do.call(xgboost,
                     c(list(data = ord.tmpX2,
                            label = tmpY2),
                       XGB_PARS))
  
  ord.knn3 <- train(x = ord.tmpX2.pp, y = tmpY2,
                           method = "knn",
                           metric = "RMSE",
                           tuneGrid = KNN3_PARS,
                           trControl = trainControl("none"))
  
  ord.knn7 <- train(x = ord.tmpX2.pp, y = tmpY2,
                    method = "knn",
                    metric = "RMSE",
                    tuneGrid = KNN7_PARS,
                    trControl = trainControl("none"))
  
  ord.knn10 <- train(x = ord.tmpX2.pp, y = tmpY2,
                    method = "knn",
                    metric = "RMSE",
                    tuneGrid = KNN10_PARS,
                    trControl = trainControl("none"))
  
  # predict first model
  ord.tmpX2 <- predict(ord.rf1, as.matrix(ord.tmpX1), type="response")
  ord.tmpX3 <- predict(ord.rf1, as.matrix(data.test), type="response")

  # aggregate model predictions
  ord.tmpX2 <- cbind(ord.tmpX2,predict(ord.xg1, as.matrix(ord.tmpX1)))
  ord.tmpX3 <- cbind(ord.tmpX3,predict(ord.xg1, as.matrix(data.test)))
  
  ord.tmpX2 <- cbind(ord.tmpX2,predict(ord.knn3, as.matrix(ord.tmpX1.pp)))
  ord.tmpX3 <- cbind(ord.tmpX3,predict(ord.knn3, as.matrix(data.test.pp)))
  
  ord.tmpX2 <- cbind(ord.tmpX2,predict(ord.knn7, as.matrix(ord.tmpX1.pp)))
  ord.tmpX3 <- cbind(ord.tmpX3,predict(ord.knn7, as.matrix(data.test.pp)))
  
  ord.tmpX2 <- cbind(ord.tmpX2,predict(ord.knn10, as.matrix(ord.tmpX1.pp)))
  ord.tmpX3 <- cbind(ord.tmpX3,predict(ord.knn10, as.matrix(data.test.pp)))
  
  # run xgboost on stacked predictions
  
  XGB_PARS2 <- list(objective = "reg:linear",
                   eval_metric = "rmse",
                   nrounds=600,
                   max_depth=5,
                   eta=0.1,
                   gamma=0.1,
                   colsample_bytree=1,
                   min_child_weight=10)
  
  ord.bst <- do.call(xgboost,
                     c(list(data = ord.tmpX2,
                            label = tmpY1),
                       XGB_PARS2))
  
  # predict test set
  ord.pred0 = predict(ord.bst,cbind(ord.tmpX3))
  if (n == 1) {
    ord.pred <- ord.pred0
  } else {
    ord.pred <- ord.pred + ord.pred0
  }
}

pred.avg = ord.pred/(runs)
pred.mat <- cbind(Id.test, pred.avg)
colnames(pred.mat) <- c("Id", "SalePrice")
write.csv(pred.mat, file = "sub1.csv", row.names = FALSE)

