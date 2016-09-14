t1 <- sample(length(y.train), length(y.train)/2)


ord.rf1 <- randomForest(x = ord.train.b.m[t1, ], y = y.train[t1], mtry = 29, ntree = 800)

ord.xg1 <- do.call(xgboost,
                   c(list(data = ord.train.m[t1, ],
                          label = y.train[t1]),
                     XGB_PARS))

ord.las1 <- train(x = as.matrix(data.frame(ord.train.m)[, PREDICTOR_ATTR]), y = y.train,
                           method = "lasso",
                           metric = "RMSE",
                           tuneGrid = expand.grid(fraction = 0.7222222),
                           trControl = trainControl("none"))


ord.knn5 <- train(x = ord.train.b.m[t1, ], y = y.train[t1],
                  method = "knn",
                  metric = "RMSE",
                  tuneGrid = KNN5_PARS,
                  trControl = trainControl("none"))

ord.knn10 <- train(x = ord.train.b.m[t1, ], y = y.train[t1],
                   method = "knn",
                   metric = "RMSE",
                   tuneGrid = KNN10_PARS,
                   trControl = trainControl("none"))

# predict first model
ord.tmpX2 <- predict(ord.rf1, as.matrix(ord.train.b.m[-t1, ]), type="response")

# aggregate model predictions
ord.tmpX2 <- cbind(ord.tmpX2,predict(ord.xg1, ord.train.m[-t1, ]))

ord.tmpX2 <- cbind(ord.tmpX2,predict(ord.knn5, ord.train.b.m[-t1, ]))

ord.tmpX2 <- cbind(ord.tmpX2,predict(ord.knn10, ord.train.b.m[-t1, ]))

ord.tmpX2 <- cbind(ord.tmpX2,predict(ord.las1, ord.train.b.m[-t1, ]))
XGB_PARS <- list(objective = "reg:linear",
                 eval_metric = "rmse",
                 nrounds=1300,
                 max_depth=5,
                 eta=0.01,
                 gamma=0.0,
                 subsample = 0.9,
                 colsample_bytree=0.5,
                 min_child_weight=1)


# run xgboost on stacked predictions
dat <- cbind(ord.train.m[-t1, ],ord.tmpX2)
grid2 <-  expand.grid(nrounds=seq(800,1400, by = 200), 
                                    eta=c(0.01),
                                    max_depth = c(3, 5, 10),
                                    colsample_bytree = c(0.5),
                                    min_child_weight = c(1, 5),
                                    gamma = c(0,0.1))

bst1 <- train(x = dat, y = y.train[-t1],
                         method = "xgbTree",bst1
                         metric = "RMSE",
                         tuneGrid = grid2,
                         trControl = CARET_TRAIN_CTRL)

ord.bst <- do.call(xgboost,
                   c(list(data = cbind(ord.train.m,ord.tmpX2),
                          label = tmpY1),
                     XGB_PARS))
