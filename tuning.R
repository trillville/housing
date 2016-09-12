
# Global Tuning -----------------------------------------------------------

set.seed(69)
cv.folds.caret <- createMultiFolds(y.train, k = 5, times = 5)

# XGBoost Tuning ----------------------------------------------------------

# tunexgboost hyperparameters using bayesian optimization
tune.bayes.xgb <- FALSE
if (tune.bays.xgb == TRUE) {
  cv.folds.bayes <- KFold(getinfo(ord.train.xgb, 'label'), nfolds = 5, stratified = FALSE, seed = 0)
  
  xgb.bayes.opt <- BayesianOptimization(xgbCvBayes,
                              bounds = list(max.depth = c(1L, 15L),
                                            subsample = c(0.3, 0.9),
                                            colsample_bytree = c(0.3, 0.9),
                                            eta = c(0.01, 0.1),
                                            min_child_weight = c(1L,15L),
                                            nrounds = c(500L,1200L)),
                              init_points = 15, n_iter = 30, acq = "ei", 
                              kappa = 3, eps = 0.5, verbose = TRUE)
}


# grid search to optimize nrounds and eta
tune.caret.xgb <- FALSE
if (tune.caret.xgb == TRUE) {
  xgb.caret.train <- train(x = ord.train.m, y = y.train,
                       method = "xgbTree",
                       metric = "RMSE",
                       tuneGrid = XGB_CARET_TUNE_GRID,
                       trControl = CARET_TRAIN_CTRL)
}

# check performance
check.base.xgb <- FALSE
if (check.base.xgb == TRUE) {
  xgb.ord.cv <- xgbCVPerformance(model.parameters = XGB_PARS, predictors = ord.train.m, y = y.train)
  xgb.ohe.cv <- xgbCVPerformance(model.parameters = XGB_PARS, predictors = ohe.train.m, y = y.train)
}


# RandomForest Tuning -----------------------------------------------------

tune.caret.rf <- TRUE
if (tune.caret.rf == TRUE) {
  rf.caret.train <- train(x = ord.train.m, y = y.train,
                       method = "rf",
                       metric = "RMSE",
                       tuneGrid = RF_CARET_TUNE_GRID,
                       trControl = CARET_TRAIN_CTRL)
}

# check performance
check.rf <- FALSE
if (check.rf == TRUE) {
  rf.ord.cv <- rfCVPerformance(model.parameters = RF_PARS, predictors = ord.train.m, y = y.train)
}


# KNN Tuning --------------------------------------------------------------

# Feature selection using simulated annealing
knn.sa <- FALSE
if (knn.sa == TRUE) {
  knnSA <- caretSA
  ctrl <- safsControl(functions = knnSA,
                      method = "repeatedcv",
                      repeats = 5,
                      ## Here are the exact folds to used:
                      index = cv.folds.caret,
                      ## What should we optimize? 
                      improve = 50,
                      allowParallel = TRUE)
  knn.sa <- safs(x = ord.train.pp,
                 y = y.train,
                 iters = 500,
                 safsControl = ctrl,
                 method = "knn",
                 tuneLength = 20,
                 trControl = CARET_TRAIN_CTRL)
}

tune.caret.knn <- TRUE
if (tune.caret.kn == TRUE) {
  knn.caret.train <- train(x = as.matrix(data.frame(ord.train.m)[, PREDICTOR_ATTR]), y = y.train,
                          method = "knn",
                          metric = "RMSE",
                          tuneGrid = KNN_CARET_TUNE_GRID,
                          trControl = CARET_TRAIN_CTRL)
}

# check performance
check.knn <- FALSE
if (check.knn == TRUE) {
  knn.ord.cv <- rfCVPerformance(model.parameters = RF_PARS, predictors = as.matrix(data.frame(ord.train.m)[, PREDICTOR_ATTR]), y = y.train)
}



# Nnet Tuning -------------------------------------------------------------
ord.train.nn <- cbind(ord.train.nn, y.train)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
