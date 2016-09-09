# Libraries ---------------------------------------------------------------

#library(FeatureHashing)
library(Matrix)
library(xgboost)
library(randomForest)
library(rBayesianOptimization)
library(caret)
library(Rtsne)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
#library(dummies)
library(Metrics)
library(readr)

library(Boruta)
#library(kernlab)
#library(data.table)

# Functions ---------------------------------------------------------------

trainOneFold <- function(fold, predictors, y, ID) {
  # fit model based on all training data except for current fold
  set.seed(825)
  fit <- do.call(xgboost,
                 c(list(data = predictors[-fold, ],
                        label = y[-fold]),
                   XGB_PARS))
  
  pred <- predict(fit,newdata = predictors[fold, ])
  
  score <- logRMSE(y[fold], pred)
  
  ans <- list(fitted.mdl = fit,
              score=score,
              predictions = data.frame(ID = ID[fold], yhat = pred, y = y[fold]))
  
  return(ans)
}

xgbCvBayes <- function(max.depth, subsample, colsample_bytree, eta, min_child_weight, nrounds) {
  cv <- xgb.cv(params = list(max.depth = max.depth,
                             subsample = subsample, 
                             colsample_bytree = colsample_bytree,
                             eta = eta,
                             min_child_weight = min_child_weight,
                             metrics = "rmse", 
                             eval_metric = "rmse", 
                             objective = "reg:linear"),
               nrounds = nrounds,
               data = ord.train.xgb, 
               folds = cv.folds.bayes, prediction = TRUE, showsd = TRUE,
               early.stop.round = 3, maximize = TRUE, verbose = TRUE)
  list(Score = -cv$dt[, max(test.rmse.mean)], Pred = cv$pred)
}

# consistent score with kaggle leaderboard
logRMSE <- function(actual, predicted) {
  return (sqrt(mean((log(actual) - log(predicted))^2)))
}

runBoruta <- function(){
  
  # ordinal encoding
  response <- y.train
  sample <- select(dat.ord, everything(), -Id, -SalePrice)
  set.seed(69)
  bor.results <- Boruta(sample[train, ], 
                        response,
                        maxRuns = 200,
                        doTrace = 2)
  
  results <- as.data.frame(bor.results$finalDecision)
  names <- rownames(test)
  final <- data.frame(cbind(names, results[[1]]))
  
  boruta.ord.tentative <- as.character(final$names[which(final$V2 == 1)])
  boruta.ord.confirmed <- as.character(final$names[which(final$V2 == 2)])
  boruta.ord.rejected <- as.character(final$names[which(final$V2 == 3)])
  ans <- list(bor.results,
              boruta.ord,tentative,
              boruta.confirmed,
              boruta.rejected)
  return(ans)
}