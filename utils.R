# Functions ---------------------------------------------------------------

xgbCVPerformance <- function(model.parameters, predictors, y) {
  set.seed(100)
  results <- c(length = length(cv.folds.caret))
  for (i in 1:length(cv.folds.caret)) {
    fold <- cv.folds.caret[[i]]
    fit <- do.call(xgboost, 
                   c(list(data = predictors[fold, ],
                          label = y[fold]),
                     model.parameters))
    pred <- predict(fit, newdata = predictors[-fold, ])
    score <- rmse(y[-fold], pred)
    results[i] <- score
  }
  return(results)
}

rfCVPerformance <- function(model.parameters, predictors, y) {
  set.seed(100)
  results <- c(length = length(cv.folds.caret))
  for (i in 1:length(cv.folds.caret)) {
    fold <- cv.folds.caret[[i]]
    fit <- do.call(randomForest, 
                   c(list(x = predictors[fold, ],
                          y = y[fold]),
                     model.parameters))
    pred <- predict(fit, newdata = predictors[-fold, ])
    score <- rmse(y[-fold], pred)
    results[i] <- score
  }
  return(results)
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
  # using ordinal encoding
  response <- y.train
  set.seed(69)
  bor.results <- Boruta(ord.train.m,
                        response,
                        maxRuns = 400,
                        doTrace = 2)
  
  results <- as.data.frame(bor.results$finalDecision)
  names <- rownames(results)
  final <- data.frame(cbind(names, results[[1]]))
  final[[1]] <- as.character(final[[1]])
  
  boruta.tentative <- final$names[which(final$V2 == 1)]
  boruta.confirmed <- final$names[which(final$V2 == 2)]
  boruta.rejected <- final$names[which(final$V2 == 3)]
  ans <- list(bor.results,
              boruta.tentative,
              boruta.confirmed,
              boruta.rejected)
  return(ans)
}