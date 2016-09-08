# Feature Importance Charts ------------------------------------------------------

# compare feature importance between OHE and ORD schemes
xgb.full.ord <- do.call(xgboost,
                        c(list(data = ord.train.s,
                               label = y.train),
                          XGB_PARS))

xgb.full.ohe <- do.call(xgboost,
                        c(list(data = ohe.train.s,
                               label = y.train),
                          XGB_PARS))

feature.names.ord <- dimnames(ord.train.s)[[2]]
feature.names.ohe <- dimnames(ohe.train.s)[[2]]

importance.ord <- xgb.importance(feature.names.ord, model = xgb.full.ord)
importance.ohe <- xgb.importance(feature.names.ohe, model = xgb.full.ohe)

# EDA Charts ---------------------------------------------------------------------

# Correlations
library(corrplot)

# TODO: redo using new variable importance data
# important.vars <- importance$Feature[1:50]
# cor.vars <- as.matrix(mat.train[,colnames(mat.train) %in% important.vars])
# cors <- cor(cor.vars, cor.vars)
# corrplot(cors, method="circle", type="lower",  sig.level = 0.01, insig = "blank")

# graphs of data/predictor relationships
library(car)

scatterplot(SalePrice ~ YearBuilt, data=raw.all,  xlab="Year Built", ylab="Sale Price", grid=FALSE)
scatterplot(SalePrice ~ YrSold, data=raw.all,  xlab="Year Sold", ylab="Sale Price", grid=FALSE)
scatterplot(SalePrice ~ `1stFlrSF`, data=raw.all,  xlab="Square Footage Floor 1", ylab="Sale Price", grid=FALSE)

