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

cors <- data

# graphs of data/predictor relationships
library(car)

scatterplot(SalePrice ~ YearBuilt, data=raw.all,  xlab="Year Built", ylab="Sale Price", grid=FALSE)
scatterplot(SalePrice ~ YrSold, data=raw.all,  xlab="Year Sold", ylab="Sale Price", grid=FALSE)
scatterplot(SalePrice ~ `1stFlrSF`, data=raw.all,  xlab="Square Footage Floor 1", ylab="Sale Price", grid=FALSE)



# Boruta Analysis ---------------------------------------------------------

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

# OHE encoding



# Looking at/handling related variables -----------------------------------

names <- colnames(dat.all)
garage.vars <- grep("garage", names, ignore.case = TRUE, value = TRUE)
garage.dat <- dat.all[,garage.vars]
# looks good!

fire.vars <- grep("fire", names, ignore.case = TRUE)
fire.dat <- dat.all[,fire.vars]
table(fire.dat, useNA = "always")
# looks good!

cond.vars <- grep("Condition", names, ignore.case = TRUE)
cond.dat <- dat.all[,cond.vars]
cond.dat <- cond.dat[,-3]
table(fire.dat, useNA = "always")

year.vars <- grep("year", names, ignore.case = TRUE)
table(dat.all[,year.vars[1]], useNA = "always")
table(dat.all[,year.vars[2]], useNA = "always")

table(dat.all[,c("Exterior1st", "Exterior2nd")])

qual.vars <- grep("qual", names, ignore.case = TRUE, value = TRUE)
table(dat.all[,c(qual.vars[c(1,6)])])

miss.garagequal <- dat.all[which(dat.all$GarageQual == "*MISSING*"),garage.vars]
miss.garagcars <- dat.all[which(dat.all$GarageCars < 0),garage.vars]
miss.garage <- dat.all[which(dat.all$GarageArea <= 0),garage.vars]

bsmt.vars <- grep("bsmt", names, ignore.case = TRUE, value = TRUE)
garage <- dat.all[,bsmt.vars]
table(dat.all[,c(bsmt.vars[c(1,4)])])

table(dat.all$GarageYrBlt)

for (i in 1:ncol(num.dat)) {
  na.frac <- sum(num.dat[,i] < 0)/nrow(num.dat)
  colname <- colnames(num.dat)[i]
  print(c(colname,na.frac))
}