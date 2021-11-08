library("xgboost")
library("xgboostExplainer")
library("caret")
library("inTrees")

data_xgb <- subset(dati_bin, select = -c(id, outlier, danno_beni_immobili,
                                     costo_medio,
                                     danno_relativo_costo))
                                     
# train test split
set.seed(123)
index <- sample(1:nrow(data_xgb), round(0.85*nrow(data_xgb)), replace = F)
data_train_xgb <- data_xgb[index, ]
data_test_xgb <- data_xgb[-index, ]

train.y <- data_train_xgb[, ncol(data_train_xgb)]
test.y <- data_test_xgb[, ncol(data_test_xgb)]
train.X <- as.matrix(data_train_xgb[, -ncol(data_train_xgb)])
test.X <- as.matrix(data_test_xgb[, -ncol(data_test_xgb)])

## DEFAULT MODEL
parameters <- list(eta = 0.3, 
                   max_depth = 6, 
                   subsample = 1, 
                   colsample_bytree = 1,
                   min_child_weight = 1,
                   gamma = 0.03, 
                   eval_metric = "rmse",
                   objective = "reg:squarederror",
                   booster = "gbtree")
                   
set.seed(123)
model <- xgboost(data = train.X,
                 label = train.y,
                 nrounds = 300, 
                 params = parameters,
                 print_every_n = 50)
                 
# predict on train and test data
pred_test <- predict(model, newdata = test.X)
pred_train <- predict(model, newdata = train.X)

# performance train
round(postResample(pred = pred_train, obs = train.y),2)
# performance test
round(postResample(pred = pred_test, obs = test.y),2)

## TUNED MODEL
# cv inputs
y <- data_xgb[, ncol(data_xgb)]
X <- as.matrix(data_xgb[, -ncol(data_xgb)])

# cv params
set.seed(123)
tune_control1 <- trainControl(method = "cv",
                              number = 5,
                              search = "random")

# params to tune
tune_grid1 <- expand.grid(eta = seq(0.02, 0.06, by = 0.02),
                          nrounds = seq(200, 500, by = 100),
                          max_depth = seq(2, 8, by = 2),
                          subsample = c(0.7, 0.8, 0.85, 1),
                          colsample_bytree = 1,
                          min_child_weight =1,
                          gamma = c(0, 0.03, 0.05, 0.1))

# tuning
start <- Sys.time()
set.seed(123)
xgb_tune1 <- train(x = X,
                   y = y,
                   method = "xgbTree",
                   trControl = tune_control1,
                   tuneGrid = tune_grid1)
end <- Sys.time()

# list of best params
parameters1 <- list(eta = xgb_tune1$bestTune$eta,
                    max_depth = xgb_tune1$bestTune$max_depth,
                    subsample = xgb_tune1$bestTune$subsample,
                    colsample_bytree = xgb_tune1$bestTune$colsample_bytree,
                    min_child_weight = xgb_tune1$bestTune$min_child_weight,
                    gamma = xgb_tune1$bestTune$gamma,
                    eval_metric = "rmse",
                    objective = "reg:squarederror",
                    booster = "gbtree")

# best params
parameters1 <- list(eta = 0.04,
                    max_depth = 4,
                    subsample = 0.7,
                    colsample_bytree = 1,
                    min_child_weight = 1,
                    gamma = 0,
                    eval_metric = "rmse",
                    objective = "reg:squarederror",
                    booster = "gbtree")

# model tuned
set.seed(123)
model1 <- xgboost(data = train.X,
                  label = train.y,
                  nrounds = 200,
                  params = parameters1,
                  print_every_n = 50)

# predict on train and test data
pred_test1 <- predict(model1, newdata = test.X)
pred_train1 <- predict(model1, newdata = train.X)

# performance train
round(postResample(pred = pred_train1, obs = train.y), 2)
# performance test
round(postResample(pred = pred_test1, obs = test.y), 2)

### VARIABLE IMPORTANCE
imp4 <- as.data.frame(xgb.importance(model = model1))
imp4 <- data.frame(names   = imp4$Feature,
                   gain = imp4$Gain)
imp4 <- imp4[order(imp4$gain, decreasing = T),] 
for(i in 1:nrow(imp4)){
  if(grepl("_", imp4$names[i], fixed=T)){ 
    imp4$name[i] <- strsplit(imp4$names[i], "_")[[1]][[1]] 
  }else{imp4$name[i] = imp4$names[i]}
}

# plot vars importance
barplot(height=imp4$gain, names=imp4$name, 
        col="orange",
        las=1, horiz = F, ylim = c(0,0.27))
title("XGB Variable Importance", adj = 0)

# scaling vars importance
scaling<- function(x){
  out=(x-min(x))/(max(x)-min(x))
}
barplot(height = scaling(imp4$gain), names = imp4$name, 
        col="orange", ylab = "importanza scalata",
        las=1, horiz = F)
title("XGB Variable Importance", adj = 0)

### RULES
tree_list <- XGB2List(model1, train.X)
ruleExec0 <- extractRules(treeList = tree_list, X = train.X, random = F)
ruleExec <- unique(ruleExec0)
ruleMetric <- getRuleMetric(ruleExec, data_train_xgb[,-ncol(data_train_xgb)], data_train_xgb[,ncol(data_train_xgb)])
rules <- presentRules(ruleMetric, colnames(data_train_xgb))
rules <- rules[order(rules[,"freq"], decreasing = T), ]
