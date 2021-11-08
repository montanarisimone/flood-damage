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
