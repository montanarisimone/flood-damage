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
