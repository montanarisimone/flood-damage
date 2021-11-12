library("randomForest")
library("caret")
library("devtools")
library("reprtree")
library("inTrees")

data_rf <- subset(dati_cat, select = -c(id, outlier, danno_beni_immobili,
                                        costo_medio,
                                        danno_relativo_costo))

data_rf$tipologia_strutturale <- as.factor(data_rf$tipologia_strutturale)

# train test split
set.seed(123)
index <- sample(1:nrow(data_rf), round(0.85*nrow(data_rf)), replace = F)
data_train_rf <- data_rf[index, ]
data_test_rf <- data_rf[-index, ]

### DEFAULT MODEL
set.seed(123)
rf <- randomForest(
  danno_relativo_valore ~.,
  data = data_train_rf,
  importance = T
)

# predict on train and test data
pred_train_rf <- predict(rf, newdata = data_train_rf)
pred_test_rf <- predict(rf, newdata = data_test_rf)

# performance train
train_performance <- postResample(pred = pred_train_rf, obs = data_train_rf[, ncol(data_train_rf)])
round(train_performance, 2)

# performance test
test_performance <- postResample(pred = pred_test_rf, obs = data_test_rf[, ncol(data_test_rf)])
round(test_performance, 2)

# TUNED MODEL (be careful, long calculation time)
set.seed(123)
# tune control
tuneGrid <- expand.grid(.mtry = c(2,3))
ctrl <- trainControl(method = "cv",
                     number = 5,
                     search = 'random')

# other parameters to tune
ntrees <- seq(200, 600, by=100)
nodesize <- seq(4, 20, by = 4)
params <- expand.grid(ntrees = ntrees,
                      nodesize = nodesize)

store_maxnode <- vector("list", nrow(params))
pb <- txtProgressBar(min = 0, max = nrow(params), style = 3) # progress bar

for(i in 1:nrow(params)){
  ntree <- params[i,1]
  nodesize <- params[i,2]
  set.seed(123)
  rf_model <- train(danno_relativo_valore ~.,
                    data = data_train_rf,
                    method = "rf",
                    metric = "RMSE",
                    tuneGrid = tuneGrid, 
                    trControl = ctrl,
                    importance = T, 
                    nodesize = nodesize,
                    ntree = ntree)
  store_maxnode[[i]] <- rf_model
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
}

# rename resulting list with corresponding parameters
names(store_maxnode) <- paste("ntrees:", params$ntrees,
                              "nodesize:", params$nodesize)

# combine results
results_mtry <- resamples(store_maxnode)

# best average performance for each model
# see the min rmse over 10 folds is achieved with: search min rmse and get params
lapply(store_maxnode, function(x) x$results[x$results$RMSE == min(x$results$RMSE),])      

# use the params to set final tuned rf model
set.seed(123)
rf_tuned <- randomForest(
  danno_relativo_valore ~.,
  data = data_train_rf,
  ntree = 300,
  nodesize = 5,
  mtry = 2,
  importance = T
)

# predict on train and test data
pred_test_tuned_rf <- predict(rf_tuned, newdata = data_test_rf)
pred_train_tuned_rf <- predict(rf_tuned, newdata = data_train_rf)

# performance train
train_performance_tuned <- postResample(pred = pred_train_tuned_rf, obs = data_train_rf[, ncol(data_train_rf)])
print(round(train_performance_tuned, 2))

# performance test
test_performance_tuned <- postResample(pred = pred_test_tuned_rf, obs = data_test_rf[, ncol(data_test_rf)])
print(round(test_performance_tuned, 2))

## VAR IMPORTANCE
imp_rf <- as.data.frame(varImp(rf_tuned))
imp_rf <- data.frame(names   = rownames(imp_rf),
                  overall = imp_rf$Overall)
imp_rf <- imp_rf[order(imp_rf$overall, decreasing = T),] 
for(i in 1:nrow(imp_rf)){
  if(grepl("_", imp_rf$names[i], fixed=T)){ # cerca _ nella stringa
    imp_rf$name[i] <- strsplit(imp_rf$names[i], "_")[[1]][[1]] # prende la parte prima di _
  }else{imp_rf$name[i] = imp_rf$names[i]}
}
# plot var importance
barplot(height=imp_rf$overall, names=imp_rf$name, 
        col="#69b3a2",
        las=1)
title("RF Variable Importance", adj = 0)
imp_rf1 <- imp_rf[, 2:3]
colnames(imp_rf1) <- c("RF", "var")

# scaled var importance
scaling <- function(x) {
  out = (x - min(x))/(max(x) -  min(x))
}
barplot(height = scaling(imp_rf$overall), names = imp_rf$name, 
        col="#69b3a2", ylab = "importanza scalata",
        las=1)
title("RF Variable Importance", adj = 0)

## RF MODEL RULES
ruleExec0_rf <- extractRules(RF2List(rf_tuned), data_train_rf[,-ncol(data_train_rf)], ntree = 300)
ruleExec_rf <- unique(ruleExec0_rf)
ruleMetric_rf <- getRuleMetric(ruleExec_rf, data_train_rf[,-ncol(data_train_rf)], data_train_rf[,ncol(data_train_rf)])
rules_rf <- presentRules(ruleMetric_rf, colnames(data_train_rf))
rules_rf <- rules_rf[order(rules_rf[,"freq"], decreasing = T), ]
