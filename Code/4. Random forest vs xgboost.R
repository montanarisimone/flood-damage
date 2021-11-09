# First run code 1, 2, 3

## VARIABLE IMPORTANCE


## PDP PLOT
# pdp for feature 'edfc' (change this input to obtain pdp for other vars)
# rf
library("iml")
mod_rf <- Predictor$new(model = rf_tuned, data = data_train_rf)
pdp_rf <- FeatureEffect$new(mod_rf, feature = 'edfc', method = 'pdp')
a <- pdp_rf$results

# xgb
library("pdp")
pdp_xgb <- partial(object = model1, pred.var = 'edfc',
                   train = train.X, type = 'regression', plot.engine = 'ggplot2')
                   
par(mfrow=c(2,1), oma = c(2,0,1,1) + 0.1)
par(mar = c(0,5,2,1) + 0.1)
# pdp
plot(a$edfc, a$.value, type="l", main = "PDP # edifici", xlab = "",
     ylab = "danno relativo predetto", ylim = c(0.09, 0.22), lwd = 2, xaxt = "n",
     bty = "n", xlim = c(0, 18))
axis(side = 1, at = seq(0.0, 18.0, by = 3), labels = c(rep("", 7)), lwd.ticks = 1)
lines(pdp_xgb$edfc, pdp_xgb$yhat, col = "red", lwd = 2)
legend(4, 0.20, legend = c("RF", "XGB"), col = c("black", "red"), lty = 1, 
       cex = .8, box.lty = 0, lwd = 2)

# density distribution 
par(mar = c(1,5,2,1), bty = "n")
hist(data_train_rf$edfc, xlim = c(0, 18), xaxt = "n",
     main = "Distribuzione", ylab = "densità", xlab = "mt", bty = "n",
     col = "tan1", freq = F, breaks = 15)
axis(side = 1, at = c(0, 18), labels = c("",""), lwd.ticks = 0)
axis(side = 1, at = seq(0.0, 18.0, by = 3), lwd = 0, lwd.ticks = 1)
