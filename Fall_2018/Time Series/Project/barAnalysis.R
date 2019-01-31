# read in Boston Armed Robberies data set
# Monthly armed robberies from Jan 1966 - Oct 1975
bar.raw <- read.csv("bar.csv", header = TRUE)

# Format into time series object
bar.ts <- ts(bar.raw$armRob, frequency = 12, start = 1966)

# initial look at data
# Observations:
#   Increasing linear trend.
#   May be some seasonality?
#   Quite a bit of autocorrelation. We have a slow decreasing ACF, an ARMA/ARIMA
#   model may be appropriate
plot(bar.ts)
acf(bar.ts)

# take log to account for increasing variation
# bar.trans <- log(bar.ts)

# demean data
bar.trans <- bar.trans - mean(bar.trans)

# first difference
bar.trans <- diff(bar.trans, differences = 1)

# check stationarity
# Observations:
#   Appears stationary in both plots!
plot(bar.trans)
acf(bar.trans, lag.max = 30)

# pacf is not complicated. AR model may be sufficient.
pacf(bar.trans)

# Use ACF to pick value of p for AR(p) model.
# Try AR(1)
acf(bar.trans)
pacf(bar.trans)
ar1.mod <- arima(x = bar.trans, order = c(1,0,0), method = 'CSS', include.mean = FALSE)

# check residuals to see if AR(1) is an adequate fit of the model.
# Observations:
#   Appears that residuals are approx white noise.
#   May have significant ACF at lag 2. 
#   Ljung-Box test also insignificant at lag = 30 (p = 0.387)
#   These results indicate that this model is likely sufficient to summarize these
#   data. 
plot(ar1.mod$residuals)
acf(ar1.mod$residuals)
Box.test(x = ar1.mod$residuals, type = "Ljung-Box", lag = 30)

forc <- predict(ar1.mod, n.ahead = 5, se.fit = TRUE)

forc.val <- forc$pred
alldata <- ts(c(bar.trans, forc.val), frequency = 12, start = 1966)

alldata.ud <- diffinv(alldata, differences = 1, xi = log(bar.ts)[1] - mean(log(bar.ts)))
alldata.ud.rm <- alldata.ud + mean(log(bar.ts))
alldata.ud.rm.exp <- exp(alldata.ud.rm)



plot(alldata.ud)


# Lets try predicting
bar.pred <- predict(ar1.mod, n.ahead = 5, se.fit = TRUE)
pred_value = bar.pred$pred

pred_lower = pred_value - 1.96 * bar.pred$se
pred_upper = pred_value + 1.96 * bar.pred$se

bar <- backTransform(ts = bar.trans, xi = log(bar.ts)[1], mean = mean(log(bar.ts)))
pv <- backTransform(ts = pred_value, xi = log(bar.ts)[118], mean = mean(log(bar.ts)))
pu <- backTransform(ts = pred_upper, xi = log(bar.ts)[1], mean = mean(log(bar.ts)))



ts.plot(bar.trans, pred_value, pred_lower, pred_upper, gpars = list(col = c("black", "red", "blue", "blue")))

backTransform <- function(ts, xi, mean){
  ts.ud <- diffinv(ts, differences = 1, xi = xi)
  ts.ud.rm <- ts.ud + mean
  ts.ud.rm.exp <- exp(ts.ud.rm)
  return(ts.ud.rm.exp)
}


hist_forc <- ts(c(bar.trans, bar.pred.5$pred), freq = 12, start = 1966)
plot(hist_forc)

hist_forc <- hist_forc + mean(log(bar.ts))

# back transform prediction
hist_forc_bt <- diffinv(hist_forc, differences = 1, xi = bar.ts[1])
hist_forc_bt <- hist_forc + mean(log(bar.ts))
hist_forc_bt <- exp(hist_forc_bt)

hist_forc_bt <- ts(hist_forc_bt, frequency = 12, start = 1966)
ts.plot(hist_forc_bt[1:118], hist_forc_bt[119:length(hist_forc_bt)], gpars = list(col = c("black", "red")))
