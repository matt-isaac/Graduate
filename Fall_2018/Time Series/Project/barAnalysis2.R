## Questions: 
## 
## 1. Does the order of log & demean matter? (subtracting off the mean gives
##    negative values which result in NA's with the log). 
##    
## 2. AR(1) model seems appropriate. Is this reasonable?
##    * 1 is the last significant lag on ACF, PACF seems relatively uncomplicated...
##    
## 3. Why would forecast not begin where time series ends?
## 
## 4. How do I transform the confidence interval? What is the starting point (xi)
##    for the upper and lower interval bounds?
##    
## 5. Do the forecasted values look reasonable?
## 
## 6. Does there appear to be seasonality (higher values in July)?
## 
## 7. Why is auto.arima() giving me two models/line?

library(forecast)
library(ggplot2)

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
bar.log <- log(bar.ts)

# demean data
bar.log.dm <- bar.log - mean(bar.log)

# first difference
bar.log.dm.diff <- diff(bar.log.dm, differences = 1)
bar.sta <- bar.log.dm.diff

# check stationarity
# Observations:
#   Appears stationary in both plots!
plot(bar.sta)
acf(bar.sta, lag.max = 50)

# pacf is not complicated. AR model may be sufficient.
pacf(bar.sta, lag.max = 50)
 
# Use ACF to pick value of p for AR(p) model.
# Try AR(1)
acf(bar.sta)
ar1.mod <- arima(x = bar.sta, order = c(1,0,0), method = 'CSS-ML', include.mean = FALSE)

# Try auto.arima to get another candidate model
auto.arima(y = bar.sta, d = 0, D = 0, max.p = 4, max.q = 4, stepwise = FALSE, trace = TRUE)

ar1.ma3.mod <- arima(x = bar.sta, order = c(1,0,3), method = 'ML', include.mean = FALSE, transform.pars = FALSE)
 


# check residuals to see if AR(1) is an adequate fit of the model.
# Observations:
#   Appears that residuals are approx white noise.
#   May have significant ACF at lag 2.
#   Ljung-Box test also insignificant at lag = 30 (p = 0.387)
#   These results indicate that this model is likely sufficient to summarize these
#   data.
plot(ar1.mod$residuals)
acf(ar1.mod$residuals, lag.max = 30) # AR(1) does appear to be sufficient 

plot(ar1.ma3.mod$residuals)
acf(ar1.ma3.mod$residuals, lag.max = 30) # ARMA(1,3) looks even better

Box.test(x = ar1.mod$residuals, type = "Ljung-Box", lag = 30) 
Box.test(x = ar1.ma3.mod$residuals, type = "Ljung-Box", lag = 30) 

###########################################
### Proceed with ARMA(1,3) model ##########
###########################################

preds <- predict(ar1.ma3.mod, n.ahead = 5, se.fit = TRUE)
pred_val <- preds$pred
pred_lower <- pred_val - 1.96*preds$se
pred_upper <- pred_val + 1.96*preds$se

pred_val

ts.plot(bar.sta, preds$pred, pred_lower, pred_upper, gpars = list(col = c("black", "red", "blue", "blue")))

# undo transformations
# bar <- exp(diffinv(bar.sta, differences = 1, xi = log(bar.ts[1])) + mean(log(bar.ts)))
pred.bt <- exp(diffinv(pred_val, differences = 1, xi = log(bar.ts[118])) + mean(log(bar.ts)))

# plot original data and backtransformed forecasts
ts.plot(bar, pred.bt, gpars = list(col = c("black", "red")))

###########################################
#### visualizations #######################
###########################################

autoplot(bar.ts) +
  geom_smooth(se = FALSE)

ggseasonplot(bar.ts)
ggseasonplot(bar.ts, polar = TRUE)
ggsubseriesplot(bar.ts)

autoplot(bar.ts) +
  geom_smooth(se = FALSE)

ggseasonplot(bar.sta)
ggseasonplot(bar.sta, polar = TRUE)
ggsubseriesplot(bar.sta)

gglagplot(bar.ts)
gglagplot(bar.sta)
ggAcf(bar.sta)
acf(bar.sta)

