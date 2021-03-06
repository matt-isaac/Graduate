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

bar.log.diff <- diff(bar.log, differences = 1)
bar.log.diff.dm <- bar.log.diff - mean(bar.log.diff)
bar.sta <- bar.log.diff.dm

plot(bar.sta)
acf(bar.sta)
pacf(bar.sta)

bar.mod1 <- arima(x = bar.sta, order = c(1, 0, 10), include.mean = FALSE, method = "CSS-ML")
acf(bar.mod1$residuals, lag.max = 50)

forc <- predict(bar.mod1, n.ahead = 5)
pred_val <- forc$pred
pred_upper <- pred_val + (1.96*forc$se)
pred_lower <- pred_val - (1.96*forc$se)

ts.plot(bar.sta, pred_val, pred_upper, pred_lower, gpars=list(col = c("black", "red", "blue", "blue")))

pred_val_bt <- exp(diffinv((pred_val + mean(bar.log.diff)), xi = log(bar.ts[118])))

upper_strt <- bar.ts[118] + 1.96 * sqrt(bar.mod1$sigma2)
lower_strt <- bar.ts[118] - 1.96 * sqrt(bar.mod1$sigma2)
pred_upper_bt <- exp(diffinv((pred_upper + mean(bar.log.diff)), xi = log(upper_strt)))
pred_lower_bt <- exp(diffinv((pred_lower + mean(bar.log.diff)), xi = log(lower_strt)))

ts.plot(bar.ts, pred_val_bt, gpars=list(col = c("black", "red")))
ts.plot(bar.ts, pred_val_bt, pred_upper_bt, pred_lower_bt, gpars=list(col = c("black", "red", "blue", "blue")))
 
