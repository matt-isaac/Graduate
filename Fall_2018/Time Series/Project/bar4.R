library(forecast)
library(ggplot2)

# read in Boston Armed Robberies data set
# Monthly armed robberies from Jan 1966 - Oct 1975
bar.raw <- read.csv("bar.csv", header = TRUE)

# Format into time series object
bar <- ts(bar.raw$armRob, frequency = 12, start = 1966)

# initial look at data
# Observations:
#   Increasing linear trend.
#   May be some seasonality?
#   Quite a bit of autocorrelation. We have a slow decreasing ACF, an ARMA/ARIMA
#   model may be appropriate
plot(bar)
acf(bar)
pacf(bar)

# Transform to stationarity
bar.l <- log(bar)
bar.l.diff <- diff(bar.l, diff = 1)
acf(bar.l.diff, lag = 30)
pacf(bar.l.diff, lag = 30)
plot(bar.l.diff)

mu <- mean(bar.l.diff)
bar.l.diff.dm <- bar.l.diff - mu
bar.sta <- bar.l.diff.dm

# fit arma(1, 10) model
# bar.mod <- arima(bar.sta, order = c(1, 0, 1), include.mean = FALSE)
# bar.mod <- arima(bar.sta, order = c(0, 0, 2), include.mean = FALSE)
bar.auto <- auto.arima(bar.sta, max.p = 5, max.q = 5, max.d = 0,stepwise = FALSE, max.P = 0, max.Q = 0, max.D = 0, trace = TRUE)
bar.mod <- arima(bar.sta, order = c(2, 0, 0), include.mean = FALSE, method = "CSS-ML")
bar.mod <- arima(bar.sta, order = c(2, 0, 2), include.mean = FALSE, method = "CSS-ML")
bar.mod <- arima(bar.sta, order = c(1, 0, 2), include.mean = FALSE, method = "CSS-ML")
bar.mod <- arima(bar.sta, order = c(0, 0, 2), include.mean = FALSE, method = "CSS-ML")



# bar.fmod <- Arima(bar.sta, order = c(2, 0, 0), include.mean = FALSE, method = "CSS-ML")
# plot(forecast(bar.fmod))
# fc <- forecast(bar.fmod)



# model fits well
acf(bar.mod$residuals, lag = 40)
pacf(bar.mod$residuals, lag = 40)
Box.test(bar.mod$residuals, lag = 40, type = "Ljung-Box")
qqnorm(bar.mod$residuals)
qqline(bar.mod$residuals)

#Using the predict function
bar_pred = predict(bar.mod, n.ahead=12,se.fit=T)
pred_value = bar_pred$pred + mu

pred_lower=pred_value - (1.96 * bar_pred$se)
pred_upper=pred_value + (1.96 * bar_pred$se)

#plot predicts with data
ts.plot(bar.sta,pred_value,pred_lower,pred_upper,gpars=list(col=c('black','red','blue','blue')))

#plot fitted and predicted values together with the observed data
bar_fit_pred=ts(rep(0,118),start=1966,frequency=12)
bar_fit_pred[2:118]=bar.sta-bar.mod$residuals + mu
# bar_fit_pred[119:128]=pred_value
ts.plot(bar.sta,bar_fit_pred,gpars=list(col=c('black','red','blue','blue')))
ts.plot(bar.sta,bar_fit_pred,pred_upper, pred_lower, pred_value, gpars=list(col=c('black','red','blue','blue', 'red')))

# Starting points for CIs
# BJS_forc[151]-1.96*BJS1_pred$se[1] 

#undo differencing
bar_forc_back=diffinv(bar_fit_pred[2:118], differences=1, xi=log(bar[1]))
bar_forc_lead=diffinv(pred_value, differences=1, xi=log(bar[118]))

pe <- log(bar[117])
moe <- 1.96*sqrt(bar.mod$sigma2[1])
upper_start <- pe + moe
lower_start <- pe - moe

bar_forc_upper=diffinv(pred_upper, differences=1, xi = upper_start)
bar_forc_lower=diffinv(pred_lower, differences=1, xi = lower_start)

bar_forc_back=ts(bar_forc_back,start=1966,frequency=12)
bar_forc_lead=ts(bar_forc_lead,start=(1975+(10/12)),frequency=12)
bar_forc_upper=ts(bar_forc_upper,start=(1975+(10/12)),frequency=12)
bar_forc_lower=ts(bar_forc_lower,start=(1975+(10/12)),frequency=12)

ts.plot(log(bar),
        bar_forc_back, 
        bar_forc_lead, 
        bar_forc_upper,
        bar_forc_lower,
        gpars=list(col=c('black','red', 'red', 'blue', 'blue')))

# undo log transformation
bar_forc_back <- exp(bar_forc_back)
bar_forc_lead <- exp(bar_forc_lead)
bar_forc_upper <- exp(bar_forc_upper)
bar_forc_lower <- exp(bar_forc_lower)

ts.plot(bar,
        bar_forc_back, 
        bar_forc_lead,
        gpars=list(col=c('black','red', 'red')))

ts.plot(bar,
        bar_forc_back, 
        bar_forc_lead, 
        bar_forc_upper,
        bar_forc_lower,
        gpars=list(col=c('black','red', 'red', 'blue', 'blue')))










