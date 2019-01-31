library(forecast)
library(ggplot2)
library(gridExtra)
library(ggfortify)
library(knitr)

# read in raw data
bar.raw <- read.csv("bar.csv", header = TRUE)
# create time series object
bar <- ts(bar.raw$armRob, frequency = 12, start = 1966)

# create data frame for plot
df.bar <- data.frame(ts = bar, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))                  

# plot raw data
rts.plot <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  xlab('Time') +
  ylab('Monthly Armed Robberies') +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))
rts.plot

# ACF of raw data
acf(bar, main = "")
# PACF of raw data
pacf(bar, main = "")

# Transform to stationary time series
bar.l <- log(bar)
bar.l.diff <- diff(bar.l, diff = 1)
# acf(bar.l.diff, lag = 30)
# pacf(bar.l.diff, lag = 30)
# plot(bar.l.diff)

# de-mean data
mu <- mean(bar.l.diff)
bar.l.diff.dm <- bar.l.diff - mu
# final stationary time series
bar.sta <- bar.l.diff.dm

# Data frame creatd for visualization
df.bar.sta <- data.frame(ts = bar.sta, time = seq(1966 + (2/12), (1975 + (10/12)), by = (1/12)))
# plot raw time series
ggplot() + 
  geom_line(data = df.bar.sta, aes(x = time, y = ts), color = "navyblue", size = 1) +
  xlab('Time') +
  ylab('Frequency') +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1)) +
  scale_y_continuous(limits = c(-0.6, 0.6), breaks = round(seq(-0.6, 0.6, by = 0.2),3))

# ACF of stationary time series
acf(bar.sta, main = "")
# PACF of stationary time series
pacf(bar.sta, main = "")

# Models Considered:
# bar.mod.ar2 <- arima(bar.sta, order = c(2, 0, 0), include.mean = FALSE, method = "CSS-ML")
bar.mod.ma2 <- arima(bar.sta, order = c(0, 0, 2), include.mean = FALSE, method = "CSS-ML")
bar.mod.arma11 <- arima(bar.sta, order = c(1, 0, 1), include.mean = FALSE, method = "CSS-ML")
bar.mod.arma12 <- arima(bar.sta, order = c(1, 0, 2), include.mean = FALSE, method = "CSS-ML")
bar.mod.arma21 <- arima(bar.sta, order = c(2,0,1), include.mean = FALSE, method = "CSS-ML")
bar.mod.arma22 <- arima(bar.sta, order = c(2,0,2), include.mean = FALSE, method = "CSS-ML")

# First, look at ARMA(2,2), since it has the smallest sigma^2 value
bar.mod <- bar.mod.arma22

# ACF of model residuals
acf(bar.mod$residuals, lag = 40, main = "")
# PACF of model residuals
pacf(bar.mod$residuals, lag = 40, main = "")
# Ljung-Box test of residuals
bt <- Box.test(bar.mod$residuals, lag = 40, type = "Ljung-Box")

#Using the predict function
bar_pred = predict(bar.mod, n.ahead=12,se.fit=T)
pred_value = bar_pred$pred + mu

# Confidence interval
pred_lower=pred_value - (1.96 * bar_pred$se)
pred_upper=pred_value + (1.96 * bar_pred$se)

# for visualization
df.bar.sta <- data.frame(ts = bar.sta, time = seq(1966 + (2/12), (1975 + (10/12)), by = (1/12)))
df.pred.sta <- data.frame(ts = pred_value, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))
df.upper.sta <- data.frame(ts = pred_upper, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))
df.lower.sta <- data.frame(ts = pred_lower, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))

# Plot of  predicted values, confidence interval, and stationary
# times series
plot_sta <- ggplot() + 
  geom_line(data = df.bar.sta, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.pred.sta, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.upper.sta, aes(x = time, y = ts), color = "blue", size = 1) +
  geom_line(data = df.lower.sta, aes(x = time, y = ts), color = "blue", size = 1) +
  xlab('Time') +
  ylab('Frequency') +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))+
  scale_y_continuous(limits = c(-0.6, 0.6), breaks = round(seq(-0.6, 0.6, by = 0.2),3))
plot_sta

#plot fitted and predicted values together with the observed data
bar_fit_pred=ts(rep(0,118),start=1966,frequency=12)
bar_fit_pred[2:118]=bar.sta-bar.mod$residuals + mu
# bar_fit_pred[119:128]=pred_value
# ts.plot(bar.sta,bar_fit_pred,gpars=list(col=c('black','red','blue','blue')))
df.bar.fitted.sta <- data.frame(ts = bar_fit_pred, time = seq(1966 + (1/12), (1975 + (10/12)), by = (1/12)))

# add fitted values to plot
plot_sta <- plot_sta +
  geom_line(data = df.bar.fitted.sta, aes(x = time, y = ts), color = "red", size = 1)
plot_sta

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

df.forc.back <- data.frame(ts = bar_forc_back, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))
df.forc.lead <- data.frame(ts = bar_forc_lead, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.forc.upper <- data.frame(ts = bar_forc_upper, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.forc.lower <- data.frame(ts = bar_forc_lower, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.bar <- data.frame(ts = log(bar), time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))

par(mfrow = c(2,1))

# Predictions on log scale
plot.ls <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.forc.back, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.upper, aes(x = time, y = ts), color = "blue", size = 1) +
  geom_line(data = df.forc.lower, aes(x = time, y = ts), color = "blue", size = 1) +
  xlab('Time') +
  ylab('log(Monthly Armed Robberies)') +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))

plot.ls

# Predictions on original scale
bar_forc_back <- exp(bar_forc_back)
bar_forc_lead <- exp(bar_forc_lead)

df.forc.back <- data.frame(ts = bar_forc_back, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))
df.forc.lead <- data.frame(ts = bar_forc_lead, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.bar <- data.frame(ts = bar, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))

plot.os <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.forc.back, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  xlab('Time') +
  ylab('Monthly Armed Robberies') +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))

plot.os

############ 
############ Try ARMA(1,2)
############ 
bar.mod <- bar.mod.arma12
# ACF of model residuals
acf(bar.mod$residuals, lag = 40, main = "")
# PACF of model residuals
pacf(bar.mod$residuals, lag = 40, main = "")
# Ljung-Box test of residuals
bt <- Box.test(bar.mod$residuals, lag = 40, type = "Ljung-Box")

#Using the predict function
bar_pred = predict(bar.mod, n.ahead=12,se.fit=T)
pred_value = bar_pred$pred + mu

# Confidence interval
pred_lower=pred_value - (1.96 * bar_pred$se)
pred_upper=pred_value + (1.96 * bar_pred$se)

# for visualization
df.bar.sta <- data.frame(ts = bar.sta, time = seq(1966 + (2/12), (1975 + (10/12)), by = (1/12)))
df.pred.sta <- data.frame(ts = pred_value, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))
df.upper.sta <- data.frame(ts = pred_upper, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))
df.lower.sta <- data.frame(ts = pred_lower, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))

# Plot of  predicted values, confidence interval, and stationary
# times series
plot_sta <- ggplot() + 
  geom_line(data = df.bar.sta, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.pred.sta, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.upper.sta, aes(x = time, y = ts), color = "blue", size = 1) +
  geom_line(data = df.lower.sta, aes(x = time, y = ts), color = "blue", size = 1) +
  xlab('Time') +
  ylab('Frequency') +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))+
  scale_y_continuous(limits = c(-0.6, 0.6), breaks = round(seq(-0.6, 0.6, by = 0.2),3))
plot_sta

#plot fitted and predicted values together with the observed data
bar_fit_pred=ts(rep(0,118),start=1966,frequency=12)
bar_fit_pred[2:118]=bar.sta-bar.mod$residuals + mu
# bar_fit_pred[119:128]=pred_value
# ts.plot(bar.sta,bar_fit_pred,gpars=list(col=c('black','red','blue','blue')))
df.bar.fitted.sta <- data.frame(ts = bar_fit_pred, time = seq(1966 + (1/12), (1975 + (10/12)), by = (1/12)))

# add fitted values to plot
plot_sta <- plot_sta +
  geom_line(data = df.bar.fitted.sta, aes(x = time, y = ts), color = "red", size = 1)
plot_sta

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

df.forc.back <- data.frame(ts = bar_forc_back, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))
df.forc.lead <- data.frame(ts = bar_forc_lead, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.forc.upper <- data.frame(ts = bar_forc_upper, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.forc.lower <- data.frame(ts = bar_forc_lower, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.bar <- data.frame(ts = log(bar), time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))

par(mfrow = c(2,1))

# Predictions on log scale
plot.ls <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.forc.back, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.upper, aes(x = time, y = ts), color = "blue", size = 1) +
  geom_line(data = df.forc.lower, aes(x = time, y = ts), color = "blue", size = 1) +
  xlab('Time') +
  ylab('log(Monthly Armed Robberies)') +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))

plot.ls

# Predictions on original scale
bar_forc_back <- exp(bar_forc_back)
bar_forc_lead <- exp(bar_forc_lead)

df.forc.back <- data.frame(ts = bar_forc_back, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))
df.forc.lead <- data.frame(ts = bar_forc_lead, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.bar <- data.frame(ts = bar, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))

plot.os <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.forc.back, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  xlab('Time') +
  ylab('Monthly Armed Robberies') +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))

plot.os

############ 
############ Try ARMA(2,1)
############ 
bar.mod <- bar.mod.arma21

# ACF of model residuals
acf(bar.mod$residuals, lag = 40, main = "")
# PACF of model residuals
pacf(bar.mod$residuals, lag = 40, main = "")
# Ljung-Box test of residuals
bt <- Box.test(bar.mod$residuals, lag = 40, type = "Ljung-Box")

#Using the predict function
bar_pred = predict(bar.mod, n.ahead=12,se.fit=T)
pred_value = bar_pred$pred + mu

# Confidence interval
pred_lower=pred_value - (1.96 * bar_pred$se)
pred_upper=pred_value + (1.96 * bar_pred$se)

# for visualization
df.bar.sta <- data.frame(ts = bar.sta, time = seq(1966 + (2/12), (1975 + (10/12)), by = (1/12)))
df.pred.sta <- data.frame(ts = pred_value, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))
df.upper.sta <- data.frame(ts = pred_upper, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))
df.lower.sta <- data.frame(ts = pred_lower, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))

# Plot of  predicted values, confidence interval, and stationary
# times series
plot_sta <- ggplot() + 
  geom_line(data = df.bar.sta, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.pred.sta, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.upper.sta, aes(x = time, y = ts), color = "blue", size = 1) +
  geom_line(data = df.lower.sta, aes(x = time, y = ts), color = "blue", size = 1) +
  xlab('Time') +
  ylab('Frequency') +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))+
  scale_y_continuous(limits = c(-0.6, 0.6), breaks = round(seq(-0.6, 0.6, by = 0.2),3))
plot_sta

#plot fitted and predicted values together with the observed data
bar_fit_pred=ts(rep(0,118),start=1966,frequency=12)
bar_fit_pred[2:118]=bar.sta-bar.mod$residuals + mu
# bar_fit_pred[119:128]=pred_value
# ts.plot(bar.sta,bar_fit_pred,gpars=list(col=c('black','red','blue','blue')))
df.bar.fitted.sta <- data.frame(ts = bar_fit_pred, time = seq(1966 + (1/12), (1975 + (10/12)), by = (1/12)))

# add fitted values to plot
plot_sta <- plot_sta +
  geom_line(data = df.bar.fitted.sta, aes(x = time, y = ts), color = "red", size = 1)
plot_sta

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

df.forc.back <- data.frame(ts = bar_forc_back, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))
df.forc.lead <- data.frame(ts = bar_forc_lead, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.forc.upper <- data.frame(ts = bar_forc_upper, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.forc.lower <- data.frame(ts = bar_forc_lower, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.bar <- data.frame(ts = log(bar), time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))

par(mfrow = c(2,1))

# Predictions on log scale
plot.ls <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.forc.back, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.upper, aes(x = time, y = ts), color = "blue", size = 1) +
  geom_line(data = df.forc.lower, aes(x = time, y = ts), color = "blue", size = 1) +
  xlab('Time') +
  ylab('log(Monthly Armed Robberies)') +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))

plot.ls

# Predictions on original scale
bar_forc_back <- exp(bar_forc_back)
bar_forc_lead <- exp(bar_forc_lead)

df.forc.back <- data.frame(ts = bar_forc_back, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))
df.forc.lead <- data.frame(ts = bar_forc_lead, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.bar <- data.frame(ts = bar, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))

plot.os <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.forc.back, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  xlab('Time') +
  ylab('Monthly Armed Robberies') +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))

plot.os

############ 
############ Try ARMA(1,1)
############ 
bar.mod <- bar.mod.arma11

# ACF of model residuals
acf(bar.mod$residuals, lag = 40, main = "")
# PACF of model residuals
pacf(bar.mod$residuals, lag = 40, main = "")
# Ljung-Box test of residuals
bt <- Box.test(bar.mod$residuals, lag = 40, type = "Ljung-Box")

#Using the predict function
bar_pred = predict(bar.mod, n.ahead=12,se.fit=T)
pred_value = bar_pred$pred + mu

# Confidence interval
pred_lower=pred_value - (1.96 * bar_pred$se)
pred_upper=pred_value + (1.96 * bar_pred$se)

# for visualization
df.bar.sta <- data.frame(ts = bar.sta, time = seq(1966 + (2/12), (1975 + (10/12)), by = (1/12)))
df.pred.sta <- data.frame(ts = pred_value, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))
df.upper.sta <- data.frame(ts = pred_upper, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))
df.lower.sta <- data.frame(ts = pred_lower, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))

# Plot of  predicted values, confidence interval, and stationary
# times series
plot_sta <- ggplot() + 
  geom_line(data = df.bar.sta, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.pred.sta, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.upper.sta, aes(x = time, y = ts), color = "blue", size = 1) +
  geom_line(data = df.lower.sta, aes(x = time, y = ts), color = "blue", size = 1) +
  xlab('Time') +
  ylab('Frequency') +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))+
  scale_y_continuous(limits = c(-0.6, 0.6), breaks = round(seq(-0.6, 0.6, by = 0.2),3))
plot_sta

#plot fitted and predicted values together with the observed data
bar_fit_pred=ts(rep(0,118),start=1966,frequency=12)
bar_fit_pred[2:118]=bar.sta-bar.mod$residuals + mu
# bar_fit_pred[119:128]=pred_value
# ts.plot(bar.sta,bar_fit_pred,gpars=list(col=c('black','red','blue','blue')))
df.bar.fitted.sta <- data.frame(ts = bar_fit_pred, time = seq(1966 + (1/12), (1975 + (10/12)), by = (1/12)))

# add fitted values to plot
plot_sta <- plot_sta +
  geom_line(data = df.bar.fitted.sta, aes(x = time, y = ts), color = "red", size = 1)
plot_sta

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

df.forc.back <- data.frame(ts = bar_forc_back, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))
df.forc.lead <- data.frame(ts = bar_forc_lead, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.forc.upper <- data.frame(ts = bar_forc_upper, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.forc.lower <- data.frame(ts = bar_forc_lower, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.bar <- data.frame(ts = log(bar), time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))

par(mfrow = c(2,1))

# Predictions on log scale
plot.ls <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.forc.back, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.upper, aes(x = time, y = ts), color = "blue", size = 1) +
  geom_line(data = df.forc.lower, aes(x = time, y = ts), color = "blue", size = 1) +
  xlab('Time') +
  ylab('log(Monthly Armed Robberies)') +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))

plot.ls

# Predictions on original scale
bar_forc_back <- exp(bar_forc_back)
bar_forc_lead <- exp(bar_forc_lead)

df.forc.back <- data.frame(ts = bar_forc_back, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))
df.forc.lead <- data.frame(ts = bar_forc_lead, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.bar <- data.frame(ts = bar, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))

plot.os <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.forc.back, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  xlab('Time') +
  ylab('Monthly Armed Robberies') +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))

plot.os

############ 
############ Try MA(2)
############ 
bar.mod <- bar.mod.ma2

# ACF of model residuals
acf(bar.mod$residuals, lag = 40, main = "")
# PACF of model residuals
pacf(bar.mod$residuals, lag = 40, main = "")
# Ljung-Box test of residuals
bt <- Box.test(bar.mod$residuals, lag = 40, type = "Ljung-Box")

#Using the predict function
bar_pred = predict(bar.mod, n.ahead=12,se.fit=T)
pred_value = bar_pred$pred + mu

# Confidence interval
pred_lower=pred_value - (1.96 * bar_pred$se)
pred_upper=pred_value + (1.96 * bar_pred$se)

# for visualization
df.bar.sta <- data.frame(ts = bar.sta, time = seq(1966 + (2/12), (1975 + (10/12)), by = (1/12)))
df.pred.sta <- data.frame(ts = pred_value, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))
df.upper.sta <- data.frame(ts = pred_upper, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))
df.lower.sta <- data.frame(ts = pred_lower, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))

# Plot of  predicted values, confidence interval, and stationary
# times series
plot_sta <- ggplot() + 
  geom_line(data = df.bar.sta, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.pred.sta, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.upper.sta, aes(x = time, y = ts), color = "blue", size = 1) +
  geom_line(data = df.lower.sta, aes(x = time, y = ts), color = "blue", size = 1) +
  xlab('Time') +
  ylab('Frequency') +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))+
  scale_y_continuous(limits = c(-0.6, 0.6), breaks = round(seq(-0.6, 0.6, by = 0.2),3))
plot_sta

#plot fitted and predicted values together with the observed data
bar_fit_pred=ts(rep(0,118),start=1966,frequency=12)
bar_fit_pred[2:118]=bar.sta-bar.mod$residuals + mu
# bar_fit_pred[119:128]=pred_value
# ts.plot(bar.sta,bar_fit_pred,gpars=list(col=c('black','red','blue','blue')))
df.bar.fitted.sta <- data.frame(ts = bar_fit_pred, time = seq(1966 + (1/12), (1975 + (10/12)), by = (1/12)))

# add fitted values to plot
plot_sta <- plot_sta +
  geom_line(data = df.bar.fitted.sta, aes(x = time, y = ts), color = "red", size = 1)
plot_sta

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

df.forc.back <- data.frame(ts = bar_forc_back, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))
df.forc.lead <- data.frame(ts = bar_forc_lead, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.forc.upper <- data.frame(ts = bar_forc_upper, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.forc.lower <- data.frame(ts = bar_forc_lower, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.bar <- data.frame(ts = log(bar), time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))

par(mfrow = c(2,1))

# Predictions on log scale
plot.ls <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.forc.back, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.upper, aes(x = time, y = ts), color = "blue", size = 1) +
  geom_line(data = df.forc.lower, aes(x = time, y = ts), color = "blue", size = 1) +
  xlab('Time') +
  ylab('log(Monthly Armed Robberies)') +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))

plot.ls

# Predictions on original scale
bar_forc_back <- exp(bar_forc_back)
bar_forc_lead <- exp(bar_forc_lead)

df.forc.back <- data.frame(ts = bar_forc_back, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))
df.forc.lead <- data.frame(ts = bar_forc_lead, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.bar <- data.frame(ts = bar, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))

plot.os <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.forc.back, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  xlab('Time') +
  ylab('Monthly Armed Robberies') +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))

plot.os

############ 
############ Try AR(2)
############ 
bar.mod <- bar.mod.ar2

# ACF of model residuals
acf(bar.mod$residuals, lag = 40, main = "")
# PACF of model residuals
pacf(bar.mod$residuals, lag = 40, main = "")
# Ljung-Box test of residuals
bt <- Box.test(bar.mod$residuals, lag = 40, type = "Ljung-Box")

#Using the predict function
bar_pred = predict(bar.mod, n.ahead=12,se.fit=T)
pred_value = bar_pred$pred + mu

# Confidence interval
pred_lower=pred_value - (1.96 * bar_pred$se)
pred_upper=pred_value + (1.96 * bar_pred$se)

# for visualization
df.bar.sta <- data.frame(ts = bar.sta, time = seq(1966 + (2/12), (1975 + (10/12)), by = (1/12)))
df.pred.sta <- data.frame(ts = pred_value, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))
df.upper.sta <- data.frame(ts = pred_upper, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))
df.lower.sta <- data.frame(ts = pred_lower, time = seq((1975 + (11/12)), (1976 + (10/12)), by = (1/12)))

# Plot of  predicted values, confidence interval, and stationary
# times series
plot_sta <- ggplot() + 
  geom_line(data = df.bar.sta, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.pred.sta, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.upper.sta, aes(x = time, y = ts), color = "blue", size = 1) +
  geom_line(data = df.lower.sta, aes(x = time, y = ts), color = "blue", size = 1) +
  xlab('Time') +
  ylab('Frequency') +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))+
  scale_y_continuous(limits = c(-0.6, 0.6), breaks = round(seq(-0.6, 0.6, by = 0.2),3))
plot_sta

#plot fitted and predicted values together with the observed data
bar_fit_pred=ts(rep(0,118),start=1966,frequency=12)
bar_fit_pred[2:118]=bar.sta-bar.mod$residuals + mu
# bar_fit_pred[119:128]=pred_value
# ts.plot(bar.sta,bar_fit_pred,gpars=list(col=c('black','red','blue','blue')))
df.bar.fitted.sta <- data.frame(ts = bar_fit_pred, time = seq(1966 + (1/12), (1975 + (10/12)), by = (1/12)))

# add fitted values to plot
plot_sta <- plot_sta +
  geom_line(data = df.bar.fitted.sta, aes(x = time, y = ts), color = "red", size = 1)
plot_sta

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

df.forc.back <- data.frame(ts = bar_forc_back, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))
df.forc.lead <- data.frame(ts = bar_forc_lead, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.forc.upper <- data.frame(ts = bar_forc_upper, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.forc.lower <- data.frame(ts = bar_forc_lower, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.bar <- data.frame(ts = log(bar), time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))

par(mfrow = c(2,1))

# Predictions on log scale
plot.ls <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.forc.back, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.upper, aes(x = time, y = ts), color = "blue", size = 1) +
  geom_line(data = df.forc.lower, aes(x = time, y = ts), color = "blue", size = 1) +
  xlab('Time') +
  ylab('log(Monthly Armed Robberies)') +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))

plot.ls

# Predictions on original scale
bar_forc_back <- exp(bar_forc_back)
bar_forc_lead <- exp(bar_forc_lead)

df.forc.back <- data.frame(ts = bar_forc_back, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))
df.forc.lead <- data.frame(ts = bar_forc_lead, time = seq((1975 + 10/12), (1976 + (10/12)), by = (1/12)))
df.bar <- data.frame(ts = bar, time = seq((1966 + 1/12), (1975 + (10/12)), by = (1/12)))

plot.os <- ggplot() + 
  geom_line(data = df.bar, aes(x = time, y = ts), color = "navyblue", size = 1) +
  geom_line(data = df.forc.back, aes(x = time, y = ts), color = "red", size = 1) +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  xlab('Time') +
  ylab('Monthly Armed Robberies') +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100)) +
  scale_x_continuous(limits = c(1966, 1977), breaks = seq(1966, 1977, by = 1))

plot.os

plot.os2 <- plot.os + 
  geom_hline(yintercept=df.forc.lead$ts[13], linetype="longdash", color = "darkred") +
  geom_line(data = df.forc.lead, aes(x = time, y = ts), color = "red", size = 1) +
  geom_hline(yintercept=df.bar$ts[118], linetype="longdash", color = "navyblue") +
  geom_point(aes(x = (1976 + (10/12)), y = df.forc.lead$ts[13]), size = 3, col = "red") +
  geom_point(aes(x = (1975 + (10/12)), y = df.bar$ts[118]), size = 3, col = "navyblue") +
  annotate("text", label = paste("about ", round(df.forc.lead$ts[13],0)), x = 1976, y = 565, color = "darkred") +
  annotate("text", label = df.bar$ts[118], x = 1976.5, y = 415, color = "navyblue")

plot.os2

