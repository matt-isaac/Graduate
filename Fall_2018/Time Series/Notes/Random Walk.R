library(Rlab)
library(datasets)

rw.sim <- function(n){
  z <- rbern(n, 0.5)
  x <- (2 * z) - 1
  s <- cumsum(x)
  plot(s, type = 'l')
  return(s)
}

trend.sim <- function(n, v){
  y <- arima.sim(model = list(order = c(0,0,0)), n, mean = 0, sd = v)
  t = 1:n
  m = ts(t, start = 1, frequency = 1)
  x = m + y
  plot(x, type = 'l')
}

trend.sim(n = 100, v = 25)

seasonal.sim <- function(n, d, v){
  y <- arima.sim(model = list(order = c(0,0,0)), n, mean = 0, sd = v)
  t = 1:n
  s = ts(10 * sin(2*pi*t/d), start = 1, frequency = 1)
  x = s + y
  plot(x, type = 'l')
}

seasonal.sim(n=100, d = 20, v = 3)

trend.seasonal.sim <- function(n, d, v){
  y <- arima.sim(model = list(order = c(0,0,0)), n, mean = 0, sd = v)
  t = 1:n
  s = ts(10 * sin(2*pi*t/d), start = 1, frequency = 1)
  m = ts(t, start = 1, frequency = 1)
  x = s + m + y
  plot(x, type = 'l')
}

trend.seasonal.sim(n = 100, d = 20, v = 5)

# making time series object
t = 1:3
data = ts(t, start = 1, frequency = 1/7)

# Examples
plot(LakeHuron)
plot(AirPassengers)

# Pre-processing data. Remove all non-random components (increasing linear trend, seasonality,
# and increasing variability).

t <- 1:24
data <- ts(t, start = 1, frequency = 2)
plot(data, type = "p")

# lake huron data -> stationary
plot(LakeHuron)
?diff
LH_d1 <- diff(LakeHuron, lag = 1, differences = 1) # if you think the original trend is linear
plot(LH_d1)
LH_d2 <- diff(LakeHuron, lag = 1, differences = 2) # if you think the original trend is quadratic
plot(LH_d2) 
par(mfrow = c(2,1))
plot(LH_d1)
plot(LH_d2)

par(mfrow = c(1,1))
# monthly airline passengers -> stationary
plot(AirPassengers) 
# increasing variation can be taken care of the natural log. MUST be applied BEFORE differencing. 
lnAP <- log(AirPassengers)
plot(lnAP)
lnAP_12 <- diff(lnAP, lag = 12)
plot(lnAP_12)
# we can still believe this is a linear trend, and try removing it now. Or, we can go back and
# and remove linear trend first. 
lnAP_1 <- diff(lnAP, differences = 1)
plot(lnAP_1)
lnAP_1_12 <- diff(lnAP_1, lag = 12)
plot(lnAP_1_12) # no seasonal component, no linear trend. 

## 9-12-18
plot(LH_d1) # stationary by appearance. To formally check, assume it is stationary and 
# check the ACF. If the ACF is consistent with an ACF that is stationary, we can be sure its
# stationary.
acf(LH_d1) # rule of thumb: plot should not look too complicated, and should die out quickly. 
acf(LakeHuron) # strong indication of non-stationarity. Dies out too slowly.
acf(lnAP)
acf(lnAP_1)
acf(lnAP_1, lag.max = 50)
# stationary: acf has no pattern, dies out quickly. 


# arima.sim() simulates moving average (or any arima model)
x <- arima.sim(model = list(ma = 0.5), rand.gen = rnorm, sd = 1, n = 100)
plot(x)
acf(x)
x.200 <- arima.sim(model = list(ma = 0.5), rand.gen = rnorm, sd = 1, n = 200)
plot(x.200)
acf(x.200)

# calculate roots
polyroot(c(1, -0.75, 0.5625))
r <- polyroot(c(1, -0.75, 0.5625))
Mod(r)

# simulate Arima model
arma11 <- arima.sim(model = list(ar = c(1, -0.5), ma = c(1, 0.4)), sd = 1, rand.gen = rnorm, n = 200)
plot(arma11)

# AR(1)
ar1 <- arima.sim(model = list(ar = 2), n = 200)

data1_ar1 <- arima.sim(model=list(ar=-0.5), rand.gen=rnorm, sd=1, n=200)
plot(data1_ar1)
acf(data1_ar1)

# AR(2)
data1_ar2 <- arima.sim(model=list(ar=c(0.7, -0.1)), rand.gen=rnorm, sd=1, n=200)                       
plot(data1_ar2)
acf(data1_ar2)
             
data1_ar2 <- arima.sim(model=list(ar=c(8/9, -64/81)), rand.gen=rnorm, sd=1, n=200)                       
plot(data1_ar2)
acf(data1_ar2)

plot(BJsales)
                   
# remove linear trend:
BJlin <- diff(BJsales)
plot(BJlin)
acf(BJlin) # sample acf'

BJquad <- diff(BJsales, differences = 2)
plot(BJquad)
acf(BJquad)


# AR1 - geometrically decaying
# AR2 - 


# PACF
plot(BJsales)
BJS1 <- diff(BJsales)
acf(BJS1)
pacf(BJS1) # everything inside blue lines can be considered to be 0. So this could be an AR(2) model,
           # since we expect everything after p (for an AR(p)) to be 0 in the PACF. 

BJS2 = diff(BJsales, differences = 2)
acf(BJS2)
pacf(BJS2) #need to use pacf for AR models. AR(3) or AR(10) could be possibilites.

# 1. plot data
# 2. plot ACF
#  - check stationarity, correlation structure, seasonality, etc.
#  - seasonality - period ~ 10
# 
plot(sunspot.year)
acf(sunspot.year)
pacf(sunspot.year) # AR(9)


## arima()
BJS1_ar2_css = arima(BJS1, order = c(2, 0, 0), method = 'CSS')
BJS1_ar2_css

BJS1_demean = BJS1 - mean(BJS1)

BJS1_ar2_css = arima(BJS1_demean, order = c(2, 0, 0), method = 'CSS', include.mean = FALSE)
BJS1_ar2_css

BJS1_ar2_css = arima(BJS1_demean, order = c(2, 0, 0), method = 'CSS-ML', include.mean = FALSE)

BJS1_ar2_css$residuals
plot(BJS1_ar2_css$residuals)



# Demean and fit an AR model with no mean
BJS1_ar2 <- arima(BJS1-mean(BJS1), order = c(2,0,0), include.mean = FALSE, method = 
                 'CSS-ML')
BJS1_res = BJS1_ar2$residuals
plot(BJS1_res)
acf(BJS1_res) # plot acf to check if residuals resemble white noice process
# if all acf's are in 95% CI, we can assume white noise.
Box.test(BJS1_res, lag = 10, type = 'Ljung') # to be sure, we can still to the Ljung-Box test.
# How to know how many lags to test? Look for troublesome lags on ACF plots
# p-value = 0.5335. .Accept that first 10 autocorrelations are 0. 
se=sqrt(diag(BJS1_ar2$var.coef)) # variances are on the diagonals of the var/covar matrix.
BJS1_ar2
CI_low = BJS1_ar2$coef-1.96*se # CI's for estimates of phi_1 and phi_2
CI_upper = BJS1_ar2$coef+1.96*se # can also be used as a significance test (is 0 included?)

BJS1_ar2

phi <- BJS1_ar2$coef

# after differencing, we have {X2, ..., X150}. Want to calculate Xhat151, Xhat152, ..., Xhat155.
BJS1_forc <- ts(rep(0,155), start = 1, frequency = 1)
BJS1_forc[2:150] = BJS1-mean(BJS1)
for(t in 151:155){
  BJS1_forc[t] = phi[1] * BJS1_forc[t-1] + phi[2] * BJS1_forc[t-2]
}

BJS1_forc_addmean = BJS1_forc + mean(BJS1)


BJS1_predict = predict(BJS1_ar2, n.ahead = 5, se.fit=TRUE)
BJS1_predict$pred
pred_value = BJS1_predict$pred + mean(BJS1)

pred_lower = pred_value - 1.96 * BJS1_predict$se
pred_upper = pred_value + 1.96 * BJS1_predict$se

ts.plot(BJS1, pred_value, pred_lower, pred_upper, gpars = list(col = c("black", "red", "blue", "blue"))) #observed, 

# plot fitted and predicted values together. 
BJS1_fit_pred <- ts(rep(0,155), start = 1, frequency = 1)
BJS1_fit_pred[2:150] <- BJS1 - mean(BJS1) - BJS1_ar2$residuals + mean(BJS1) #means will cancel and simplify this

ts.plot(BJS1, BJS1_fit_pred, pred_lower, pred_upper, gpars=list(col = c("black", 'red', 'blue', 'blue')))

# model diagnostics : AIC, Log Likelihood, sigma^2
# AIC: smaller the better. Takes into account model complexity and how well it fits. 
# Likelihood: measures of purity on fitting. Large as possible = better model
# Sigma^2: smaller better.
BJS1_ar2

# undo the differencing
BJS_forc <- diffinv(BJS1_fit_pred[2:155], differences = 1, xi = BJsales[1])
BJS_forc <- ts(BJS_forc, start = 1, frequency = 1)
ts.plot(BJsales, BJS_forc, gpars = list(col = c('black', 'red')))

BJS_lower <- diffinv(pred_lower, differences = 1, xi = BJS1_forc[151] - 1.96 * BJS1_pred$se[1])
BJS_lower <- ts(BJS_lower, start = 151, frequency = 1)

BJS_upper <- diffinv(pred_upper, differences = 1, xi = BJS1_forc[151] + 1.96 * BJS1_pred$se[1])
BJS_upper <- ts(BJS_upper, start = 151, frequency = 1)

ts.plot(BJsales, BJS_forc, BJS_lower, BJS_upper, gpars = list(col = c("black", "red", "blue", "blue")))


# ARMAtoMA()
ARMAtoMA(ar = c(-0.4), lag.max = 10)

plot(IP)
IP_trans # transformed data (log, 1st order diff, lag 12)
ip_train <- IP_trans[1:816]
ip_vali <- 
acf(ip_train, lag.max = 36) #doesn't look very good. 
pacf(ip_train, lag.max = 36) # not great. PACF never dies. So AR model might not be a good choice. 
# What about MA?
acf(ip_train, lag.max = 36) #Look at the last significant ACF value. Max lag should be 16. MA(16) would be a good model.

ip_ma <- arima(ip_train-mean(ip_train), order = c(0, 0, 16), include.mean = FALSE, method = 'CSS-ML')
ip_ma # look at model 
plot(ip_ma$residuals) # is model sufficient for data? Looks good. 
acf(ip_ma$residuals, lag.max = 50) # check again to see if residuals resemble white noise. 
Box.test(ip_ma$residuals, type = "Ljung", lag = 16) # can also perform ljong box test
acf(ip_train) #notice that many lags before lag 16 are not significant. We can create a sparse model
# by removing non=significant lag terms. 0 - fixes constant. NA - still included and estimated.
ip_ma_sparse <- arima(ip_train-mean(ip_train), order = c(0, 0, 16), include.mean = FALSE, fixed = c(0, NA, rep(0, 9), rep(NA, 5)), method = 'CSS-ML')
# check residuals again.
library(lmtest)
coeftest(ip_ma) # can give you evidence for which variables may be good candidates. 

ip_forc = predict(ip_ma_sparse, n.ahead = 24, se = T)
ip_pred = ip_forc$pred
ip_low = ip_pred - 1.96*ip_forc$se
ip_up = ip_pred + 1.96*ip_forc$se

ts.plot(ip_vali, ip_pred, ip_low, ip_up, gpars=c())
ts.plot(IP_trans, ip_pred, ip_low, ip_up, gpars=c())
ts.plot(ts(IP_trans[804:851], start = 2007, freq = 12), ip_pred, ip_low, ip_up, gpars=c())


#EACF
plot(LakeHuron)
library(TSA)
eacf(LakeHuron_d1, ma.max = 10, ar.max = 10)
library(forecast)
LakeHuron_d1 <- diff(LakeHuron, differences = 1)
auto.arima(LakeHuron_d1, D=0, d=0, max.p=10, max.q=10, ic ='aic' ,stepwise = FALSE, trace = TRUE)

# ARMA order selection
library(forecast)
eacf(IP_trans)
auto.arima(IP_trans,d=0, D = 0, max.p = 5, max.q = 5, trace = TRUE)

### ARIMA examples
library(tseries)
data_rw <- rw.sim(n = 400)
adf.test(data_rw)

data_2 <- arima.sim(model = list(ar = 0.4), n = 400)
adf.test(data_2)

plot(AirPassengers)
plot(lnAP_1)

adf.test(lnAP_1)

# Financial Time Series

DAX <- EuStockMarkets[,2]
plot(DAX)
DAX_lr <- diff(log(DAX))
plot(DAX_lr)
acf(DAX_lr) # white noise. Nothing we can do in terms of ARMA modeling
acf(DAX^2) # there is non-linear dependence

acf(DAX_lr)
mean(DAX_lr)
Z = DAX_lr - mean(DAX_lr)
acf(Z^2)
pacf(Z^2)

library(fGarch)
DAX_arch3 <- garchFit(formula = ~garch(3,0), data = DAX_lr, trace = FALSE) # p = 3, q = 0
DAX_arch3
summary(DAX_arch3)
# R stands for standarized residual
# # if model fits well, R should follow a normal iid process
# pvalue for R^2 tells you how well GARCH model fits. specify cond.dist = 'sdt' ... 

DAX_arch3t = garchFit(formula~garch(3,0), data = DAX_lr, cond.dist = "std", trace = FALSE)
summary(DAX_arch3t)
plot(DAX_lr)

# estimating volatility is as or more important than making predictions.
DAX_v = volatility(DAX_arch3t, type = 'sigma')
DAX_v <- ts(DAX_v, start = 1992, frequency = 253) # approx. 253 transaction days per year (avg.)
plot(DAX_v)
DAX_pred <- predict(DAX_arch3, 10)
DAX_pred

DAX_pred_v = ts(c(DAX_v[1859], DAX_pred$standardDeviation), start = 1860)
ts.plot(DAX_v, DAX_pred_v)

(DAX_pred$standardDeviation)




sp500_ar6 <- arima(sp500, order = c(6,0,0), include.mean = TRUE, method = 'CSS-ML')
sp500_shock = sp500_ar6$residuals
acf(sp500_shock)
pacf(sp500_shock^2) # rule of thumb for choosing GARCH parameter. 
auto.arima(sp500_shohck, d = 0, D = 0, max.p = 3, max.q = 3, max.P = 0, max.Q = 0, stepwise = FALSE)
sp500_garch11 = garchFit(formula = ~garch(1,1), data = sp500_shock, trace = FALSE)
