setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
y=function(){dev.new();x=par(no.readonly=T);dev.off();x} #'
par(y());options(scipen=0);dev.off();rm(list=ls()); cat("\014")


#### Ans To The Question Number 1.1


library(readxl)
library(zoo)
library(xts)

## Load the data
data <- read_excel("data3.xlsx")

## Select closing price
closing_price <- data[,c("Date","Close")]
closing_price <- data.frame(closing_price)
closing_price <- xts(closing_price[,-1], order.by=as.Date(closing_price[,1], "%Y/%m/%d"))
closing_price <- na.omit(closing_price)

## Plot
plot(closing_price, type = "l")


#### Ans To The Question Number 1.2


library(quantmod)
## continuous return
r_closing_price <- (diff(closing_price, arithmetic=FALSE) - 1)*100
r_closing_price <- na.omit(r_closing_price)
colnames(r_closing_price)[1] <- "Close"
View(r_closing_price)
ts.plot(r_closing_price)

library(moments)
hist(r_closing_price)
# Almost every mass is in the middle. 
kurtosis(r_closing_price)
# kurtosis is 8.510188. This value indicates that data set has heavy tail or outliers.
skewness(r_closing_price)
# skewness is -0.04379812. As the value is negative and very close to 0, it means the distribution
# has a left tail and not very long. 


#### Ans To The Question Number 1.3


## Degrees of freedom

df <- round(6/(kurtosis(r_closing_price)-3) + 4) # kurtosis = 3 + (6/(df-4))

## Plot student t-distribution against histogram

x <- seq(min(r_closing_price), max(r_closing_price), length=10000)
t_d <- dt(x,df=df)
hist(r_closing_price,probability = TRUE, ylim = c(0,max(t_d)), main= "Histogram vs Student t Distribution")
lines(x,t_d, lwd=2, col= "red")


#### Ans To The Question Number 1.4


library(forecast)
library(rugarch)
g1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
               mean.model=list(armaOrder=c(1,1)),distribution.model="std")

garch11<-ugarchfit(g1,data = r_closing_price)
volatility <- ts(garch11@fit$sigma^2,end=c(2022,6),frequency = 12)
print(volatility)
ts.plot(volatility)


#### Ans To The Question Number 1.5


r_closing_price_arima <- auto.arima(r_closing_price)
summary(r_closing_price_arima)

## Test for Autocorrelation in (Squared) Residuals

Box.test(r_closing_price_arima$residuals, lag = 10, type = "Ljung-Box")
# The p value is higher than 0.05. S0, There are no autocorrelation among normal residuals at 5% significance level.
# In that case the residuals are white noise process.

Box.test(I(r_closing_price_arima$residuals)^2, lag = 10, type = "Ljung-Box")
# The p value is less than 0.05. S0, There are autocorrelation among the squared residuals 5% significance level.
# In that case the residuals are not white noise process.


#### Ans To The Question Number 1.6


## ARMA(0,0)-GARCH(1,1) Model

## Normal distribution
specGARCH11_norm <- ugarchspec(
  mean.model         = list(armaOrder = c(0,0)),
  variance.model     = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "norm")

GARCH11_norm <- ugarchfit(specGARCH11_norm, r_closing_price)
GARCH11_norm

## Sum of GARCH Coefficients
sum(GARCH11_norm@fit$matcoef[3:4,1])
# we observe a high degree of persistence in the conditional variance. 

## Residual Analysis
residGARCH11_norm <- resid(GARCH11_norm@fit)
condvarGARCH11_norm <- c(GARCH11_norm@fit$var)

## Standardize Residuals by Conditional Std. Dev.
residstandGARCH11_norm <- residGARCH11_norm/I(condvarGARCH11_norm^.5)

## Student t-distribution
specGARCH11_std <- ugarchspec(
  mean.model         = list(armaOrder = c(0,0)),
  variance.model     = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "std")

GARCH11_std <- ugarchfit(specGARCH11_std, r_closing_price)
GARCH11_std 

## Sum of GARCH Coefficients
sum(GARCH11_std@fit$matcoef[3:4,1])
# we observe a high degree of persistence in the conditional variance.

## Residual Analysis
residGARCH11_std <- resid(GARCH11_std@fit)
condvarGARCH11_std <- c(GARCH11_std@fit$var)

## Standardize Residuals by Conditional Std. Dev.
residstandGARCH11_std <- residGARCH11_std/I(condvarGARCH11_std^.5)


#### Ans To The Question Number 1.7


## GJR-GARCH(1,1) Model
specGJRGARCH11 <- ugarchspec(
  mean.model         = list(armaOrder = c(0,0)),
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1,1)),
  distribution.model = "norm")

GJRGARCH11 <- ugarchfit(specGJRGARCH11, r_closing_price)
GJRGARCH11

## Leverage Effect

GJRGARCH11@fit$matcoef[3,1]+GJRGARCH11@fit$matcoef[5,1]

# Gamma is Larger than 0. Means we have leverage effect present in the model.
# Constant term is 0.023939.
# alpha is positive


#### Ans To The Question Number 1.8


## EGARCH(1,1) Model
specEGARCH11 <- ugarchspec(
  mean.model         = list(armaOrder = c(0,0)),
  variance.model     = list(model = "eGARCH", garchOrder = c(1,1)),
  distribution.model = "norm")

EGARCH11 <- ugarchfit(specEGARCH11, r_closing_price)
EGARCH11 

## Leverage Effect

EGARCH11@fit$matcoef[3,1]+EGARCH11@fit$matcoef[5,1] # Negative shock
-EGARCH11@fit$matcoef[3,1]+EGARCH11@fit$matcoef[5,1] # Positive shock

# Constant term 0.012605
# Gamma is positive
# For the EGARCH model, if the alpha is negative there is a leverage effect present.
# As alpha is -0.128029, we expect the existence of a leverage effect in the model.
# The leverage effect is significant in both specifications(GIJ-GARCH and EGARCH).
# This leverage effect leads to higher volatility as a result of negative shocks as compared to positive ones


#### Ans To The Question Number 1.9


data_new <- data                                  # Duplicate data
data_new$year <- strftime(data_new$Date, "%Y")    # Create year column
data_new$month <- strftime(data_new$Date, "%m")   # Create month column

closing_price_mon <- aggregate(Close ~ month + year,       # Aggregate data
                        data_new,
                        FUN = sum)
View(closing_price_mon)


#### Ans To The Question Number 1.10


## Monthly time series 
closing_price_mon_ts <- ts(closing_price_mon$Close, start=2003, frequency = 12)

## Estimating Arima(p,d,q) model

closing_price_mon_arima <- auto.arima(closing_price_mon_ts)
summary(closing_price_mon_arima) # Arima(0,0,1)

## ARMA(0,1)-GARCH(1,1) Model
specGARCH11_mon <- ugarchspec(
  mean.model         = list(armaOrder = c(0,1)),
  variance.model     = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "norm")

GARCH11_mon <- ugarchfit(specGARCH11_mon, closing_price_mon_ts)
GARCH11_mon
# P-values of Standardized Residuals and Standardized Squared Residuals are less than 0.05. 
# Means we reject H0 at 5% significance level. The are serial correlation and they are not white noise process.

## Standardize Residuals by Conditional Std. Dev.

residGARCH11_mon <- resid(GARCH11_mon@fit)
condvarGARCH11_mon <- c(GARCH11_mon@fit$var)
residstandGARCH11_mon <- residGARCH11_mon/I(condvarGARCH11_mon^.5)

## Test for heteroskedasticity in the squared residuals

Box.test(I(residstandGARCH11_mon^2), lag = 10, type = "Ljung-Box")
# p-values of Standardized squared Residuals are less than 0.05.
# So, we reject the null hypothesis at 5% significance level. 
# There are serial correlation among the Standardized squared Residuals.
# So, Squared residuals are not white noise process.
# The Standardized squared Residuals are heteroskedastic.

## Quarterly time series

closing_price_quar_ts <- ts(closing_price_mon$Close, start=2003, frequency = 4)

## Estimating Arima(p,d,q) model

closing_price_quar_arima <- auto.arima(closing_price_quar_ts)
summary(closing_price_quar_arima) # Arima(1,1,0) is the best fit

## ARMA(1,0)-GARCH(1,1) Model
specGARCH11_quar <- ugarchspec(
  mean.model         = list(armaOrder = c(1,0)),
  variance.model     = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "norm")

GARCH11_quar <- ugarchfit(specGARCH11_quar, closing_price_quar_ts)
GARCH11_quar
# p-values of Standardized Residuals are less than 0.05.
# so, we reject the null hypothesis at 5% significance level.
# There are serial autocorrelation among the Standardized Residuals.
# But the p-values of Standardized squared Residuals are greater than 0.05.
# So, so, we cannot reject the null hypothesis at 5% significance level. 
# There are no serial autocorrelation among the Standardized squared Residuals.
# So, Squared residuals are white noise process.

## Standardize Residuals by Conditional Std. Dev.

residGARCH11_quar <- resid(GARCH11_quar@fit)
condvarGARCH11_quar <- c(GARCH11_quar@fit$var)
residstandGARCH11_quar <- residGARCH11_quar/I(condvarGARCH11_quar^.5)

## Test for heteroskedasticity in the squared residuals

Box.test(I(residstandGARCH11_quar^2), lag = 10, type = "Ljung-Box")
# p-values of Standardized squared Residuals are greater than 0.05.
# So, we cannot reject the null hypothesis at 5% significance level. 
# There are no serial autocorrelation among the Standardized squared Residuals.
# So, Squared residuals are white noise process.
# The Standardized squared Residuals are not heteroskedastic.


#### Ans To The Question Number 2

## Manually Lagrange-Multiplier test

## Lagrange-Multiplier test pre-existing code
library(nortsTest)
lm_test <- Lm.test(as.vector(residstandGARCH11_quar^2),lag.max = 10,alpha = 0.05)
lm_test 
# p-value < 0.05. So, we reject the null hypothesis. 
# In this case squared residuals are heteroscedastic. 






