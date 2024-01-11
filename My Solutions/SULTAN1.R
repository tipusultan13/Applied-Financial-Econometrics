### Ans to the question number: 1.1


library(readxl)
data<-read_excel("data1.xlsx")
# Time series corresponding to Euro Area
Ts_Euro_Area <- ts(data$EA, start = c(1990,1), end = c(2022,3), frequency = 12)
# Time series corresponding to Denmark
Ts_Denmark <- ts(data$Denmark, start = c(1990,1), end = c(2022,3), frequency = 12)
# Time series corresponding to Sweden
Ts_Sweden <- ts(data$Sweden, start = c(1990,1), end = c(2022,3), frequency = 12)


### Ans to the question number: 1.2


png("Time_Series_Plot.png")
ts.plot(Ts_Euro_Area, ylab = "%in",ylim = c(-5,30), col = "red")
lines(Ts_Denmark, col = "blue")
lines(Ts_Sweden, col = "green")
legend("topright", c("EA=red","Denmark=blue,Sweden=green"))
grid(NA, NULL, lwd = 2, col = "black")
dev.off()


### Ans to the question number: 1.3


##Summary
summary(Ts_Euro_Area)
summary(Ts_Denmark)
summary(Ts_Sweden)

## Variable normality test
# Jarque Bera test
library(tseries)
jarque.bera.test(Ts_Euro_Area)
# 5% level of significance we reject the null hypothesis
# The data is significally different from normal.
jarque.bera.test(Ts_Denmark)
# 5% level of significance we reject the null hypothesis
# The data is significally different from normal.
jarque.bera.test(Ts_Sweden)
# 5% level of significance we reject the null hypothesis
# The data is significally different from normal.

## Histograms
png("Histograms.png")
par(mfrow = c(2, 2))
hist(Ts_Euro_Area)
hist(Ts_Denmark)
hist(Ts_Sweden)
dev.off()

## Skewness
library(moments)
skewness(Ts_Euro_Area) # Right or positive skewed
skewness(Ts_Denmark) # Right or positive skewed
skewness(Ts_Sweden) # Right or positiveskewed


### Ans to the question number: 1.4


cor(data$EA,data$Denmark)
# The variables Euro Area and Denmark have a positive correlation of 0.973859
cor(data$Sweden,data$Denmark)
# The variables Sweden and Denmark have a positive correlation of 0.9390906
cor(data$EA,data$Sweden)
# The variables Sweden and Denmark have a positive correlation of 0.9731517

# My conclusion is that Euro Area and Denmark are the closest to each other.


### Ans to the question number: 1.5


# Time series starts from 2013
Ts_Euro_Area_2013 <- ts(data$EA, start = 2013, end = c(2022,3), frequency = 12)
Ts_Denmark_2013 <- ts(data$Denmark, start = 2013, end = c(2022,3), frequency = 12)
Ts_Sweden_2013 <- ts(data$Sweden, start = 2013, end = c(2022,3), frequency = 12)

## Comparing to the question number 1.3
##Summary
summary(Ts_Euro_Area_2013) #Mean increased
summary(Ts_Denmark_2013) #Mean increased
summary(Ts_Sweden_2013) #Mean increased

## Variable normality test
# Jarque Bera test
library(tseries)
jarque.bera.test(Ts_Euro_Area_2013)
# 5% level of significance we reject the null hypothesis
# The data is significally different from normal.
jarque.bera.test(Ts_Denmark_2013)
# 5% level of significance we reject the null hypothesis
# The data is significally different from normal.
jarque.bera.test(Ts_Sweden_2013)
# 5% level of significance we reject the null hypothesis
# The data is significally different from normal.

# Data significantly did not change from not normal

## Histograms
png("Histograms_2013.png")
par(mfrow = c(2, 2))
hist(Ts_Euro_Area_2013)
hist(Ts_Denmark_2013)
hist(Ts_Sweden_2013)
dev.off()
# Shape of the histogram changeed

## Skewness
library(moments)
skewness(Ts_Euro_Area_2013) # Skewness changed from 0.89 to .10
skewness(Ts_Denmark_2013) # Skewness changed from 1.20 to .65
skewness(Ts_Sweden_2013) # # Skewness changed from 1.30 to .57

## Comparing to the question number 1.4
data_2013<-data[277:387,]
cor(data_2013$EA,data_2013$Denmark)
# The variables Euro Area and Denmark have a positive correlation of 0.864288
cor(data_2013$Sweden,data_2013$Denmark)
# The variables Sweden and Denmark have a positive correlation of 0.7760128
cor(data_2013$EA,data_2013$Sweden)
# The variables Sweden and Denmark have a positive correlation of 0.7023434

# The close relation between EA and Denmark remain same.
# But the strength of the correlation has been changed


###Ans to the question number: 1.6


## Convert into quarterly time series
Ts_Euro_Area_Quarterly <- aggregate(Ts_Euro_Area, nfrequency = 4)
Ts_Denmark_Quarterly <- aggregate(Ts_Denmark, nfrequency = 4)
Ts_Sweden_Quarterly <- aggregate(Ts_Sweden, nfrequency = 4)

## Average annueal change

Average_Annual_Change_Euro_Area<-mean(diff(Ts_Euro_Area_Quarterly))
print(Average_Annual_Change_Euro_Area)

Average_Annual_Change_Denmark<-mean(diff(Ts_Denmark_Quarterly))
print(Average_Annual_Change_Denmark)

Average_Annual_Change_Sweden<-mean(diff(Ts_Sweden_Quarterly))
print(Average_Annual_Change_Sweden)


###Ans to the question number: 1.7


# Unit root test
library(urca)
summary(ur.df(Ts_Sweden, selectlags = "BIC", type = "trend"))
# p value is 0.0001128. so 5% level of significance we fail to accept the null hypothesis. Which tells us the time series is not stationary.
# The t value of lag.1 is -2.924 which is greater than the value of tau3(-3.42). So, we have unit roots. 
# The series is not stationary

## Seasonality trend

library(forecast)
# Existance
Ts_Sweden_decompose<-decompose(Ts_Sweden) # Ts_Sweden object was a monthly time series
plot(Ts_Sweden_decompose) # Presence of seasonality and trend.

# Remove
Ts_Sweden_Deseason<- seasadj(Ts_Sweden_decompose)
plot(Ts_Sweden_Deseason) # Seasonality removed

Ts_Sweden_Deseason_Diff <- diff(Ts_Sweden_Deseason)
plot.ts(Ts_Sweden_Deseason_Diff) # Trend removed

## Stochastic Trend

# Existance
plot.ts(Ts_Sweden_Deseason_Diff) # Stochastic trend does not exist in the graph after taking the first difference.

## Detarministic Trend                          

#Remove
library(lmtest)
Ts_Sweden_Trend<-lm(Ts_Sweden_Deseason_Diff~c(1:length(Ts_Sweden_Deseason_Diff)))
Ts_Sweden_Detrend<-residuals(Ts_Sweden_Trend)
Ts_Sweden_Detrend<-ts(Ts_Sweden_Detrend, start = c(1990,1), end = c(2022,3), frequency= 12)
plot.ts(Ts_Sweden_Detrend) # Deterministic trend removed

## Root test
library(tseries)
adf.test(Ts_Sweden_Detrend, alternative = "stationary", k=0)
# At 5% significance level we reject null hypothesis
# The series is stationary at 5% significance level.


### Ans to the question number 1.8


# Using loop
# PACF test to determine lags
Pacf(Ts_Sweden_Detrend, lag.max = 20, ylab = "", ylim = c(-.4,.8),
     main = "", lwd = 4, las = 1)
# We can see from the graph, 8 lag can be taken.

## Choose Model that Minimizes IC
# Create object to store AIC/BIC for different lag lengths
ic_arma <- data.frame("AR" = NA, "MA" = NA, "AIC" = NA, "BIC" = NA)

r <- 1

for(q in c(0:8)){
  for(p in c(0:8)){
    ic_arma[p,"AR"] <- p
    ic_arma[p,"MA"] <- q
    ic_arma[p,"AIC"] <- Arima(Ts_Sweden_Detrend, order = c(p,0,q))["aic"][[1]]
    ic_arma[p,"BIC"] <- Arima(Ts_Sweden_Detrend, order = c(p,0,q))["bic"][[1]]
  }
  r <- r + 9
}

## Obtain model with the minimum AIC/BIC
ic_arma[which(ic_arma["AIC"]==min(ic_arma["AIC"])),]
ic_arma[which(ic_arma["BIC"]==min(ic_arma["BIC"])),]

## Best overall BIC: ARMA(3,8)
## Best overall AIC: ARMA(7,8)
Arma_AIC <- Arima(Ts_Sweden_Detrend, order = c(7,0,8))
Arma_BIC<- Arima(Ts_Sweden_Detrend, order = c(3,0,8))

## Using package
## Estimating best ARIMA(p,d,q) model
Sweden_ARIMA <- auto.arima(Ts_Sweden_Detrend)


## Interpretation
summary(Sweden_ARIMA) #ARIMA(0,0,3)(2,0,2)[12] with zero mean
summary(Arma_AIC)
summary(Arma_BIC)
# arma_aic has the lowest MS and MSE. In that case Arma_AIC is more efficient



### Ans to the question number 1.9


## Test for autocorrelation in residuals
Box.test(resid(arma_aic), lag = 16, type ="Ljung-Box")
# Null Hypothesis: There is no serial autocorrelation upto 8 lags
# p-value > 0.05. We don't have enough statistical evidence to reject the null hypothesis.
Box.test(resid(arma1_bic), lag = 16, type ="Ljung-Box")
# Null Hypothesis: There is no serial autocorrelation upto 8 lags
# p-value > 0.05. We don't have enough statistical evidence to reject the null hypothesis.
Box.test(resid(Sweden_ARIMA), lag = 16, type ="Ljung-Box")
# Null Hypothesis: There is no serial autocorrelation upto 8 lags
# p-value > 0.05. We don't have enough statistical evidence to reject the null hypothesis.


### Ans to the question number 1.10


## Create and Transform the model
Ts_Sweden_2021 <- ts(data$Sweden, start = c(1990,1), end = c(2021,2), frequency = 12)
Ts_Sweden_decompose_2021<-decompose(Ts_Sweden_2021)
Ts_Sweden_Deseason_2021<- seasadj(Ts_Sweden_decompose)
Ts_Sweden_Deseason_Diff_2021 <- diff(Ts_Sweden_Deseason_2021)

Ts_Sweden_Trend_2021<-lm(Ts_Sweden_Deseason_Diff_2021~c(1:length(Ts_Sweden_Deseason_Diff_2021)))
Ts_Sweden_Detrend_2021<-residuals(Ts_Sweden_Trend_2021)
Ts_Sweden_Detrend_2021<-ts(Ts_Sweden_Detrend_2021, start = c(1990,1), end = c(2021,2), frequency= 12)
Sweden_ARIMA_2021 <- auto.arima(Ts_Sweden_Detrend_2021)

## Forecast
forecast_Sweden_ARIMA_2021 <- forecast(Sweden_ARIMA_2021, h = 12)
forecast_naive  <- naive(Ts_Sweden_Detrend_2021, h = 12)

## Graphically compare
library(ggplot2)
ggplot2::autoplot(Sweden_ARIMA_2021)
# All the roots are inside the circles. so, our model is causal
ggplot2::autoplot(forecast_Sweden_ARIMA_2021)
# The graph is well accurate
checkresiduals(Sweden_ARIMA_2021)
# Residuals are stationary with mean zero and constant variance
# 3 significant autocorrelations with the residuals where dotted blue lines are confidence bound
# Residuals has a curve of normal distribution but are almost not normal at the 3rd graph

## Compare formally
accuracy(forecast_Sweden_ARIMA_2021, Ts_Sweden_Detrend)
accuracy(forecast_naive, Ts_Sweden_Detrend)
# RMSE of ARIMA model is less than the RMSE of naive model.
# ME of ARIMA model is less than the ME of naive model. 
# So, in this scenario our naive model is less efficient than the ARIMA model. 


### Ans to the question number 2.a


# Create a time series.

set.seed(123)
 t <- seq(from = 1, to = 100, by = 1) + 10 + rnorm(100, sd = 7)
tseries <- ts(t, start = c(2000, 1), frequency = 4)
plot.ts(tseries)

