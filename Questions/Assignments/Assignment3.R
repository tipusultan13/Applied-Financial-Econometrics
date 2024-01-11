#### GARCH Models ####

# Preamble ---------------------------------------------------------------------

#' (sets working directory, clears memory, resets) 
if (!("rstudioapi" %in% installed.packages())) {install.packages("rstudioapi")}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))# set the working directory to the current R Script location 
y = function(){dev.new();x=par(no.readonly=T); dev.off(); x} #'
par(y()); options(scipen=0); dev.off(); rm(list=ls()); cat("\014")

# Packages ---------------------------------------------------------------------

#update.packages()

library("readxl")
library("timeSeries")
library("tseries")
library("quantmod")
library("zoo")
library("moments")
library("forecast")
library("ggplot2") 
library("dplyr")
library("rugarch")
library("urca")

theme_set(theme_bw())  # set plotting theme

windowsFonts(C=windowsFont("Segoe UI"))
UTblue <- rgb(5/255, 110/255, 167/255, 1)

# 1. --------------------------------------------------------------------------- 
#' Load the multivariate dataset data3.xlsx. Then, select the closing price of 
#' the OMXS30 Index and plot it.

data <- read_excel("data3.xlsx")

OMXS30 <- as.xts(data$Close, order.by = data$Date)
OMXS30 <- with(OMXS30, zoo(data$Close, time(OMXS30)))
OMXS30 <- na.omit(OMXS30)

# png("OMXS30.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "white")
plot(OMXS30, main = "OMX Stockholm 30 Index", xlab="", ylab="", 
     lwd = 4, col = UTblue, las = 1)
grid(NA, NULL, lwd = 2, col = "black")
# dev.off()


# 2. --------------------------------------------------------------------------- 
#' Describe the continuous returns of the index with the help of a histogram, 
#' kurtosis, and skewness measures.

rOMXS30 <- 100*(log(OMXS30) - log(stats::lag(OMXS30,-1)))

# png("rOMXS30.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "white")
plot(rOMXS30, main = "OMX Stockholm 30 Log-Returns", xlab="", ylab="", 
     lwd = 4, col = UTblue, las = 1)
grid(NA, NULL, lwd = 2, col = "black")
# dev.off()

summary(rOMXS30)
kurtosis <- kurtosis(rOMXS30)
skewness(rOMXS30)

h <- hist(rOMXS30, breaks = 120, col = UTblue, # with frequency on the x-axis
          xlab = "Continuous returns", main ="OMXS30 Log-Returns")
xfit <- seq(min(rOMXS30), max(rOMXS30), length = 1000) 
yfit <- dnorm(xfit, mean = mean(rOMXS30), sd = sd(rOMXS30)) 
yfit <- yfit * diff(h$mids[1:2]) * length(rOMXS30) 
lines(xfit, yfit, col="red", lwd=2) 
abline(v = mean(rOMXS30), col="red")

# 3. --------------------------------------------------------------------------- 
#' Calculate the degrees of freedom appropriate for the Student's t-distributional
#' shape and plot it against the actual histogram.

df <- 6/(kurtosis - 3) + 4
df # 5.091355


n <- length(rOMXS30) # number of observations

pt <- data.frame(x = rnorm(n), returns = rOMXS30) %>% # density histogram
  ggplot(aes(returns, fill = "yellow")) +
  geom_histogram(aes(y = ..density..), bins = 100,
                 fill = "yellow", col = "black") +
  stat_function(fun = dt, args = list(df = df), aes(x = x, col = "red"), size = 1.5) +
  ylab("Density") + xlab("") +
  theme(legend.position = "none")
pt

# 4. --------------------------------------------------------------------------- 
#' Compute the 30-day historical volatility.

#' Defined as: the annualized standard deviation of the relative daily log price 
#' changes for the 30 most recent trading days closing price, 
#' expressed as a percentage.

rOMXS30_30d <- rOMXS30[(n-29):n]
N <- length((n-29):n) # 30 days

(252 * sum((rOMXS30_30d - mean(rOMXS30_30d))^2)/(N-1)) %>%
  sqrt() # 22.95083% volatility

#' or

(252 * var(rOMXS30_30d)) %>%
  sqrt() # 22.95083% volatility

# 5. --------------------------------------------------------------------------- 
#' Start your analysis with an ARIMA(p,d,q) model. Comment on the results.

summary(ur.df(rOMXS30, selectlags = "BIC", type ="drift")) 
summary(ur.df(rOMXS30, selectlags = "BIC", type ="none")) # stationary at the 5%

# Check the (P)ACF
Acf(rOMXS30, lag.max = 30, ylab = "", 
    main = "", col = UTblue, lwd = 4, las = 1, na.action = na.pass) # pronounced 4th lag


Pacf(rOMXS30, lag.max = 30, ylab = "", 
     main = "", col = UTblue, lwd = 4, las = 1, na.action = na.pass) # pronounced 4th lag

# Estimate ARIMA(p, d, q)

ic_arma <- data.frame("AR" = NA, "MA" = NA, "AIC" = NA, "BIC" = NA)

r <- 1

for(q in c(0:4)){
  for(p in c(0:4)){
    ic_arma[p+r,"AR"] <- p
    ic_arma[p+r,"MA"] <- q
    ic_arma[p+r,"AIC"] <- Arima(rOMXS30, order = c(p,0,q))["aic"][[1]]
    ic_arma[p+r,"BIC"] <- Arima(rOMXS30, order = c(p,0,q))["bic"][[1]]
  }
  r <- r + 5
}

# Obtain model with the minimum IC
ic_arma[which(ic_arma["AIC"]==min(ic_arma["AIC"])),] # ARMA(3,3)
ic_arma[which(ic_arma["BIC"]==min(ic_arma["BIC"])),] # ARMA(1,1)

arma1_1 <- Arima(rOMXS30, order = c(1,0,1)) # ARIMA(1,0,1) with non-zero mean 
arma1_1

arma3_3 <- Arima(rOMXS30, order = c(3,0,3)) # ARIMA(3,0,3) with non-zero mean 
arma3_3

# Test for Autocorrelation in (Squared) Residuals
Box.test(arma1_1$residuals, lag = 10, type = "Ljung-Box") # autocorrelation
Box.test(I(arma1_1$residuals)^2, lag = 10, type = "Ljung-Box") # autocorrelation


Box.test(arma3_3$residuals, lag = 10, type = "Ljung-Box") # no autocorrelation
Box.test(I(arma3_3$residuals)^2, lag = 10, type = "Ljung-Box") # autocorrelation

# 6. --------------------------------------------------------------------------- 
#' Find an adequate ARMA-GARCH specification for the log-returns. Consider normal
#' as well as t-distributed residuals.

# ARMA(3,3) - GARCH(1,1) Model
specGARCH <- ugarchspec(
  mean.model         = list(armaOrder = c(3,3)),
  variance.model     = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "norm")

GARCH <- ugarchfit(specGARCH, rOMXS30)
GARCH  # BIC: 3.0954

## Sum of GARCH Coefficients
sum(GARCH@fit$matcoef[c("alpha1", "beta1"), 1]) # 0.9855, highly persistent cond. var.

residGARCH <- resid(GARCH@fit)
condvarGARCH <- GARCH@fit$var
residstandGARCH <- residGARCH/I(condvarGARCH^.5)
kurtosis(residstandGARCH) #  4.2873 a bit of an excess kurtosis

# tseries::jarque.bera.test(residGARCH/I(condvarGARCH^.5)) # still not normal

## Test for Autocorrelation
Box.test(residstandGARCH, lag = 10, type = "Ljung-Box") # no autocorrelation

## Test for Conditional Heteroskedasticity
Box.test(residstandGARCH^2, lag = 10, type = "Ljung-Box") # not optimal


# ARMA(3,3) - GARCH(1,1) Model (std)
specGARCH <- ugarchspec(
  mean.model         = list(armaOrder = c(3,3)),
  variance.model     = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "std")

GARCH <- ugarchfit(specGARCH, rOMXS30)
GARCH  # BIC: 3.0679

## Sum of GARCH Coefficients
sum(GARCH@fit$matcoef[c("alpha1", "beta1"), 1]) # 0.989, highly persistent cond. var.

residGARCH <- resid(GARCH@fit)
condvarGARCH <- GARCH@fit$var
residstandGARCH <- residGARCH/I(condvarGARCH^.5)
kurtosis(residstandGARCH) #  4.305956 a bit of an excess kurtosis
# tseries::jarque.bera.test(residGARCH/I(condvarGARCH^.5)) # still not normal

## Test for Autocorrelation
Box.test(residstandGARCH, lag = 10, type = "Ljung-Box") # no autocorrelation

## Test for Conditional Heteroskedasticity
Box.test(residstandGARCH^2, lag = 10, type = "Ljung-Box") # also not optimal


# ARMA(0,0) - GARCH(1,1) Model
specGARCH <- ugarchspec(
  mean.model         = list(armaOrder = c(0,0)),
  variance.model     = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "norm")

GARCH <- ugarchfit(specGARCH, rOMXS30)
GARCH  # BIC: 3.0954

## Sum of GARCH Coefficients
sum(GARCH@fit$matcoef[c("alpha1", "beta1"), 1]) # 0.9856, highly persistent cond. var.

residGARCH <- resid(GARCH@fit)
condvarGARCH <- GARCH@fit$var
residstandGARCH <- residGARCH/I(condvarGARCH^.5)
kurtosis(residstandGARCH) #  4.290544 a bit of an excess kurtosis

## Test for Autocorrelation
Box.test(residstandGARCH, lag = 10, type = "Ljung-Box") # no autocorrelation

## Test for Conditional Heteroskedasticity
Box.test(residstandGARCH^2, lag = 10, type = "Ljung-Box") # no need to include
# higher order (G)ARCH terms anymore: improvement of the modeling

# ARMA(0,0) - GARCH(1,1) Model (std)
specGARCH <- ugarchspec(
  mean.model         = list(armaOrder = c(0,0)),
  variance.model     = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "std")

GARCH <- ugarchfit(specGARCH, rOMXS30)
GARCH  # BIC: 3.0632

## Sum of GARCH Coefficients
sum(GARCH@fit$matcoef[c("alpha1", "beta1"), 1]) # 0.989059, highly persistent cond. var.

residGARCH <- resid(GARCH@fit)
condvarGARCH <- GARCH@fit$var
residstandGARCH <- residGARCH/I(condvarGARCH^.5)
kurtosis(residstandGARCH) #  1.30532 a bit of an excess kurtosis

## Test for Autocorrelation
Box.test(residstandGARCH, lag = 10, type = "Ljung-Box") # no autocorrelation

## Test for Conditional Heteroskedasticity
Box.test(residstandGARCH^2, lag = 10, type = "Ljung-Box") # no need to include
# higher order (G)ARCH terms anymore: improvement of the modeling

# stick with the last model

# 7. --------------------------------------------------------------------------- 
#' Use a GJR-GARCH(p,q) model to check whether the leverage effect is present.

## GJR-GARCH(1,1) Model
specGJRGARCH11 <- ugarchspec(
  mean.model         = list(armaOrder = c(0,0)),
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1,1)),
  distribution.model = "std")

GJRGARCH11 <- ugarchfit(specGJRGARCH11, rOMXS30)
GJRGARCH11

# Leverage Effect
GJRGARCH11@fit$matcoef[3,1]+GJRGARCH11@fit$matcoef[5,1] # 0.1631661

#' Leverage effect is present as gamma1 > 0

# 8. --------------------------------------------------------------------------- 
#' Compare your results to an exponential GARCH(p,q) model. What can be inferred?

## EGARCH(1,1) Model
specEGARCH11 <- ugarchspec(
  mean.model         = list(armaOrder = c(0,0)),
  variance.model     = list(model = "eGARCH", garchOrder = c(1,1)),
  distribution.model = "std")

EGARCH11 <- ugarchfit(specEGARCH11, rOMXS30)
EGARCH11

# Leverage Effect
EGARCH11@fit$matcoef[3,1]+EGARCH11@fit$matcoef[5,1] # -0.01226704
-EGARCH11@fit$matcoef[3,1]+EGARCH11@fit$matcoef[5,1] # 0.2557233
#' ARCH effect for negative residuals is larger

#' alpha1 <= -0.13 -> leverage effect is captured

# 9. --------------------------------------------------------------------------- 
#' Transform the variable into monthly frequency.

OMXS30m <- to.monthly(OMXS30, endof = T)$OMXS30.Close

# rOMXS30m <- aggregate(rOMXS30, as.yearmon, mean) # <- would be a poor aggregation
# summary(rOMXS30m)

rOMXS30m <- 100*(log(OMXS30m) - log(stats::lag(OMXS30m,-1)))

summary(ur.df(rOMXS30m, selectlags = "BIC", type ="drift")) # stationary with a non-zero mean

# png("rOMXS30m_temp_agg.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "white")
plot(rOMXS30m, main = "OMX Stockholm 30 Monthly Log-Returns", xlab="", ylab="", 
     lwd = 4, col = UTblue, las = 1)
grid(NA, NULL, lwd = 2, col = "black")
# dev.off()

# 10. --------------------------------------------------------------------------
#' Estimate an ARIMA(p,d,q) model using the monthly time series. Test for 
#' heteroskedasticity in the squared residuals. What changes in the case of a 
#' quarterly time series?

# Check the (P)ACF
Acf(rOMXS30m, lag.max = 20, ylab = "", 
    main = "", col = UTblue, lwd = 4, las = 1, na.action = na.pass) 


Pacf(rOMXS30m, lag.max = 20, ylab = "", 
     main = "", col = UTblue, lwd = 4, las = 1, na.action = na.pass) 

# Estimate ARIMA(p, d, q)

ic_arma <- data.frame("AR" = NA, "MA" = NA, "AIC" = NA, "BIC" = NA)

r <- 1

for(q in c(0:10)){
  for(p in c(0:10)){
    ic_arma[p+r,"AR"] <- p
    ic_arma[p+r,"MA"] <- q
    ic_arma[p+r,"AIC"] <- Arima(rOMXS30m, order = c(p,0,q))["aic"][[1]]
    ic_arma[p+r,"BIC"] <- Arima(rOMXS30m, order = c(p,0,q))["bic"][[1]]
  }
  r <- r + 11
}

# Obtain model with the minimum IC
ic_arma[which(ic_arma["AIC"]==min(ic_arma["AIC"])),] # ARMA(6,6)
ic_arma[which(ic_arma["BIC"]==min(ic_arma["BIC"])),] # ARMA(0,0)

arma6_6 <- Arima(rOMXS30m, order = c(6,0,6)) # ARIMA(6,0,6) with non-zero mean 
arma6_6

plot(arma6_6$x, col = "red", lwd = 4, las = 1) # original variable
lines(fitted(arma6_6), col = UTblue, lwd = 4, las = 1) # fitted variable

arma0_0 <- Arima(rOMXS30m, order = c(0,0,0)) # ARIMA(0,0,0) with non-zero mean 
arma0_0

summary(rOMXS30m) # compare

plot(arma0_0$x, col = "red", lwd = 4, las = 1) # original variable
lines(fitted(arma0_0), col = UTblue, lwd = 4, las = 1) # fitted variable

# Test for Autocorrelation in (Squared) Residuals
Box.test(arma6_6$residuals, lag = 10, type = "Ljung-Box") # no autocorrelation
Box.test(I(arma6_6$residuals)^2, lag = 10, type = "Ljung-Box") # autocorrelation

Box.test(arma0_0$residuals, lag = 10, type = "Ljung-Box") # no autocorrelation
Box.test(I(arma0_0$residuals)^2, lag = 10, type = "Ljung-Box") # autocorrelation

## Repeat with quarterly TS

# rOMXS30q <- aggregate(rOMXS30, as.yearqtr, mean) # <- would be poor

OMXS30q <- to.quarterly(OMXS30, endof = T)$OMXS30.Close

# rOMXS30m <- aggregate(rOMXS30, as.yearmon, mean) # <- would be a poor aggregation
# summary(rOMXS30m)

rOMXS30q <- 100*(log(OMXS30q) - log(stats::lag(OMXS30q,-1)))

summary(ur.df(rOMXS30q, selectlags = "BIC", type ="drift")) 
summary(ur.df(rOMXS30q, selectlags = "BIC", type ="none")) # stationary 

# png("rOMXS30q_temp_agg.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "white")
plot(rOMXS30q, main = "OMX Stockholm 30 Quarterly Log-Returns", xlab="", ylab="", 
     lwd = 4, col = UTblue, las = 1)
grid(NA, NULL, lwd = 2, col = "black")
# dev.off()

Acf(rOMXS30q, lag.max = 20, ylab = "", 
    main = "", col = UTblue, lwd = 4, las = 1, na.action = na.pass) 


Pacf(rOMXS30q, lag.max = 20, ylab = "", 
     main = "", col = UTblue, lwd = 4, las = 1, na.action = na.pass) 

# Estimate ARIMA(p, d, q)

ic_arma <- data.frame("AR" = NA, "MA" = NA, "AIC" = NA, "BIC" = NA)

r <- 1

for(q in c(0:4)){
  for(p in c(0:4)){
    ic_arma[p+r,"AR"] <- p
    ic_arma[p+r,"MA"] <- q
    ic_arma[p+r,"AIC"] <- Arima(rOMXS30q, method = "ML", order = c(p,0,q))["aic"][[1]]
    ic_arma[p+r,"BIC"] <- Arima(rOMXS30q, method = "ML", order = c(p,0,q))["bic"][[1]]
  }
  r <- r + 5
}

# Obtain model with the minimum IC
ic_arma[which(ic_arma["AIC"]==min(ic_arma["AIC"])),] # ARMA(4,4)
ic_arma[which(ic_arma["BIC"]==min(ic_arma["BIC"])),] # ARMA(0,0)

arma4_4 <- Arima(rOMXS30q, method = "ML", order = c(4,0,4)) # ARIMA(4,0,4) with non-zero mean 
arma4_4

plot(arma4_4$x, col = "red", lwd = 4, las = 1) # original variable
lines(fitted(arma4_4), col = UTblue, lwd = 4, las = 1) # fitted variable

arma0_0 <- Arima(rOMXS30q, method = "ML", order = c(0,0,0)) # ARIMA(0,0,0) with non-zero mean 
arma0_0

plot(arma0_0$x, col = "red", lwd = 4, las = 1) # original variable
lines(fitted(arma0_0), col = UTblue, lwd = 4, las = 1) # fitted variable

# Test for Autocorrelation in (Squared) Residuals
Box.test(arma4_4$residuals, lag = 10, type = "Ljung-Box") # no autocorrelation
Box.test(I(arma4_4$residuals)^2, lag = 10, type = "Ljung-Box") # no autocorrelation

Box.test(arma0_0$residuals, lag = 10, type = "Ljung-Box") # no autocorrelation
Box.test(I(arma0_0$residuals)^2, lag = 10, type = "Ljung-Box") # autocorrelation

#' Here, no ARCH effects are left. Typically, the end-of-period temporal 
#' aggregation helps to get rid of the ARCH effects.














