#### Modeling the 3-Month Money Market Interest Rates ####

# Preamble ---------------------------------------------------------------------

#' (sets working directory, clears memory, resets) 
if (!("rstudioapi" %in% installed.packages())) {install.packages("rstudioapi")}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))# set the working directory to the current R Script location 
y = function(){dev.new();x=par(no.readonly=T); dev.off(); x} #'
par(y()); options(scipen=0); dev.off(); rm(list=ls()); cat("\014")

# Packages ---------------------------------------------------------------------

#update.packages()

library("readxl")
library("dplyr")
library("e1071")
library("ggplot2")
library("urca")
library("timeSeries")
library("tseries")
library("feasts") 
library("forecast") 
library("moments")
library("tidyverse")
library("gridExtra")

theme_set(theme_bw())  # set plotting theme

windowsFonts(C=windowsFont("Segoe UI"))
UTblue <- rgb(5/255, 110/255, 167/255, 1)

# 1. --------------------------------------------------------------------------- 
#' Read the data1.xlsx dataset into your working environment. 
#' Create three time series corresponding to the interest rates in the 
#' Euro Area, Denmark, and Sweden.

DATA <- read_excel("data1.xlsx") 


EA <- ts(DATA$EA, start = 1990, frequency = 12)
Denmark <- ts(DATA$Denmark, start = 1990, frequency = 12)
Sweden <- ts(DATA$Sweden, start = 1990, frequency = 12)

# 2. ---------------------------------------------------------------------------
#' Familiarize yourself with the variables by plotting them separately 
#' and jointly in one graph.   	


# png("EA.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "transparent")
ts.plot(EA, gpars = list(main = "Euro Area", xlab="", ylab="in %", lwd = 4, col = UTblue, 
                              las = 1, ylim = c(-1,12), xlim = c(1990,2022)))
grid(NA, NULL, lwd = 2, col = "black")
#dev.off()

# png("Denmark.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "transparent")
ts.plot(Denmark, gpars = list(main = "Denmark", xlab="", ylab="in %", lwd = 4, col = UTblue, 
                         las = 1, ylim = c(-1,19), xlim = c(1990,2022)))
grid(NA, NULL, lwd = 2, col = "black")
#dev.off()

# png("Sweden.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "transparent")
ts.plot(Sweden, gpars = list(main = "Sweden", xlab="", ylab="in %", lwd = 4, col = UTblue, 
                         las = 1, ylim = c(-1,23), xlim = c(1990,2022)))
grid(NA, NULL, lwd = 2, col = "black")
#dev.off()


# png("Joint.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "transparent", res=200)
cbind(Sweden, Denmark, EA) %>% # change the order to make EA visible
  autoplot() +
  geom_line(size = 1.2) +
  xlab("") + ylab("%") + 
  ggtitle("                      3-Month Money Market Interest Rates")+
  theme(legend.title=element_blank())
dev.off()

# 3. ---------------------------------------------------------------------------
#' Summarize, test  the variables for normality, and compute the histograms. 
#' Which type of skewness is present? 

summary(EA)
summary(Denmark)
summary(Sweden)

# Test for normality
jarque.bera.test(EA) # non-normal
jarque.bera.test(Denmark) # non-normal
jarque.bera.test(Sweden) # non-normal

moments::skewness(EA) # 0.8933608  positive a.k.a. right skewness
moments::skewness(Denmark) # 1.203506  
moments::skewness(Sweden) # 1.305928  

moments::kurtosis(EA) # 2.929249 flatter than normal a.k.a #' platykurtic 
                                                           #' (but almost
                                                           #' normal a.k.a
                                                           #' mesokurtic)

moments::kurtosis(Denmark) # 4.30285 more peaked than normal a.k.a leptokurtic 
moments::kurtosis(Sweden) #  4.50712

#' try another package to directly calculate excess kurtosis

e1071::kurtosis(EA) # -0.08586996

# Histograms 

hist(EA, breaks = 120, col = UTblue, 
     xlab = "Return", main ="EA's 3-MMM Rates")

#' With the normal distribution and the symmetry lines
h <- hist(EA, breaks = 120, col = UTblue, 
          xlab = "Return", main ="EA's 3-MMM Rates")
xfit <- seq(min(EA), max(EA), length = 1000) 
yfit <- dnorm(xfit, mean = mean(EA), sd = sd(EA)) 
yfit <- yfit * diff(h$mids[1:2]) * length(EA) 
lines(xfit, yfit, col="red", lwd=2) 
abline(v = mean(EA), col="red") 

hist(Denmark, breaks = 120, col = UTblue, 
     xlab = "Return", main ="Denmark's 3-MMM Rates")
hist(Sweden, breaks = 120, col = UTblue, 
     xlab = "Return", main ="Sweden's 3-MMM Rates")

# 4. ---------------------------------------------------------------------------
#' Which pair of the three variables is the closest to each other? 
#' Show your conclusions.

cor(EA, Denmark) # 0.9738359 highest degree of association
cor(EA, Sweden) # 0.9731517
cor(Sweden, Denmark) # 0.9390906

#\\\\\\
#'[Correlation is a measure of a LINEAR association between variables, e.g.:]
x <- rnorm(1000000)
y <- x^2
cor(x, y) # zero correlation although x and y are perfectly dependent
#\\\\\\

#' Other Measures
#' Mutual Information
library("mpmi")
cmi.pw(EA, Denmark)$mi # 1.615969 the most "mutual information"
cmi.pw(EA, Sweden)$mi # 1.527722
cmi.pw(Sweden, Denmark)$mi # 1.359872 the least "mutual information"

#' Scatter Plots
#' ...

#' Dispersion
#' ... 

#' Distance Correlation
library("energy")
dcor(EA, Denmark) # 0.9829918 same here
dcor(EA, Sweden) # 0.9760837
dcor(Sweden, Denmark) # 0.9494058

# 5. ---------------------------------------------------------------------------
#' Now focus on the period starting from the year 2013. What has changed 
#' with respect to 3. and 4.?

EA_2013 <- window(EA, start = 2013)
Denmark_2013 <- window(Denmark, start = 2013)
Sweden_2013 <- window(Sweden, start = 2013)

summary(EA_2013) # mean<0 ; max<1
summary(Denmark_2013) # same here
summary(Sweden_2013) # mean~0 ; 1<max<2


jarque.bera.test(EA_2013) # still non-normal
jarque.bera.test(Denmark_2013) #
jarque.bera.test(Sweden_2013) # 

moments::skewness(EA_2013) # 0.62266  less right-skewed
moments::skewness(Denmark_2013) # 0.9478647
moments::skewness(Sweden_2013) #  1.073991 

e1071::kurtosis(EA_2013) # -0.9254132 more flat
e1071::kurtosis(Denmark_2013) # -0.591008 more flat 
e1071::kurtosis(Sweden_2013) #  0.02059571 almost normal-like

hist(EA_2013, breaks = 120, col = UTblue, 
     xlab = "Return", main ="EA's 3-MMM Rates")

hist(Denmark_2013, breaks = 120, col = UTblue, 
     xlab = "Return", main ="Denmark's 3-MMM Rates") #' almost a bimodal
                                                     #' or a multimodal distribution

h <- hist(Sweden_2013, breaks = 120, col = UTblue, 
          xlab = "Return", main ="Sweden's 3-MMM Rates")
xfit <- seq(min(Sweden_2013), max(Sweden_2013), length = 1000) 
yfit <- dnorm(xfit, mean = mean(Sweden_2013), sd = sd(Sweden_2013)) 
yfit <- yfit * diff(h$mids[1:2]) * length(Sweden_2013) 
lines(xfit, yfit, col="red", lwd=2) 
abline(v = mean(Sweden_2013), col="red")


cor(EA_2013, Denmark_2013) # 0.864288 still highest degree of association
cor(EA_2013, Sweden_2013) # 0.7023434
cor(Sweden_2013, Denmark_2013) # 0.7760128 higher relative correlation 


cmi.pw(EA_2013, Denmark_2013)$mi # 0.9472047 
cmi.pw(EA_2013, Sweden_2013)$mi # 1.044358 now the most "mutual information"
cmi.pw(Sweden_2013, Denmark_2013)$mi # 0.7597458 still the least "mutual information"


dcor(EA_2013, Denmark_2013) # 0.89413498 similar change like plain correlation
dcor(EA_2013, Sweden_2013) # 0.796396
dcor(Sweden_2013, Denmark_2013) # 0.8316933

# 6. ---------------------------------------------------------------------------
#' Convert the monthly data into quarterly. Calculate the average annual 
#' change based on the resulting time series.


#' Averaging
EA_2013q <- aggregate(EA_2013, nfrequency = 4, mean)
Denmark_2013q <- aggregate(Denmark_2013, nfrequency = 4, mean)
Sweden_2013q <- aggregate(Sweden_2013, nfrequency = 4, mean)

data_frameq <- data.frame(EA_2013q, Denmark_2013q, Sweden_2013q)
names(data_frameq) <- c("EA_2013q", "Denmark_2013q", "Sweden_2013q")

#' or

#' Skip Sampling
#' ...


#' Using continuous growth rates with logs are not at all considered since the 
#' mmm rates would have to be non-negative

n <- nrow(data_frameq)

data_frame_ch <- data_frameq[-c((4-3):4),] - data_frameq[-c((n-3):n),] #' annual
                                                                       #' change
                                                                       #' in pp
AARC <- colSums(data_frame_ch)/nrow(data_frame_ch) #' on average, negative change 
                                                   #' every year

# 7. ---------------------------------------------------------------------------
#' Use the original monthly time series for Sweden for the rest of the 
#' analysis. Check this variable for the existence of a trend. 
#' Take the necessary actions.

#' Unit root tests help to distinguish formally between the trend- and 
#' difference-stationary behavior of a time series

# ADF test with a trend and a constant
summary(ur.df(Sweden, selectlags = "BIC", type ="trend")) 
#' tt & constant are significant at the 5%  
#' phi2-statistic (joint NULL) is FTR so that there is a unit root and we can 
#' exclude drift and trend terms

# Take first differences
Sweden_d <- diff(Sweden, 12) # annual change

ts.plot(Sweden_d, gpars = list(main = "Sweden", xlab="", ylab="in %", lwd = 4, col = UTblue, 
                             las = 1,  xlim = c(1990,2022)))
grid(NA, NULL, lwd = 2, col = "black")

# 8. ---------------------------------------------------------------------------
#' Determine the appropriate lag length for an ARIMA(p,d,q) model. 
#' Estimate it and interpret the results.

# Check the (P)ACF
Acf(Sweden_d, lag.max = 20, ylab = "", 
    main = "", col = UTblue, lwd = 4, las = 1) # oscillates

Pacf(Sweden_d, lag.max = 20, ylab = "", 
     main = "", col = UTblue, lwd = 4, las = 1) # breaks off

# Estimate ARIMA(p, d, q)

ic_arma <- data.frame("AR" = NA, "MA" = NA, "AIC" = NA, "BIC" = NA)

r <- 1

for(q in c(0:10)){
  for(p in c(0:10)){
    ic_arma[p+r,"AR"] <- p
    ic_arma[p+r,"MA"] <- q
    ic_arma[p+r,"AIC"] <- Arima(Sweden_d, order = c(p,0,q))["aic"][[1]]
    ic_arma[p+r,"BIC"] <- Arima(Sweden_d, order = c(p,0,q))["bic"][[1]]
  }
  r <- r + 11
}

# Obtain model with the minimum IC
ic_arma[which(ic_arma["AIC"]==min(ic_arma["AIC"])),] # ARMA(10,9)
ic_arma[which(ic_arma["BIC"]==min(ic_arma["BIC"])),] # ARMA(10,9)

arma10_9 <- Arima(Sweden_d, order = c(10,0,9))
arma10_9

# 9. ---------------------------------------------------------------------------

#' Perform a test for residual autocorrelation. Justify the number of lags 
#' of your choice and interpret the results.

## Test for autocorrelation in residuals
Box.test(resid(arma10_9), lag = 12, type ="Ljung-Box")
Box.test(resid(arma10_9), lag = 6, type ="Ljung-Box")

#' FTR at the 5% significance level => no autocorrelation => stick with this model

# 10. ---------------------------------------------------------------------------
#' Forecast the interest rate for the period of March 2021-March 2022. 
#' Measure the accuracy formally and graphically. Comment on the results.

#' First, cut the TS; stop before March 2021 for out-of-sample forecasts

Sweden_d_03_21 <- window(Sweden_d, end = c(2021,2))

# View(as_tsibble(Sweden_d_03_21))

Acf(Sweden_d_03_21, lag.max = 20, ylab = "", 
    main = "", col = UTblue, lwd = 4, las = 1) # oscillates

Pacf(Sweden_d_03_21, lag.max = 20, ylab = "", 
     main = "", col = UTblue, lwd = 4, las = 1) # breaks off

# Estimate ARIMA(p, d, q)

ic_arma <- data.frame("AR" = NA, "MA" = NA, "AIC" = NA, "BIC" = NA)

r <- 1

for(q in c(0:10)){
  for(p in c(0:10)){
    ic_arma[p+r,"AR"] <- p
    ic_arma[p+r,"MA"] <- q
    ic_arma[p+r,"AIC"] <- Arima(Sweden_d_03_21, order = c(p,0,q))["aic"][[1]]
    ic_arma[p+r,"BIC"] <- Arima(Sweden_d_03_21, order = c(p,0,q))["bic"][[1]]
  }
  r <- r + 11
}

# Obtain model with the minimum IC
ic_arma[which(ic_arma["AIC"]==min(ic_arma["AIC"])),] # ARMA(10,9)
ic_arma[which(ic_arma["BIC"]==min(ic_arma["BIC"])),] # ARMA(8,10)

arma10_9 <- Arima(Sweden_d_03_21, order = c(10,0,9))
arma8_10 <- Arima(Sweden_d_03_21, order = c(8,0,10))

arma10_9
arma8_10

## Test for autocorrelation in residuals
Box.test(resid(arma10_9), lag = 12, type ="Ljung-Box")
Box.test(resid(arma8_10), lag = 6, type ="Ljung-Box")

# Check if the ARIMA forecasts "beat" the naive one
forecast_arma10_9 <- forecast(arma10_9, h = 13)
forecast_arma8_10 <- forecast(arma8_10, h = 13)
forecast_naive  <- naive(Sweden_d_03_21, h = 13)

summary(forecast_arma10_9)
summary(forecast_arma8_10)
summary(forecast_naive)

accuracy(forecast_arma10_9, Sweden_d)

#' MAE 0.3953190
#' RMSE 0.4193220
#' Theil's U 42.63248 ( > 1 does not outperform the naive one)

accuracy(forecast_arma8_10, Sweden_d)
#' MAE 0.5126298
#' RMSE 0.5351136 
#' Theil's U 56.97012

accuracy(forecast_naive,  Sweden_d) 
#' MAE 0.1523385
#' RMSE 0.1647385
#' Theil's U 18.16672 so high due to a long forecast horizon


p1 <- autoplot(forecast_arma10_9) + 
  xlab("") + ylab("ARIMA(10,0,9)") + 
  coord_cartesian(xlim = c(2018, 2022.3))

p2 <- autoplot(forecast_arma8_10) + 
  xlab("") + ylab("ARIMA(8,0,10)")+ 
  coord_cartesian(xlim = c(2018, 2022.3)) 

p3 <- autoplot(forecast_naive) + 
  xlab("") + ylab("Naive") + 
  coord_cartesian(xlim = c(2018, 2022.3))

p <- grid.arrange(p1, p2, p3, ncol = 1, nrow = 3)

#' Descriptive measures speak in favor of the naive forecast 

