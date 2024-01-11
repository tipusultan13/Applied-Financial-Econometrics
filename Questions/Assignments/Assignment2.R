#### Analyzing the Monetary Transmission Mechanism ####

# Preamble ---------------------------------------------------------------------

#' (sets working directory, clears memory, resets) 
if (!("rstudioapi" %in% installed.packages())) {install.packages("rstudioapi")}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))# set the working directory to the current R Script location 
y = function(){dev.new();x=par(no.readonly=T); dev.off(); x} #'
par(y()); options(scipen=0); dev.off(); rm(list=ls()); cat("\014")

# Packages ---------------------------------------------------------------------
#update.packages()

library("readxl")
library("dplyr")       # data wrangling, mutate
library("car")         # linear.hypothesis
library("ggplot2")    
library("vars")   
library("flextable")
library("lmtest")
library("dynlm")
library("systemfit")
library("rstatix")


windowsFonts(C=windowsFont("Segoe UI"))
UTblue <- rgb(5/255, 110/255, 167/255, 1)

# 1. --------------------------------------------------------------------------- 
#' Load the multivariate dataset data2.xlsx. Familiarize yourself with the 
#' data (industrial production growth, HICP, transaction volumes of the national 
#' payment system, and interest rate). Plot the variables.

data <- read_excel("data2.xlsx")

# Declare the whole dataset as time series at once
Data <- ts(data[ , 2:5], start = c(2013,5), frequency = 12)

summary(Data[,"IND_PROD_G"])

# png("IND_PROD_G.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "transparent")
ts.plot(Data[,"IND_PROD_G"], gpars = list(main = "Industrial Production Growth", xlab="", ylab="in %", lwd = 4, col = UTblue, 
                                          las = 1, ylim = c(-13,16), xlim = c(2013,2022)))
grid(NA, NULL, lwd = 2, col = "black")
# dev.off()

summary(Data[,"HICP"])

# png("HICP.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "transparent")
ts.plot(Data[,"HICP"], gpars = list(main = "HICP", xlab="", ylab="index", lwd = 4, col = UTblue, 
                                    las = 1, ylim = c(97,106), xlim = c(2013,2022)))
grid(NA, NULL, lwd = 2, col = "black")
# dev.off()

summary(Data[,"TR_VOL"])
Data[,"TR_VOL"] <- Data[,"TR_VOL"]/1000000000 # convert to billions for more aesthetic notation

# png("TR_VOL.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "transparent")
ts.plot(Data[,"TR_VOL"], gpars = list(main = "Payment System Volumes", xlab="", 
                                      ylab="currency units (bn)", lwd = 4, col = UTblue, 
                                      las = 1, xlim = c(2013,2022)))
grid(NA, NULL, lwd = 2, col = "black")
# dev.off()

summary(Data[,"INT_R"])

# png("INT_R.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "transparent")
ts.plot(Data[,"INT_R"], gpars = list(main = "Key Interest Rate", xlab="", ylab="in %", lwd = 4, col = UTblue, 
                                     las = 1, xlim = c(2013,2022)))
grid(NA, NULL, lwd = 2, col = "black")
# dev.off()


# 2. --------------------------------------------------------------------------- 
#' Replace the HICP with the annual inflation rate. Create a short 
#' descriptive statistics table for all four variables.

# INFL <- diff(log(Data[,"HICP"]), 12) * 100

Data[-(1:12), "HICP"] <- diff(log(Data[,"HICP"]), 12) * 100 # overwrite HICP by INFL
Data <- window(Data, start = c(2014, 5)) # get rid of the first 12 observations
colnames(Data) <- c("IND_PROD_G", "INFL", "TR_VOL", "INT_R" ) # replace HICP by INFL

# Visualize the new variable
# png("INFL.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "transparent")
ts.plot(window(Data[, "INFL"], start = c(2014, 5)), gpars = list(main = "Annual Inflation Rate", xlab="", ylab="in %", lwd = 4, col = UTblue, 
                                                                 las = 1, xlim = c(2014,2022)))
grid(NA, NULL, lwd = 2, col = "black")
# dev.off()


# Declare each variable as a ts object for convenience

IND_PROD_G <- ts(Data[,"IND_PROD_G"], start = c(2014,5), frequency = 12)
INFL <- ts(Data[,"INFL"], start = c(2014,5), frequency = 12)
TR_VOL <- ts(Data[,"TR_VOL"], start = c(2014,5), frequency = 12)
INT_R <- ts(Data[,"INT_R"], start = c(2014,5), frequency = 12)

# View(tsibble::as_tsibble(IND_PROD_G))

# Descriptive Statistics

as.data.frame(Data) %>% get_summary_stats(
  IND_PROD_G, INFL, TR_VOL, INT_R,  
  type = "common") %>%
  flextable() 


# 3. --------------------------------------------------------------------------- 
#' Can the variables be used for the following analysis or should they be transformed?

#' It depends as some parts of the analysis may require every single 
#' variable to be stationary and some parts of it would do well with the
#' stationary system of variables. Strictly speaking, the Granger Causality 
#' tests are valid with (weakly) stationary variables. The IRFs require 
#' the whole dynamic system of variables to be stationary [check with vars::roots()]. 
#' In practice it may vary depending on the "school" one follows.

# IND_PROD_G

summary(ur.df(IND_PROD_G, selectlags = "BIC", type ="drift")) #' mean
#' is
#' insignificant

summary(ur.df(IND_PROD_G, selectlags = "BIC", type ="none")) 
# IND_PROD_G is stationary with a zero mean => no transformation needed

# INFL
summary(ur.df(INFL, selectlags = "BIC", type ="drift")) 
summary(ur.df(INFL, selectlags = "BIC", type ="none")) 

# unit root => take first differences

INFL_d <- diff(INFL, 1) # monthly change in pp

# Data[-1, "INFL"] <- diff(Data[,"INFL"], 1) # overwrite 
# Data <- window(Data, start = c(2014, 6)) # get rid of the first observation

summary(ur.df(INFL_d, selectlags = "BIC", type ="drift")) 
summary(ur.df(INFL_d, selectlags = "BIC", type ="none")) 
# stationary with a zero mean at the 5% significance level

# Visualize the new variable
# png("INFL_d.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "transparent")
ts.plot(INFL_d, gpars = list(main = "Monthly Change in Inflation Rate", xlab="", 
                             ylab="in pp", lwd = 4, col = UTblue, 
                             las = 1, xlim = c(2014,2022)))
grid(NA, NULL, lwd = 2, col = "black")
# dev.off()


# TR_VOL

summary(ur.df(TR_VOL, selectlags = "BIC", type ="trend"))
#' tt & constant are significant at the 5% 
#' the NULL of unit root is FTR => stochastic trend 
#' phi2-statistic (joint NULL) is FTR so that there is a unit root and we can 
#' exclude drift and trend terms 
#' => take first differences

TR_VOL_d <- diff(log(TR_VOL), 1) * 100 # monthly percentage change 

summary(ur.df(TR_VOL_d, selectlags = "BIC", type ="drift")) 
# stationary with a non-zero mean at the 5% significance level

# Visualize the new variable
# png("TR_VOL_d.png", width = 1500, height = 1000,
#     pointsize = 32, family = "C", bg = "transparent")
ts.plot(TR_VOL_d, gpars = list(main = "Monthly Change in Transaction Volumes", xlab="", 
                               ylab="in %", lwd = 4, col = UTblue, 
                               las = 1, xlim = c(2014,2022)))
grid(NA, NULL, lwd = 2, col = "black")
# dev.off()

# INT_R
summary(ur.df(INT_R, selectlags = "BIC", type ="trend")) 
summary(ur.df(INT_R, selectlags = "BIC", type ="drift")) 
# INT_R is stationary with a non-zero mean => no transformation needed

# Obtain the final dataset

Data[-1, "INFL"] <- INFL_d # in pp
Data[-1, "TR_VOL"] <- TR_VOL_d # in %
Data <- window(Data, start = c(2014, 6)) # all start in June

# 4. --------------------------------------------------------------------------- 
#' Test for multivariate interdependence and interpret the results at the 5% 
#' significance level.

## Multivariate Granger Causality Tests

multi_IND_PROD_G <- dynlm(IND_PROD_G ~ L(IND_PROD_G,1:6) + L(INFL,1:6) + L(TR_VOL,1:6) + L(INT_R,1:6))
multi_INFL <- dynlm(INFL ~ L(IND_PROD_G,1:6) + L(INFL,1:6) + L(TR_VOL,1:6) + L(INT_R,1:6))
multi_TR_VOL <- dynlm(TR_VOL ~ L(IND_PROD_G,1:6) + L(INFL,1:6) + L(TR_VOL,1:6) + L(INT_R,1:6))
multi_INT_R <- dynlm(INT_R ~ L(IND_PROD_G,1:6) + L(INFL,1:6) + L(TR_VOL,1:6) + L(INT_R,1:6))

# IND_PROD_G, INFL

multi_IND_PROD_G_inst <- dynlm(IND_PROD_G ~ L(IND_PROD_G,1:6) + L(INFL,0:6) + L(TR_VOL,0:6) + L(INT_R,0:6))
multi_INFL_inst       <- dynlm(INFL       ~ L(IND_PROD_G,0:6) + L(INFL,1:6) + L(TR_VOL,0:6) + L(INT_R,0:6))
# multi_TR_VOL_inst <- dynlm(TR_VOL ~ L(IND_PROD_G,0:6) + L(INFL,0:6) + L(TR_VOL,1:6))

linearHypothesis(multi_IND_PROD_G, paste0("L(INFL, 1:6)", 1:6, "=0")) # FTR
linearHypothesis(multi_INFL, paste0("L(IND_PROD_G, 1:6)", 1:6, "=0")) # FTR
linearHypothesis(multi_IND_PROD_G_inst, "L(INFL, 0:6)0=0") # FTR
linearHypothesis(multi_INFL_inst, "L(IND_PROD_G, 0:6)0=0") # FTR

## Continue with:

# IND_PROD_G, TR_VOL

# IND_PROD_G, INT_R

# INFL, TR_VOL

# TR_VOL, INT_R

# INT_R, INFL


# 5. --------------------------------------------------------------------------- 
#' Is there an instantaneous relationship between the time series? 
#' Do you observe a feedback effect?

#' A variable is instantaneously causal to another variable iff
#' the latter also instantaneously causes the former.

#' There is feedback between two variables if both are causal 
#' to each other (only defined for simple relationship).


# IND_PROD_G, INFL
#' no instantaneous relationship
#' no feedback effect

## Continue with:

# IND_PROD_G, TR_VOL

# IND_PROD_G, INT_R

# INFL, TR_VOL

# TR_VOL, INT_R

# INT_R, INFL

# 6. --------------------------------------------------------------------------- 
#' Select the four-variable VAR model specification with the best fit and
#'  estimate it.

VARselect(Data, lag.max = 6, type = "cons")

#' Estimate

VAR1 <- VAR(Data, p = 1)

roots(VAR1) # stable / stationary multivariate system

# 7. --------------------------------------------------------------------------- 
#' Is there residual autocorrelation when you conduct an appropriate test 
#' with 12 lags (with 3 lags)? How do you select the lag length for the test?

resid_VAR1 <- resid(VAR1)

Box.test(resid_VAR1[,1], lag = 12, type = "Ljung-Box")
Box.test(resid_VAR1[,2], lag = 12, type = "Ljung-Box")
Box.test(resid_VAR1[,3], lag = 12, type = "Ljung-Box") #autocorr
Box.test(resid_VAR1[,4], lag = 12, type = "Ljung-Box")

Box.test(resid_VAR1[,1], lag = 3, type = "Ljung-Box") #autocorr
Box.test(resid_VAR1[,2], lag = 3, type = "Ljung-Box")
Box.test(resid_VAR1[,3], lag = 3, type = "Ljung-Box") 
Box.test(resid_VAR1[,4], lag = 3, type = "Ljung-Box") #autocorr

#' Autocorrelation is detected more frequently when testing with up to 3 lags.

#' The lag selection is still more or less an arbitrary choice in the literature 
#' as well as in practice. There has been developed a few methodologies of lag ,
#' selection, yet the most widely used is the rule of thumb: monthly = 12,
#' quarterly = 4, yearly = 1, or its multiples / numbers divisible by them.

# 8. --------------------------------------------------------------------------- 
#' Re-estimate the model after you have increased the VAR (p) order by one.
#'  Repeat the procedure from the previous question. What has changed?

VAR2 <- VAR(Data, p = 2)

Box.test(resid_VAR1[,1], lag = 12, type = "Ljung-Box")
Box.test(resid_VAR1[,2], lag = 12, type = "Ljung-Box")
Box.test(resid_VAR1[,3], lag = 12, type = "Ljung-Box") #autocorr
Box.test(resid_VAR1[,4], lag = 12, type = "Ljung-Box")

Box.test(resid_VAR1[,1], lag = 3, type = "Ljung-Box") #autocorr
Box.test(resid_VAR1[,2], lag = 3, type = "Ljung-Box")
Box.test(resid_VAR1[,3], lag = 3, type = "Ljung-Box") 
Box.test(resid_VAR1[,4], lag = 3, type = "Ljung-Box") #autocorr

#' No change in terms of residual autocorrelation.

# 9. --------------------------------------------------------------------------- 
#' Derive the impulse response functions (IRFs) for all four variables. 
#' Justify the choice of your Cholesky ordering. Use 48 periods and 95 % 
#' confidence bands. Interpret the results with regard to significance, 
#' persistence, and stationarity.

fit <- VAR(Data[, c("IND_PROD_G", "INFL", "INT_R", "TR_VOL")], 
           p = 1, type = "const")

#' Impulse: IND_PROD_G

irfs_IND_PROD_G <- irf(fit, impulse="IND_PROD_G", n.ahead = 48, 
                       ci = 0.95, runs = 500)
plot(irfs_IND_PROD_G)

#' In addition "zoom in" less clear IRFs 
#' and leave out the specification of the periods


irfs_IND_PROD_G1 <- irf(fit, impulse="IND_PROD_G", response = "INFL", 
                        ci = 0.95, runs = 500)
plot(irfs_IND_PROD_G1)

irfs_IND_PROD_G2 <- irf(fit, impulse="IND_PROD_G", response = "INT_R", 
                        ci = 0.95, runs = 500)
plot(irfs_IND_PROD_G2)


#' Impulse: INFL

irfs_INFL <- irf(fit, impulse="INFL", 
                 ci = 0.95, runs = 500)
plot(irfs_INFL)

#' "Zoom in" less clear IRFs 

irfs_INFL1 <- irf(fit, impulse="INFL", response = "INT_R", 
                  ci = 0.95, runs = 500)
plot(irfs_INFL1)


#' Impulse: INT_R

irfs_INT_R <- irf(fit, impulse="INT_R", 
                  ci = 0.95, runs = 500)
plot(irfs_INT_R)

#' "Zoom in" less clear IRFs 

irfs_INFL1 <- irf(fit, impulse="INT_R", response = "INFL", 
                  ci = 0.95, runs = 500)
plot(irfs_INFL1)

irfs_INFL2 <- irf(fit, impulse="INT_R", response = "INT_R", 
                  ci = 0.95, runs = 500)
plot(irfs_INFL2)

#' IMPULSE is one standard deviation to the residuals of an impulse variable

#'   - significance (out of the CB?)
#'   - persistence (peak effect etc.?)
#'   - stationarity (dies out?)
#'
#'
#' - IND_PROD_G:
#'   - insignificant throughout the whole response period
#'
#' - INFL:
#'   - insignificant throughout the whole response period
#'
#' - INT_R:
#'   - significant throughout the whole response period
#'   - persistent decrease after a peak hike with a max impact in the first period, 
#'   appr. 0.06pp
#'   - stationarity is suggested as the IRF dies out
#'   
#'   - TR_VOL:
#'   - insignificant throughout the whole response period


#' Impulse: TR_VOL

irfs_TR_VOL <- irf(fit, impulse="TR_VOL", 
                   ci = 0.95, runs = 500)
plot(irfs_TR_VOL)

#' "Zoom in" less clear IRFs 

irfs_TR_VOL1 <- irf(fit, impulse="TR_VOL", response = "INFL", 
                    ci = 0.95, runs = 500)
plot(irfs_TR_VOL1)

irfs_TR_VOL2 <- irf(fit, impulse="TR_VOL", response = "INT_R", 
                    ci = 0.95, runs = 500)
plot(irfs_TR_VOL2)

irfs_TR_VOL3 <- irf(fit, impulse="TR_VOL", response = "IND_PROD_G", 
                    ci = 0.95, runs = 500)
plot(irfs_TR_VOL3)

# 10. --------------------------------------------------------------------------- 
#' Based on a forecast error variance decomposition, which variable is most 
#' affected by the payment system volumes?

FEVD <- fevd(fit, n.ahead = 48)
plot(FEVD)

FEVD$TR_VOL[1,]  * 100
FEVD$TR_VOL[12,] * 100
FEVD$TR_VOL[24,] * 100
FEVD$TR_VOL[36,] * 100
FEVD$TR_VOL[48,] * 100

#' Even after four years after the shock the impact order does not change. 
#' Most affected are payment transaction volumes themselves in the first place, 
#' in the second - interest rate, third -  inflation rate, fourth - 
#' industrial production growth. 

