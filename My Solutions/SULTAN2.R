#### Ans to the question number 1.1:


library(readxl)
data <- read_excel("data2.xlsx")
## Create Time Series
IND_PROD <- ts(data$IND_PROD_G, start = c(2013,5), end = c(2022,2), frequency = 12)
HICP <- ts(data$HICP, start = c(2013,5), end = c(2022,2),  frequency = 12)
TR_VOL <- ts(data$TR_VOL, start = c(2013,5), end = c(2022,2),  frequency = 12)
INT_R <- ts(data$INT_R, start = c(2013,5), end = c(2022,2),  frequency = 12)

## Plot
png("Relation_Analysis.png", width = 2000, height = 1000,
      pointsize = 32, family = "C")
par(mfrow=c(2,2))
ts.plot(IND_PROD, xlab="Time", ylab="Growth", main="Industrial production growth")
ts.plot(TR_VOL, xlab="Time", ylab="Volumes", main="Transaction volumes of the national payment system")
ts.plot(INT_R,xlab="Time", ylab="Interest rate", main="Interest rate")
ts.plot(HICP,xlab="Time", ylab="HICP", main="HICP")
dev.off()


#### Ans to the question number 1.2:


## Adding the column the annual inflation rate.
library(quantmod)
data$AIR <- (Delt(HICP, k=12, type ="log"))*100
data_new <- data[,c(1,2,4,5,6)] # Removing the HICP column

## Short descriptive statistics table for all four variables.
library(psych)
table<-describe(data_new[,c(2,3,4,5)])
View(table)


#### Ans to the question number 1.3:


# After replacing the HICP, the length of the AIR is different from the original HICP.
data_new <- na.omit(data_new) # Removed the NA values to equalize the length of all variables.

## check the stationarity for the following test.

library(tseries)
adf.test(data_new$IND_PROD_G) # p value is less than 5%. Has no unit root. So, the data is stationary
adf.test(data_new$AIR) # p value is greater than 5%. Has unit root. So, the data is not stationary
adf.test(data_new$TR_VOL) # p value is greater than 5%. Has unit root. So, the data is not stationary
adf.test(data_new$INT_R) # p value is less than 5%. Has unit no root. So, the data is stationary

## Taking the first diff of TR_VOL and AIR to make the non stationary data into stationary data. 

IND_PROD <- ts(data_new$IND_PROD_G, start = c(2014,5), end = c(2022,2), frequency = 12)
AIR <- ts(diff(data_new$AIR), start = c(2014,5), end = c(2022,2),  frequency = 12)
TR_VOL <- ts(diff(data_new$TR_VOL), start = c(2014,5), end = c(2022,2),  frequency = 12)
INT_R <- ts(data_new$INT_R, start = c(2014,5), end = c(2022,2),  frequency = 12)

#### Ans to the question number 1.4: 


library(lmtest)
library(dynlm)
library(car)
multi_ind_prod <- dynlm(IND_PROD ~ L(IND_PROD,1:4) + L(AIR,1:4) + L(TR_VOL,1:4) + L(INT_R,1:4))
multi_air <- dynlm(AIR ~ L(AIR,1:4) + L(IND_PROD,1:4) + L(TR_VOL,1:4) + L(INT_R,1:4))
multi_tr_vol <- dynlm(TR_VOL ~ L(TR_VOL,1:4) + L(AIR,1:4) + L(IND_PROD,1:4) + L(INT_R,1:4))
multi_int_r <- dynlm(INT_R ~ L(INT_R,1:4) + L(AIR,1:4) + L(TR_VOL,1:4) + L(IND_PROD,1:4))

## Industrial production growth. 

linearHypothesis(multi_ind_prod, paste0("L(AIR, 1:4)", 1:4, "=0"))
# P value is higher than 5%. The result is insignificant. 
# Industrial production growth does not granger causes annual inflasion rate. 

linearHypothesis(multi_ind_prod, paste0("L(TR_VOL, 1:4)", 1:4, "=0"))
# P value is higher than 5%. The result is insignificant. 
# Industrial production growth does not granger causes transaction volumes. 

linearHypothesis(multi_ind_prod, paste0("L(INT_R, 1:4)", 1:4, "=0"))
# P value is higher than 5%. The result is insignificant. 
# Industrial production growth does not granger causes interest rate. 

## Annual inflation rate

linearHypothesis(multi_air, paste0("L(IND_PROD, 1:4)", 1:4, "=0"))
# P value is higher than 5%. The result is insignificant. 
# Annual inflation rate does not granger causes industrial production. 

linearHypothesis(multi_air, paste0("L(TR_VOL, 1:4)", 1:4, "=0"))
# P value is higher than 5%. The result is insignificant. 
# Annual inflation rate does not granger causes transaction volumes. 

linearHypothesis(multi_air, paste0("L(INT_R, 1:4)", 1:4, "=0"))
# P value is higher than 5%. The result is insignificant. 
# Annual inflation rate does not granger causes interest rate.

## Transaction volumes

linearHypothesis(multi_tr_vol, paste0("L(IND_PROD, 1:4)", 1:4, "=0"))
# P value is higher than 5%. The result is insignificant. 
# Transaction volumes does not granger causes industrial production. 

linearHypothesis(multi_tr_vol, paste0("L(AIR, 1:4)", 1:4, "=0"))
# P value is higher than 5%. The result is insignificant. 
# Transaction volumes does not granger causes annual inflasion rate.

linearHypothesis(multi_tr_vol, paste0("L(INT_R, 1:4)", 1:4, "=0"))
# P value is higher than 5%. The result is insignificant. 
# Transaction volumes does not granger causes interest rate.

## Interest rate

linearHypothesis(multi_int_r, paste0("L(IND_PROD, 1:4)", 1:4, "=0"))
# P value is higher than 5%. The result is insignificant. 
# Interest rate does not granger causes industrial production.

linearHypothesis(multi_int_r, paste0("L(AIR, 1:4)", 1:4, "=0"))
# P value is higher than 5%. The result is insignificant. 
# Interest rate does not granger causes annual inflation rate.

linearHypothesis(multi_int_r, paste0("L(TR_VOL, 1:4)", 1:4, "=0"))
# P value is higher than 5%. The result is insignificant. 
# Interest rate does not granger causes transaction volumes.



#### Ans to the question number 1.5: 


multi_ind_prod_inst <- dynlm(IND_PROD ~ L(IND_PROD,1:4) + L(AIR,0:4) + L(INT_R,0:4) + L(TR_VOL,0:4))
multi_air_inst <- dynlm(AIR ~ L(AIR,1:4) + L(IND_PROD,0:4) + L(INT_R,0:4) + L(TR_VOL,0:4))
multi_int_r_inst <- dynlm(INT_R ~ L(INT_R,1:4) + L(AIR,0:4) + L(IND_PROD,0:4) + L(TR_VOL,0:4))
multi_tr_vol_inst <- dynlm(TR_VOL ~ L(TR_VOL,1:4) + L(AIR,0:4) + L(INT_R,0:4) + L(IND_PROD,0:4))

## Industrial production growth. 

 linearHypothesis(multi_ind_prod_inst, "L(AIR, 0:4)0=0")
# P value is smaller than 5%. The result is significant. 
# Industrial production growth instantaneously granger causes annual inflation rate.

linearHypothesis(multi_ind_prod_inst, "L(INT_R, 0:4)0=0")
# P value is smaller than 5%. The result is significant. 
# Industrial production growth instantaneously granger causes annual interest rate.

linearHypothesis(multi_ind_prod_inst, "L(TR_VOL, 0:4)0=0")
# P value is higher than 5%. The result is insignificant. 
# Industrial production growth does not instantaneously granger causes transaction volumes.

## Annual interest rate.

linearHypothesis(multi_air_inst, "L(IND_PROD, 0:4)0=0")
# P value is higher than 5%. The result is insignificant. 
# Annual interest rate does not instantaneously granger causes industrial production growth.

linearHypothesis(multi_air_inst, "L(INT_R, 0:4)0=0")
# P value is higher than 5%. The result is insignificant. 
# Annual interest rate does not instantaneously granger causes interest rate.

linearHypothesis(multi_air_inst, "L(TR_VOL, 0:4)0=0")
# P value is higher than 5%. The result is insignificant. 
# Annual interest rate does not instantaneously granger causes transaction volumes.

## Interest rate

linearHypothesis(multi_int_r_inst, "L(IND_PROD, 0:4)0=0")
# P value is smaller than 5%. The result is significant. 
# Interest rate instantaneously granger causes industrial production growth.

linearHypothesis(multi_int_r_inst, "L(AIR, 0:4)0=0")
# P value is higher than 5%. The result is insignificant. 
# Interest rate does not instantaneously granger causes annual inflation rate.

linearHypothesis(multi_int_r_inst, "L(TR_VOL, 0:4)0=0")
# P value is higher than 5%. The result is insignificant. 
# Interest rate does not instantaneously granger causes transaction volumes

## Transaction volumes

linearHypothesis(multi_tr_vol_inst, "L(IND_PROD, 0:4)0=0")
# P value is higher than 5%. The result is insignificant. 
# Transaction volumes does not instantaneously granger causes industrial production growth.

linearHypothesis(multi_tr_vol_inst, "L(AIR, 0:4)0=0")
# P value is higher than 5%. The result is insignificant. 
# Transaction volumes does not instantaneously granger causes annual inflation rate.

linearHypothesis(multi_tr_vol_inst, "L(INT_R, 0:4)0=0")
# P value is higher than 5%. The result is insignificant. 
# Transaction volumes does not instantaneously granger causes interest rate.

## Feedback effect

# we have feedback effect on industrial production growth and interest rate.



#### Ans to the question number 1.6:


## VAR Model
library(vars)
library(systemfit)
int <- cbind(IND_PROD,AIR,TR_VOL,INT_R)

## lag length selection
VARselect(int, lag.max = 6, type = "cons")
# selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
#   2      1      1      2 

## Estimating specific VAR model with the best fit. 
var2 <- VAR(int, type = "const", p = 2, ic = "AIC")


#### Ans to the question number 1.7:


## Test for Autocorrelation
resid_VAR2 <- resid(var2) 

# For 12 lags
Box.test(resid_VAR2[,1], lag = 12, type = "Ljung-Box") 
# p-value = 0.8383. Residuals are independent. Means there is no autocorrelation.
Box.test(resid_VAR2[,2], lag = 12, type = "Ljung-Box")
# p-value = 0.545. Residuals are independent. Means there is no autocorrelation.
Box.test(resid_VAR2[,3], lag = 12, type = "Ljung-Box")
# p-value = 7.912e-08. Residuals are not independent. Means there is autocorrelation.
Box.test(resid_VAR2[,4], lag = 12, type = "Ljung-Box")
# p-value = 0.7183. Residuals are independent. Means there is no autocorrelation.

# For 3 lags
Box.test(resid_VAR2[,1], lag = 3, type = "Ljung-Box") 
# p-value = 0.6204. Residuals are independent. Means there is no autocorrelation.
Box.test(resid_VAR2[,2], lag = 3, type = "Ljung-Box")
# p-value = 0.9337. Residuals are independent. Means there is no autocorrelation.
Box.test(resid_VAR2[,3], lag = 3, type = "Ljung-Box")
# p-value = 0.4563. Residuals are independent. Means there is no autocorrelation.
Box.test(resid_VAR2[,4], lag = 3, type = "Ljung-Box")
# p-value = 0.4184. Residuals are independent. Means there is no autocorrelation.

## Lag Length Selection
VARselect(int, lag.max = 6, type = "cons")
## selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
#   2      1      1      2 


#### Ans to the question number 1.8:


## Estimate VAR(3) Model
var3 <- VAR(int, type = "const", p = 3, ic = "AIC")
var3 # Increased the order by 1. 

## Test for Autocorrelation
resid_VAR3 <- resid(var3)

## For 12 lags
Box.test(resid_VAR3[,1], lag = 12, type = "Ljung-Box") 
# p-value = 0.9766. Residuals are independent. Means there is no autocorrelation. (Does not change)
Box.test(resid_VAR3[,2], lag = 12, type = "Ljung-Box")
# p-value = 0.5526. Residuals are independent. Means there is no autocorrelation. (Does not change)
Box.test(resid_VAR3[,3], lag = 12, type = "Ljung-Box")
# p-value = 2.405e-06. Residuals are not independent. Means there is autocorrelation. (Does not change)
Box.test(resid_VAR3[,4], lag = 12, type = "Ljung-Box")
# p-value = 0.9583. Residuals are independent. Means there is no autocorrelation. (Does not changed)

## For 3 lags
Box.test(resid_VAR3[,1], lag = 3, type = "Ljung-Box") 
# p-value = 0.978. Residuals are independent. Means there is no autocorrelation. (changed)
Box.test(resid_VAR3[,2], lag = 3, type = "Ljung-Box")
# p-value = 0.9858. Residuals are independent. Means there is no autocorrelation. (Does not change)
Box.test(resid_VAR3[,3], lag = 3, type = "Ljung-Box")
# p-value = 0.9182. Residuals are independent. Means there is no autocorrelation. (Changed)
Box.test(resid_VAR3[,4], lag = 3, type = "Ljung-Box")
# p-value = 0.7653. Residuals are independent. Means there is no autocorrelation. (Changed)


#### Ans to the question number 1.9


## Impulse response for all 4 variables
irf  <- irf(var3, n.ahead = 48, runs=500)
plot(irf)
## "In terms of AIR, TR_VOL first increases up to 3 period. Then decrease till 6 period.
#  But the shock is not very big. Then again increases and latter become persistent".

## "In terms of IND_PROD, TR_VOL takes a little shock. First increases till 4 period. 
# Then decreases till 5 period. then slowly become persistent".

## "In terms TR_VOL, TR_VOL decreases first up to 2 period.
# Then increases a little(6 period).Then become persistent over time".

## In terms of IND_PROD, TR_VOL takes an increasing shock till 3 period. 
# Then decreases till 6 period. Then become persistent over time.

## In terms of INT_R, there is almost no effect on TR_VOL


## Justify the choice of your Cholesky ordering
int2 <- int[,c(4,2,1,3)]
## Re-Estimate VAR 
var3_co <- VAR(int2, p = 3, type = "const", ic="AIC")
## Impulse Responses
irf2  <- irf(var3_co, n.ahead = 48, runs = 500)
plot(irf2) # Result is almost same. No effect on the new ordering. 


#### Ans to the question number 1.10

FEVD <- fevd(var3, n.ahead = 49)
plot(FEVD) 
# Interest rate is affected most by the payment system volumes(Transaction volumes).
FEVD
# Interest rate changes most for the change of payment system volumes(Transaction volumes).


#### Ans to the question number 2:

















