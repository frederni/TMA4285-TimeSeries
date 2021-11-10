# Project part 2
# install.packages("fGarch")

library(fGarch)
library(readxl)
library(tseries)
library(forecast)
Data <- read_excel("CBLTCUSD.xls", 
                   col_types = c("date", "numeric", "numeric"))
Updated_Data <- read_excel("CBLTCUSD_updated.xls", 
                           col_types = c("date", "numeric"))

#Plotting the data
plot(Data$observation_date,Data$CBLTCUSD, type = "l", col = "red", xlab = "Years", ylab = "Value")

#Theoretically seasonality should not be present in a financial time series as it would mean an arbritrage edge.
#The time series does not seem to be stationary due to the presence of a trend. 
#Because of this we cannot apply directly the Box-Jenkins methodology, so we will evaluate if first differences are sufficient to turn the time series stationary.
Normal_Series_Diffed <- diff(Normal_Time_Series)
plot.ts(Normal_Series_Diffed)

#Therefore, we will apply a logarithmic transformation and a subsequent first difference.
Normal_Series_Diffed.log <- diff(log(Normal_Time_Series))
plot.ts(Normal_Series_Diffed.log)

#ACF,PACF
acf(log_model) #old
acf(Normal_Series_Diffed.log)
pacf(Normal_Series_Diffed.log) #value decrease

auto.arima(Normal_Series_Diffed.log)
plot(Normal_Series_Diffed.log)
summary(Normal_Series_Diffed.log)
plot(model_normal)
summary(model_normal) #ARIMA(1,1,4)

# fit an ARCH-GARCH :
mod_res <- model_normal$residuals
tsdisplay(mod_res)

# model GARCH(1,1)
gfit <- garchFit(~garch(1,1), mod_res, trace=F)
summary(gfit) # AIC = 5.46
gfit1 <- garchFit(~garch(1,0), mod_res, trace=F)
summary(gfit1) # too high AIC
gfit2<- garchFit(~garch(2,0), mod_res, trace=F)
summary(gfit2) # AIC = 5.92
gfit3<- garchFit(~garch(3,0), mod_res, trace=F)
sum <- summary(gfit3) # AIC = 5.77
getMethods()



# forecasting and look at the final results.
u <- gfit@sigma.t  # conditional sd
argaplusu <- mod_res + 1.96 * u
argaminusu <- mod_res - 1.96 * u
plot(mod_res, main="GARCH(1,1)")
lines(argaplusu, lty= 2, col = 3)
lines(argaminusu, lty= 2, col = 3)

# Forecast for 6 periods
x <- predict(gfit, n.ahead = 6)
x

Box.test(Normal_Series_Diffed.log, lag = 20, type = "Ljung-Box")

