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

#Slowly decaying pattern in ACF.We observe the expected exponential decay in the ACF plot of this series. 
#So, AR(1) has auto-correlation at lags 1, 2, 3, and so on.
acf(Normal_Time_Series)

#There is a very high first correlation in PACF which implies the existence of trend and it is nonstationarity. 
#We will consider converting the series to a return series and apply GARCH models.
pacf(Normal_Time_Series)

ts_log = log(Normal_Time_Series)
plot(ts_log)

#After applying log transformation the non-stationarity seems to be removed. 
#But the log of series adjusts for change in variance but the trend still exists. 
#Hence, we applied differencing of order 1 to the log series to calculate returns of Bitcoin price to removes the trend.

ts_log_diff = diff(ts_log)
acf(ts_log_diff)
pacf(ts_log_diff)

ga.11 = garch(ts_log_diff, order = c(1,1), trace = FALSE,na.action = na.pass)
summary(ga.11)
#GARCH(1,1) in which all the coefficients are significant, normality is quite bad and has no problem with auto correlation.
ga.12 = garch(ts_log_diff, order = c(1,2), trace = FALSE,na.action = na.pass)
summary(ga.12)

ga.22 = garch(ts_log_diff, order = c(2,2), trace = FALSE,na.action = na.pass)
summary(ga.22)

ga.33 = garch(ts_log_diff, order = c(3,3), trace = FALSE,na.action = na.pass)
summary(ga.33)

ga.20 = garch(ts_log_diff, order = c(2,0), trace = FALSE,na.action = na.pass)
summary(ga.20)



sc.AIC = AIC(ga.11, ga.22, ga.12, ga.33, ga.20)
sc.AIC


#Theoretically seasonality should not be present in a financial time series as it would mean an arbritrage edge.
#The time series does not seem to be stationary due to the presence of a trend. 
#Because of this we cannot apply directly the Box-Jenkins methodology, so we will evaluate if first differences are sufficient to turn the time series stationary.
Normal_Series_Diffed <- diff(Normal_Time_Series)
plot.ts(Normal_Series_Diffed)

#Therefore, we will apply a logarithmic transformation and a subsequent first difference.
Normal_Series_Diffed.log <- diff(log(Normal_Time_Series))
plot.ts(Normal_Series_Diffed.log)


# Residuals analysis 
residual.analysis(ga.11, class="GARCH", start=2)


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



#ARIMA models 
Normal_Series_Diffed.log <- diff(log(Normal_Time_Series))

plot(Normal_Series_Diffed.log,type="l")

#Fitting an ARIMA model 
acf(Normal_Series_Diffed.log)
pacf(Normal_Series_Diffed.log) #value decrease

library(forecast)
model_normal <- auto.arima(Normal_Time_Series) #whithout log and diff #ARIMA(1,1,4)
arima101 = auto.arima(Normal_Series_Diffed.log)
plot(Normal_Series_Diffed.log)
summary(Normal_Series_Diffed.log)
plot(model_normal)
summary(model_normal) #ARIMA(1,1,4)

Box.test(arima101$residuals,type="Ljung-Box",lag = 12)

res.arima101=arima101$res
squared.res.arima101=res.arima101^2
par(mfcol=c(1,1))
plot(squared.res.arima101,main='Squared Residuals')
acf.squared101=acf(squared.res.arima511,main='ACF Squared
                   Residuals',lag.max=100,ylim=c(-0.5,1))
pacf.squared101=pacf(squared.res.arima511,main='PACF Squared
                     Residuals',lag.max=100,ylim=c(-0.5,1))

# Residuals analysis
residual.analysis <- function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA-GARCH")[1]){
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  res.model = na.remove(res.model)
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  acf(res.model,main="ACF of standardised residuals")
  pacf(res.model,main="PACF of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  print(shapiro.test(res.model))
  k=0
  LBQPlot(res.model, lag.max = 30, StartLag = k + 1, k = 0, SquaredQ = FALSE)
}
  
