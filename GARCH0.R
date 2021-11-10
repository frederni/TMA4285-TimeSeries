### ARCH - MODELS  ##

## Getting the data ##

first_date <- '2007-01-01' # first date in sample ("2000-01-01" in paper)
last_date <- '2021-07-24' # set Sys.Date() for current date ("2020-06-01" in paper)
my_ticker <- 'AAPL' # Ibovespa ticker (fell free to change to any 

library(BatchGetSymbols)

apple.ts <- BatchGetSymbols(tickers = my_ticker, 
                         first.date = first_date, 
                         last.date = last_date)

apple.ts$df.tickers$ref.date <- as.Date(apple.ts$df.tickers$ref.date)
plot(apple.ts$df.tickers$ref.date,apple.ts$df.tickers$price.adjusted,type="l")

## We will do the usual thing with ARIMA models ##
log.apple.ts <-  log(apple.ts$df.tickers$price.adjusted)
diff.log.apple.ts <- diff(log.apple.ts)

par(mfrow=c(3,1))
plot(apple.ts$df.tickers$ref.date,apple.ts$df.tickers$price.adjusted,type="l")
plot(apple.ts$df.tickers$ref.date,log.apple.ts,type="l")
plot(apple.ts$df.tickers$ref.date[-1],diff.log.apple.ts,type="l")

##Fitting an ARIMA model 
par(mfrow=c(2,2))
acf.appl=acf(log.apple.ts,main='ACF Apple',lag.max=100,ylim=c(-
                                                            0.5,1))
pacf.appl=pacf(log.apple.ts,main='PACF
               Apple',lag.max=100,ylim=c(-0.5,1))

acf.appl=acf(diff.log.apple.ts,main='ACF Difference Log
Apple',lag.max=100,ylim=c(-0.5,1))
pacf.appl=pacf(diff.log.apple.ts,main='PACF Difference Log
               Apple',lag.max=100,ylim=c(-0.5,1))


## Fitting ARIMA(1,1,0) model  ##
library(forecast)
auto.arima(diff.log.apple.ts,ic="aicc")
arima511=arima(log.apple.ts,order=c(5,1,1))
summary(arima511)

par(mfrow=c(2,1))
plot(arima511$residuals)
acf(arima511$residuals)
Box.test(arima511$residuals,type="Ljung-Box",lag = 12)

res.arima511=arima511$res
squared.res.arima511=res.arima511^2
par(mfcol=c(1,1))
plot(squared.res.arima511,main='Squared Residuals')
acf.squared511=acf(squared.res.arima511,main='ACF Squared
                   Residuals',lag.max=100,ylim=c(-0.5,1))
pacf.squared511=pacf(squared.res.arima511,main='PACF Squared
                     Residuals',lag.max=100,ylim=c(-0.5,1))

