### ARCH - MODELS  ##

## Getting the data ##

first_date <- '2017-01-04' # first date in sample ("2000-01-01" in paper)
last_date <- '2021-06-30' # set Sys.Date() for current date ("2020-06-01" in paper)
my_ticker <- 'AAPL' # Ibovespa ticker (fell free to change to any 

library(BatchGetSymbols)

apple.ts <- BatchGetSymbols(tickers = my_ticker, 
                            first.date = first_date, 
                            last.date = last_date)

apple.val <- BatchGetSymbols(tickers = my_ticker, 
                             first.date = '2021-06-30', 
                             last.date = '2021-11-01')



apple.ts$df.tickers$ref.date <- as.Date(apple.ts$df.tickers$ref.date)
plot(apple.ts$df.tickers$ref.date,apple.ts$df.tickers$price.adjusted,type="l")

## We will do the usual thing with ARIMA models ##
log.apple.ts <-  log(apple.ts$df.tickers$price.adjusted)
diff.log.apple.ts <- diff(log.apple.ts)

par(mfrow=c(3,1))
plot(apple.ts$df.tickers$ref.date,apple.ts$df.tickers$price.adjusted,type="l")
plot(apple.ts$df.tickers$ref.date,log.apple.ts,type="l")
plot(apple.ts$df.tickers$ref.date[-1],diff.log.apple.ts,type="l")


## Model 0: ARIMA(0,1,1) ##
model0 <- arima(log.apple.ts,order = c(0,1,1))

## Model 1: ARIMA(0,1,1)-ARCH(q) model ##
model1.spec <- ugarchspec(variance.model = list(garchOrder=c(6,0)),mean.model = list(armaOrder=c(0,1),include.mean=FALSE))
model1=ugarchfit(spec=model1.spec, data=diff.log.apple.ts)
plot(model1,which='all')

## Model 2: ARIMA(0,1,1)-GARCH(p,q) model ##
model2.spec <- ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,1),include.mean=FALSE))
model2=ugarchfit(spec=model2.spec, data=diff.log.apple.ts)
plot(model2, which = "all")

## Model 3: Same model 2, with t-distribution
model3.spec <- ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,1),include.mean=FALSE),distribution.model = "std")
model3=ugarchfit(spec=model3.spec, data=diff.log.apple.ts)
plot(model3, which = "all")


## Model 4: eGARCH model 
model4.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,1)), distribution.model = "std")
model4=ugarchfit(spec=model4.spec, data=diff.log.apple.ts)
plot(model4,which="all")

## Forecast via Bootstrapping ##
n <- length(apple.ts$df.tickers$price.adjusted)
h <- 80
trueobs <- apple.val$df.tickers$price.adjusted[1:(h+1)]

model0.forecast  <- forecast(model0,h =h,lambda = 0,bootstrap = T)
model1.forecast <- ugarchboot(model1, n.ahead = h,method="Partial",n.bootpred = 500)
model2.forecast <- ugarchboot(model2, n.ahead = h,method="Partial",n.bootpred = 500)
model3.forecast <- ugarchboot(model3, n.ahead = h,method="Partial",n.bootpred = 500)
model4.forecast <- ugarchboot(model4, n.ahead = h,method="Partial",n.bootpred = 500)


## Back-transforming the predictions ##
init=apple.ts$df.tickers$price.adjusted[1]
difflogdata = diff.log.apple.ts
backtransform <- function(x,difflogdata=NULL,init=NULL){
  if(is.null(init)){print("Error: Initial value of the original time series is needed")}
  if(is.null(difflogdata)){print("The transformed time series is required")}
  n <- length(difflogdata)
  ts.all <- c(difflogdata,x)
  cs <- cumsum(ts.all)
  newts <- exp(cs[(n+1):length(ts.all)])*init
  return(newts)
}

model1.forecast <- t(apply(model1.forecast@fseries,1,backtransform,difflogdata=difflogdata,init=init))
model2.forecast <- t(apply(model2.forecast@fseries,1,backtransform,difflogdata=difflogdata,init=init))
model3.forecast <- t(apply(model3.forecast@fseries,1,backtransform,difflogdata=difflogdata,init=init))
model4.forecast <- t(apply(model4.forecast@fseries,1,backtransform,difflogdata=difflogdata,init=init))


par(mfrow=c(2,3))

## Model 0 ##
plot(1:n,apple.ts$df.tickers$price.adjusted[1:n],type="l",xlim=c(n-100,n+h),ylim=c(100,200))
lines(seq(n+1,n+h),model0.forecast$mean,col="red")

## Model 1 ##
plot(1:n,apple.ts$df.tickers$price.adjusted[1:n],type="l",xlim=c(n-100,n+h),ylim=c(100,200))
for(i in 1:nrow(model1.forecast)){
  lines(seq(n+1,n+h),model1.forecast[i,],col="lightgrey")
}
lines(seq(n+1,n+h),apply(model1.forecast, 2, mean),col="red")
lines(seq(n+1,n+h),apply(model1.forecast, 2, quantile,probs=0.025),col="blue")
lines(seq(n+1,n+h),apply(model1.forecast, 2, quantile,probs=0.975),col="blue")
lines(seq(n,n+h),trueobs,col="darkgreen")

## Model 2 ##
plot(1:n,apple.ts$df.tickers$price.adjusted[1:n],type="l",xlim=c(n-100,n+h),ylim=c(100,200))
for(i in 1:nrow(model2.forecast)){
  lines(seq(n+1,n+h),model2.forecast[i,],col="lightgrey")
}
lines(seq(n+1,n+h),apply(model2.forecast, 2, mean),col="red")
lines(seq(n+1,n+h),apply(model2.forecast, 2, quantile,probs=0.025),col="blue")
lines(seq(n+1,n+h),apply(model2.forecast, 2, quantile,probs=0.975),col="blue")
lines(seq(n,n+h),trueobs,col="darkgreen")

## Model 3 ##
plot(1:n,apple.ts$df.tickers$price.adjusted[1:n],type="l",xlim=c(n-100,n+h),ylim=c(100,200))
for(i in 1:nrow(model3.forecast)){
  lines(seq(n+1,n+h),model3.forecast[i,],col="lightgrey")
}
lines(seq(n+1,n+h),apply(model3.forecast, 2, mean),col="red")
lines(seq(n+1,n+h),apply(model3.forecast, 2, quantile,probs=0.025),col="blue")
lines(seq(n+1,n+h),apply(model3.forecast, 2, quantile,probs=0.975),col="blue")
lines(seq(n,n+h),trueobs,col="darkgreen")

## Model 4 ##
plot(1:n,apple.ts$df.tickers$price.adjusted[1:n],type="l",xlim=c(n-100,n+h),ylim=c(100,200))
for(i in 1:nrow(model4.forecast)){
  lines(seq(n+1,n+h),model4.forecast[i,],col="lightgrey")
}
lines(seq(n+1,n+h),apply(model4.forecast, 2, mean),col="red")
lines(seq(n+1,n+h),apply(model4.forecast, 2, quantile,probs=0.025),col="blue")
lines(seq(n+1,n+h),apply(model4.forecast, 2, quantile,probs=0.975),col="blue")
lines(seq(n,n+h),trueobs,col="darkgreen")

