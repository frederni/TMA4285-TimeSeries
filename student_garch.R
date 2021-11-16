# Project part 2
install.packages("rugarch")

library(rugarch)

# Preprocessing and data cleaning
data = read_excel('CBLTCUSD_updated.xls')
stocks <- as.numeric(data[, 2]) # Remove dates and convert to numeric
stocks <- na.omit(stocks) # Remove invalid stock prices

# Naive/Brute force p and q selection
info.res <- vector(mode="list", length=4)
names(info.res) <- c("p", "q", "aic", "bic")
p_q_lim <- 5
pb_iter <- (p_q_lim+1)^2 # Number of iterations
pb_count <- 0
pb <- txtProgressBar(min=pb_count, max=pb_iter, style=3, width=50, char="=")

for(p in 0:p_q_lim){
    for(q in 0:p_q_lim){
        # Create and fit model for each (p,q)
        tmp_model <- ugarchfit(
            spec=ugarchspec(
                variance.model = list(garchOrder=c(p,q)),
                mean.model = list(armaOrder=c(0,1), include.mean = FALSE),
                distribution.model = "std"
            ),
            data=stocks
        )
        # log(log(nObs)) may produce NaNs in some cases, so
        # we skip iterations with invalid AIC/BIC
        skip_iter <- FALSE
        tryCatch(
            calc <- infocriteria(tmp_model),
            error = function(e){ skip_iter <<- TRUE}
            )
        if(skip_iter) { next }
        
        info.res$p <- append(info.res$p, p)
        info.res$q <- append(info.res$q, q)

        # Extract first 2 results, since we just want Akaike and Bayes
        info.res$aic <- append(info.res$aic, calc[1])
        info.res$bic <- append(info.res$bic, calc[2])
        # Update progress bar
        pb_count <- pb_count + 1
        setTxtProgressBar(pb, pb_count)
    }
}
print("Completed naive p, q test")
best_aic_index = match(min(info.res$aic), info.res$aic)
best_bic_index = match(min(info.res$bic), info.res$bic)
cat("Best AIC for (p,q) = (", info.res$p[best_aic_index],
    ",", info.res$q[best_aic_index], ")", sep="")
cat("Best BIC for (p,q) = (", info.res$p[best_aic_index], 
    ",", info.res$q[best_aic_index], ")", sep="")

#model3=ugarchfit(spec=model3.spec, data=diff.log.apple.ts)
#plot(model3, which = "all")


# Try using rugarch # TODO update with optimal values and don't use gjrGARCH
model1.spec <- ugarchspec(
    variance.model = list(model = "gjrGARCH"),
    mean.model = list(armaOrder = c(6,1), include.mean = TRUE),
    distribution.model = "std"
)
model1 = ugarchfit(spec=model1.spec, data=stocks)

pred1 = ugarchforecast(model1, n.ahead = 20)

plot(pred1) # STD distribution

# ______ Using normal distribution model

model2.spec <- ugarchspec(
    variance.model = list(model = "gjrGARCH"),
    mean.model = list(armaOrder = c(6,1), include.mean = TRUE),
    distribution.model = "norm"
)
model2 = ugarchfit(spec=model2.spec, data=stocks)
pred2 = ugarchforecast(model2, n.ahead = 20)
plot(pred2)

# >>>>> BEGIN   log transforms, differencing and bootstrapping
log.stocks <- log(stocks)
diff.log.stocks <- diff(log.stocks)

# Backtransform
init = stocks[1]
difflogdata = diff.log.stocks
backtransform <- function(x, difflogdata=NULL, init=NULL){
    if(is.null(init)){
        print("Error: Initial value of the original time series is needed")
        }
    if(is.null(difflogdata)){
        print("The transformed time series is required")
        }
    n <- length(difflogdata)
    ts.all <- c(difflogdata,x)
    cs <- cumsum(ts.all)
    newts <- exp(cs[(n+1):length(ts.all)])*init
    return(newts)
}
# trans.model1 <- ...
transf.model1.forecast <- ugarchboot(transf.model1, n.ahead=20,
                                     method="Partial", n.bootpred =500)
transf.model1.foreacast <- t(
    apply(transf.model1.forecast @ fseries,
          1,
          backtransform,
          difflogdata=difflogdata,
          init=init)
    )
# <<<<< END     log transforms, differencing and bootstrapping


# >>>>> BEGIN   dump of unused code
tmp_model_gjrGARCH = ugarchfit(
    spec=ugarchspec(
        variance.model = list(model = "gjrGARCH"),
        mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
        distribution.model = "std"
    ),
    data=stocks,
)

# <<<<< END     dump of unused code

