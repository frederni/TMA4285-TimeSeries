# Project part 2
install.packages("rugarch")

library(rugarch)
library(readxl)

# Preprocessing and data cleaning
data = read_excel('CBLTCUSD_updated.xls')
data <- na.omit(data)
stocks <- data$CBLTCUSD # Remove dates and convert to numeric
plot(data)

log.stocks <- log(stocks)
diff.log.stocks <- diff(log.stocks)


# Naive/Brute force p and q selection
naive_selection <- function(data, pqmax, distrmod){
    info.res <- vector(mode="list", length=4)
    names(info.res) <- c("p", "q", "aic", "bic")
    pb_iter <- (pqmax+1)^2 # Number of iterations
    pb_count <- 0
    pb <- txtProgressBar(min=pb_count, max=pb_iter, style=3, width=50, char="=")
    
    for(p in 0:pqmax){
        for(q in 0:pqmax){
            # Create and fit model for each (p,q)
            tmp_model <- ugarchfit(
                spec=ugarchspec(
                    variance.model = list(garchOrder=c(p,q)),
                    #mean.model = list(armaOrder=c(0,1), include.mean = FALSE),
                    distribution.model = distrmod
                ),
                data=data
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
    
    best_aic <- c(info.res$p[best_aic_index], info.res$q[best_aic_index])
    best_bic <- c(info.res$p[best_bic_index], info.res$q[best_bic_index])
    cat("Best AIC for (p,q) = (", info.res$p[best_aic_index],
        ",", info.res$q[best_aic_index], ")", sep="")
    cat("Best BIC for (p,q) = (", info.res$p[best_bic_index], 
        ",", info.res$q[best_bic_index], ")", sep="")
    return(c(best_aic, best_bic))
}

best_choice_std <- naive_selection(stocks, 4, "std")


# >>>>> Model 1 : T-GARCH

model1.notrans <- ugarchfit(
    spec=ugarchspec(
        variance.model = list(garchOrder=c(1,1)),
        #mean.model = list(armaOrder=c(0,1), include.mean = FALSE),
        distribution.model = "std"
    ),
    data=stocks
)

pred1.notrans <- ugarchforecast(model1.notrans, n.ahead = 20)
plot(pred1.notrans, which=1)

model1.logdiff <- ugarchfit(
    spec=ugarchspec(
        variance.model = list(garchOrder=c(1,1)),
        #mean.model = list(armaOrder=c(0,1), include.mean = FALSE),
        distribution.model = "std"
    ),
    data=diff.log.stocks
)

plot(model1.logdiff, which="all")
pred1.logdiff <- ugarchforecast(model1.logdiff, n.ahead = 20)
plot(pred1.logdiff)
# <<<<< Model 1 : T-GARCH



# ______ Using normal distribution model

model2.spec <- ugarchspec(
    variance.model = list(model = "gjrGARCH"),
    mean.model = list(armaOrder = c(0,1), include.mean = TRUE),
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

model1.logdiff.forecast <- ugarchboot(transf.model1, n.ahead=20,
                                     method="Partial", n.bootpred =500)
model1.logdiff.foreacast <- t(
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

