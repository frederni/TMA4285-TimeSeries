# Project part 2
install.packages("rugarch")
install.packages('Rcpp')


library(rugarch)
library(readxl)
library(Rcpp)

# Preprocessing and data cleaning
data = read_excel('CBLTCUSD_updated.xls')
data <- na.omit(data)
stocks <- data$CBLTCUSD # Remove dates and convert to numeric
plot(data)

log.stocks <- log(stocks)
diff.log.stocks <- diff(log.stocks)


# Helper functions

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

# Backtransform log-diff ts
init = stocks[1]
difflogdata = diff.log.stocks
backtransform <- function(x, difflogdata, init){
    n <- length(difflogdata)
    ts.all <- c(difflogdata,x)
    cs <- cumsum(ts.all)
    newts <- exp(cs[(n+1):length(ts.all)])*init
    return(newts)
}


# >>>>> Model 0 : sGARCH
# <<<<< Model 0 : sGARCH

# >>>>> Model 1 : T-GARCH

# Function above shows (p,q)=(1,1) to be best fit
model1.notrans <- ugarchfit(
    spec=ugarchspec(
        variance.model = list(garchOrder=c(1,1)),
        distribution.model = "std"
    ),
    data=stocks
)
plot(model1.notrans, which="all")

pred1.notrans <- ugarchforecast(model1.notrans, n.ahead = 27)
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


# Predict and backtrack and stuff
model1.logdiff.forecast <- ugarchboot(model1.logdiff, n.ahead=27,
                                      method="Partial", n.bootpred =500)
model1.logdiff.forecast <- t(
    apply(model1.logdiff.forecast @ fseries,
          1,
          backtransform,
          difflogdata=difflogdata,
          init=init)
)

## Model 2 ##
n <- length(stocks)
plot(1:n,stocks[1:n],type="l",xlim=c(0,n+20),ylim=c(0,400))
for(i in 1:nrow(model1.logdiff.forecast)){
    lines(seq(n+1,n+20),model1.logdiff.forecast[i,],col="lightgrey")
}
lines(seq(n+1,n+20),apply(model1.logdiff.forecast, 2, mean),col="red")
lines(seq(n+1,n+20),apply(model1.logdiff.forecast, 2, quantile,probs=0.025),col="blue")
lines(seq(n+1,n+20),apply(model1.logdiff.forecast, 2, quantile,probs=0.975),col="blue")
lines(seq(n,n+20),trueobs,col="darkgreen")

# TODO
# * Change x axis to be correct dates
# * Import trueobs
# * Fix package issue with Rcpp


# <<<<< Model 1 : T-GARCH




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

