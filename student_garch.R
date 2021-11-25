# Project part 2
install.packages("rugarch")
install.packages('Rcpp') # Necessary for bootstrapping

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

# Testing observations
trueobs_data <- read_excel('CBLTCUSD_update_2.xls')
trueobs <- trueobs_data$CBLTCUSD


# Helper functions

# Naive/Brute force p and q selection
naive_selection <- function(data, pqmax, distrmod){
    # Helper to select p and q based on lowest AIC/BIC
    # :param data:      should be stocks or the log-diff transformed stock data
    # :param pqmax:     range of combinations to examine, e.g. pqmax=3 will check
    #                   every combination between (0,0) and (3,3)
    # :param distrmod:  Distribution model, e.g. "std" or "norm"
    # :return:          Array of lowest AIC/BIC (p and q printed to console)
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
    info.optimal <- vector(mode="list", length=4)
    names(info.optimal) <- c("aic_val", "bic_val", "aic_param", "bic_param")
    
    best_aic_index = match(min(info.res$aic), info.res$aic)
    best_bic_index = match(min(info.res$bic), info.res$bic)
    
    info.optimal$aic_val <- append(info.optimal$aic_val, min(info.res$aic))
    info.optimal$bic_val <- append(info.optimal$bic_val, min(info.res$bic))
    
    info.optimal$aic_param <- append(
        info.optimal$aic_param, 
        c(info.res$p[best_aic_index], info.res$q[best_aic_index])
        )
    info.optimal$bic_param <- append(
        info.optimal$bic_param,
        c(info.res$p[best_bic_index], info.res$q[best_bic_index])
        )
    
    cat("Best AIC for (p,q) = (", info.optimal$aic_param[1],
        ",", info.optimal$aic_param[2], ")", sep="")
    cat("Best BIC for (p,q) = (", info.optimal$bic_param[1], 
        ",", info.optimal$bic_param[2], ")", sep="")
    return(info.optimal)
}

best_choice_std <- naive_selection(diff.log.stocks, 4, "std")
best_choice_norm <- naive_selection(diff.log.stocks, 4, "norm")

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

# Plot forecasted values
plot_forecast <- function(data, trueobs, n.ahead, ylim, forecasted_model, modeltxt){
    # Generalized plotter function to plot all forecasts similarly
    # data = time series used in modeling
    # trueobs = testing data not used in modeling
    # n.ahead = number of time steps forecasted for
    # forecasted_model = Bootrap-forecasted and backtransformed data
    # modeltxt = string representation of model used
    
    # Plot forecast of backtransformed data
    n <- length(data)
    plot(1:n,stocks[1:n],type="l",xlim=c(as.integer(n/1.25),n+n.ahead),ylim=c(0,ylim),
         main= paste("Forecasting Litecoin using", modeltxt, "model"))
    for(i in 1:nrow(forecasted_model)){
        lines(seq(n+1,n+n.ahead),forecasted_model[i,],col="lightgrey")
    }
    lines(seq(n+1,n+n.ahead),apply(forecasted_model, 2, mean),col="red")
    lines(seq(n+1,n+n.ahead),apply(forecasted_model, 2, quantile,probs=0.025),col="blue")
    lines(seq(n+1,n+n.ahead),apply(forecasted_model, 2, quantile,probs=0.975),col="blue")
    lines(seq(n+1,n+n.ahead), trueobs,col="darkgreen")
    legend("topleft", legend=c(
        "Previous stock value",
        "Mean forecast",
        "97.5% confidence interval",
        "Actual observation"),
        col=c("black", "red", "blue", "darkgreen"),
        lty=1, cex=1.05)
}

numeric_quantile <- function(n.ahead, forecasted_model){
    # Helper to get numeric values of confidance interval to the n.ahead-th prediction
    all_lower <- apply(forecasted_model, 2, quantile,probs=0.025)
    all_upper <- apply(forecasted_model, 2, quantile,probs=0.975)
    conf_interval <- c(all_lower[n.ahead], all_upper[n.ahead], abs(all_lower[n.ahead]-all_upper[n.ahead]))
    return(conf_interval)
}
mean_error <- function(forecasted_model, trueobs){
    # Helper to get mean absolute error between mean forecast and true observations
    mean_preds <- apply(forecasted_model, 2, mean) # goes from [len(dataset), len(dataset) + n.ahead]
    err <- abs(mean_preds - trueobs)
    return(mean(err))
}


# >>>>> Model 0 : sGARCH
best_choice_norm <- naive_selection(diff.log.stocks, 10, "norm")
# Fit with the p and q found from function above
# Use AIC parameters since they produce best forecast results
model0.logdiff <- ugarchfit(
    spec=ugarchspec(
        variance.model = list(garchOrder=best_choice_norm$aic_param),
        mean.model = list(armaOrder=c(0,1)),
        distribution.model = "norm"
    ),
    data=diff.log.stocks
)


for(plotNo in 8:11){
    plot(model0.logdiff, which=plotNo)
}

model0.logdiff.forecast <- ugarchboot(model0.logdiff, n.ahead=27,
                                      method="Partial", n.bootpred =500)
model0.logdiff.forecast <- t(
    apply(model0.logdiff.forecast @ fseries,
          1,
          backtransform,
          difflogdata=difflogdata,
          init=init)
)
plot_forecast(data=stocks, trueobs=trueobs, n.ahead=27, ylim=400,
              forecasted_model = model0.logdiff.forecast, modeltxt=paste(
                  "sGARCH(", best_choice_norm$aic_param[1], ", ",
                  best_choice_norm$aic_param[2], ")", sep=""
                  )
              )


# <<<<< Model 0 : sGARCH

# >>>>> Model 1 : T-GARCH

# Function above shows (p,q)=(1,1) to be best fit
model1.logdiff <- ugarchfit(
    spec=ugarchspec(
        variance.model = list(garchOrder=best_choice_std$aic_param),
        distribution.model = "std"
    ),
    data=diff.log.stocks
)

# Plots indicating general model fit
for(plotNo in 8:11){
    plot(model1.logdiff, which=plotNo)
}


# Predict and and backtransform
model1.logdiff.forecast <- ugarchboot(model1.logdiff, n.ahead=27,
                                      method="Partial", n.bootpred =500)
model1.logdiff.forecast <- t(
    apply(model1.logdiff.forecast @ fseries,
          1,
          backtransform,
          difflogdata=difflogdata,
          init=init)
)


plot_forecast(data=stocks, trueobs=trueobs, n.ahead=27, ylim=400,
              forecasted_model=model1.logdiff.forecast, modeltxt=paste(
                  "TGARCH(", best_choice_std$aic_param[1], ", ",
                  best_choice_std$aic_param[2], ")", sep="")
              )

# <<<<< Model 1 : T-GARCH


# >>>> UNUSED CODE
model1.notrans <- ugarchfit(
    spec=ugarchspec(
        variance.model = list(garchOrder=c(3,1)),
        distribution.model = "std"
    ),
    data=stocks
)
plot(model1.notrans, which="all")

pred1.notrans <- ugarchforecast(model1.notrans, n.ahead = 27)
plot(pred1.notrans, which=1)

# <<<<< UNUSED CODE
