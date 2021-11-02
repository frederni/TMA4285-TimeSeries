# Project part 2
# install.packages("fGarch")

library(fGarch)

# Preprocessing and data cleaning
data = read.csv('../Tidsrekker/CBLTCUSD.csv')
stocks <- as.numeric(data[, 2]) # Get the stock values only and convert to numeric
stocks <- na.omit(stocks) # Remove invalid stock prices


# Fit with GARCH(1,1) because that's the default
fit <- garchFit(formula=~garch(2,2), data=stocks, cond.dist="std")
predict(fit, n.ahead = 10, plot=TRUE)

# TODO:
# 1) Determine why meanForecast is constant in the predict() call -- and fix
# 2) Find optimal p, q s.t. GARCH(p, q) has the best fit
# 3) Find out wether or not cond.dist="std" is sufficient for the student garch