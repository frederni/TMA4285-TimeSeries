# Project part 2
# install.packages("fGarch")

library(fGarch)

# Preprocessing and data cleaning
data = read.csv('../Tidsrekker/CBLTCUSD.csv')
stocks <- as.numeric(data[, 2]) # Get the stock values only and convert to numeric
stocks <- na.omit(stocks) # Remove invalid stock prices


# Fit with GARCH(1,1) because that's the default
fit <- garchFit(formula= ~garch(1,1), data=stocks, cond.dist="std")
predict(fit, n.ahead = 10)
