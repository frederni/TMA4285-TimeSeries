# Project part 2
# install.packages("fGarch")

library(fGarch)

# Do everything in one line, maybe?
data = read.csv('../Tidsrekker/CBLTCUSD.csv')
stocks = cbind(data)[2] # Get the stock values only


garchFit(data=stocks)
