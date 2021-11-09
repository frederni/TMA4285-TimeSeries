# Project part 2
install.packages("rugarch")

library(rugarch)

# Preprocessing and data cleaning
data = read.csv('../Tidsrekker/CBLTCUSD.csv')
stocks <- as.numeric(data[, 2]) # Get the stock values only and convert to numeric
stocks <- na.omit(stocks) # Remove invalid stock prices

for(p in 0:10){
    for(q in 0:10){
        # TODO Check AIC/BIC for each combination
    }
}

# Try using rugarch
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



