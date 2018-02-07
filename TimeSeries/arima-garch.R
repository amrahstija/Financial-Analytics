library(xts)
library(timeSeries)
library(tseries)
library(forecast)

#### Importing the data
setwd("F:/Semester 8/Financial Analytics Lab/Lab 3")
price = read.csv(file = "nifty.csv")
head(price)

### Subsetting of the to select only the dates and the closing price
price = price[, c(1,5)]

### Converting to the xts
price$Date = as.Date(price$Date, format= "%d-%b-%Y")

price = as.xts(price$Close, order.by = price$Date)

plot(price) ## the plot of the stock price shows the trending time series therefore it's not stationary


### Finding the log return of the time series
logret = diff(log(price))
plot(logret)
logret = na.omit(logret)

### Checking the stationarity of the time series with Augumented Dickey Fuller Test
adf.test(logret, alternative = "stationary", k =20)

### Finding the ACF of the time series
acf(logret, main="Return Series")
acf(logret^2, main="Return Square Series")
pacf(logret)


## Training a ARIMA model
retMod = auto.arima(logret, ic=c("aic"), trace = T)
summary(retMod)
priceMod = auto.arima(price, ic=c("aic"), trace=T)
summary(priceMod)

plot(logret, type="l")
lines(fitted(retMod), col="red")

plot(price, type="l")
lines(fitted(priceMod), col="red")

## Forecasting 
retfc = forecast(retMod, h=10, level=0.99)
summary(retfc)
plot(retfc)

pricefc = forecast(priceMod, h=10, level = 0.99)
summary(pricefc)
plot(pricefc)


### Testing the residual of the model
Box.test(retMod$residuals, lag = 20, type = "Ljung-Box")
## Residual of not correlated



manRes = logret - mean(logret)
yest = logret


coeff = retMod$coef
for(i in 1:length(logret)){
  if(i == 1){
    yest[i] = coeff[2] + coeff[1]*(manRes[i])
  }else{
    manRes[i-1] = logret[i-1] - yest[i-1]
    yest[i] = coeff[2] + coeff[1]*manRes[i-1]
  }
}

library(ggplot2)

plot(manRes) 
#lines(retMod$residuals, col="red", add=T)
plot(retMod$residuals)



### GARCH modelling
Box.test((retMod$residuals)^2, lag = 20, type = "Ljung-Box")
library(rugarch)

myspec = ugarchspec(mean.model = list(armaOrder = c(0,1)) , variance.model = list(garchOrder = c(1,1)))

model = ugarchfit(spec = myspec, data = logret)

infocriteria(model)[1]

## choosing the best GARCH in mean model

bestGarch = function(dataSeries, p, q, m, n){
  AICMatrix = 1:(m*n)
  dim(AICMatrix) = c(m,n)
  for (i in 1:m){
    for (j in 1:n){
      myspec = ugarchspec(mean.model = list(armaOrder = c(p,q)),
                          variance.model = list(garchOrder =c(i,j)))
      model = ugarchfit(spec = myspec, data = dataSeries)
      AICMatrix[i, j] = infocriteria(model)[1]
    }
  }
  minAIC.index = which(AICMatrix == min(AICMatrix), arr.ind = TRUE)
  cat(paste('The best GARCH model for the given series is', minAIC.index[1],minAIC.index[2]))
}

bestGarch(logret,0,1,3,3)
