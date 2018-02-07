library(xts)
library(timeSeries)
library(tseries)
library(forecast)
library(vars)
library(urca)

price = read.csv('VECM.csv')

price$Date = as.Date(price$Date, format= "%d-%b-%y")
price = as.xts(price$Close, order.by = price$Date)
plot(price)

spot.ret = diff(log(price$Spot))
adf.test(spot.ret, k = 1)

futures.ret = diff(log(price$Futures))
adf.test(futures.ret, k = 1)

data = as.data.frame(cbind(spot.ret, futures.ret))
mlag = VARselect(data, lag.max = 10, type = 'both')
mlag

var.model = VAR(data, p = mlag$selection[1], type = 'both')
var.model
# Causality runs from from futures to spot but not from spot to futures

predict(var.model)
covvMat = summary(var.model)$covres
hedgeRatio = covvMat[1,2]/covvMat[2,2]

caModel.ret = ca.jo(data, type = c('eigen','trace'),
                ecdet = c('const'), 
                K = mlag$selection[1],
                spec = 'longrun')
caModel.price = ca.jo(price[,c(2,3)], type = c('eigen','trace'),
                    ecdet = c('const'), 
                    K = mlag$selection[1],
                    spec = 'longrun')
summary(caModel.price)
## at 5%, r = 0 is rejected, thus it says that cointegration exists. 
## if Ho was true then simply VAR model would have worked 

## at 5% r<= 1 is not rejected thus we say that there is one cointegration pair
## one long run cointegration relationship exist

vecmModel = cajorls(caModel.price, 1)
summary(vecmModel)
vecmModel
