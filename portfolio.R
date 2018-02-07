library(matlib)
# Import the stock Price data for the year 2017
prices = read.csv('F:/QuantFin/NSEdata.csv')
prices = prices[,c(1,6,13,20,27,34,41,48,55,62)]
colnames(prices) = c('Date','BhartiAirtel', 'TataMotors', 'HindPetro', 'InfraTel',
                     'Bosch','PowerGrid', 'CoalIndia', 'SunPharma', 'ITC')

# Preparing the returns series of each stock
returns = as.data.frame(diff(log(prices$BhartiAirtel)))
names(returns) = 'BhartiAirtel'
returns$TataMotors = diff(log(prices$TataMotors))
returns$HindPetro = diff(log(prices$HindPetro))
returns$Infratel = diff(log(prices$InfraTel))
returns$Bosch = diff(log(prices$Bosch))
returns$PowerGrid = diff(log(prices$PowerGrid))
returns$CoalIndia = diff(log(prices$CoalIndia))
returns$SunPharma = diff(log(prices$SunPharma))
returns$ITC = diff(log(prices$ITC))

# returns that we have obtained are the daily returns, thus if we compute the mean
# and variance-covariance matrix we would get it for daily values and it would require
# us to scale to annual values so we shall do that in the beginning itself

returns = returns*252 #making it annualized

# computation of the variance-Covariance Matrix of the portfolio Stocks
covMatrix = cov(returns)

# computation of the Correlation Matrix
corMatrix = cov2cor(covMatrix)

# the Variance-Covarince Matrix must be positive-Definite or 
# in general semi-positive definite, this can be tested if it equals its transpose

t(covMatrix) == covMatrix
# we see that all of its throws true for all columns

# Another, method to test is if all the eigenvalues of covMatrix are positive
eig = eigen(covMatrix)
eigenValues = eig$values
# All eigenValues are positive

# Let's calculate the annual returns of each stock
StockMeans = as.matrix(apply(returns, 2, function(x) mean(x)))

# Let us assign some random weight to the portfolio of these 9 stocks
PortfolioWeight = as.matrix(rep(1/9, 9))

# portfolio returns
PortfolioReturns = t(StockMeans)%*%PortfolioWeight

#Calculating Portfolio Variance
PortfolioVariance = t(PortfolioWeight)%*%covMatrix%*%PortfolioWeight

### Estimation of Minimum Variance Portfolio ###
unitMatrix = as.matrix(rep(1,9))
lambda = 2/(t(unitMatrix)%*%inv(covMatrix)%*%unitMatrix)

MinVarWeights = (as.numeric(lambda)/2)*(inv(covMatrix)%*%unitMatrix)
MinVarPortfolioVariance = t(MinVarWeights)%*%covMatrix%*%MinVarWeights


