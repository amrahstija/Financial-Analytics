### OPTION PRICING USING MONTE CARLO SIMULATIONS ###
# @ Ajit Sharma {ajitsharma.iitkgp@gmail.com}

#*** Computation of Volatility using the NSE50 values for the last 1 year ***
nse.data = read.csv(file.choose(), header = TRUE)
returns = diff(log(nse.data$Close))
sigma = sd(returns)*sqrt(252)

s0 = 10530
nSim = 10000
nDiv = 5000
r = 0.0687
K = 11100
Time = 70/252
dt = Time/nDiv

call.payoff = 0
put.payoff = 0

for ( i in 1:nSim){
  #simulating the stock path in nDiv steps
  s = s0
  for (j in 2:nDiv){
    epsilon = rnorm(1,0,1)
    s[j] = s[j-1]*exp((r - 0.5*sigma^2)*dt + sigma*epsilon*sqrt(dt))
  }
  call.payoff[i] = max((s[length(s)] - K),0)
  put.payoff[i] = max((K - s[length(s)]),0)
  
  call.val[i] = mean(call.payoff[1:i])*exp(-(r*Time))
  put.val[i] = mean(put.payoff[1:i])*exp(-(r*Time))
}

plot(x = (1:nSim), y = call.val, type = 'l', main = 'Convergence of Call Price')
plot(x = (1:nSim), y = put.val, type = 'l', main = 'Convergence of Call Price')

call.val = mean(call.payoff)*exp(-(r*Time))
put.val = mean(put.payoff)*exp(-(r*Time))

### INCLUSION OF JUMPS IN DEFINING UNDERLYING PRICE###
price.increments = diff(log(nse.data$Close))
jumps = 0
for (i in 1:length(price.increments)){
  if ( abs(price.increments[i] >= 3*sd(returns))){
    jumps = jumps + 1
  }
}

price.increments = diff((nse.data$Close))
jumps = 0
for (i in 1:length(price.increments)){
  if ( abs(price.increments[i] >= 3*sd(diff(nse.data$Close)))){
    jumps = jumps + 1
  }
}

s0 = 10530
nSim = 1000
nDiv = 1000
r = 0.0687
K = 11100
Time = 70/252
dt = Time/nDiv

call.payoff = 0
put.payoff = 0
call.val = 0
put.val = 0

for ( i in 1:nSim){
  #simulating the stock path in nDiv steps
  s = s0
  for (j in 2:nDiv){
    epsilon = rnorm(1,0,1)
    a = min(price.increments)
    b = max(price.increments)
    N = 2
    jump.factor = 0
    for (k in 1:N){
      jump.factor = jump.factor + runif(N, min = a, max = b)
    }
    
    s[j] = s[j-1]*exp((r - 0.5*sigma^2)*dt + sigma*epsilon*sqrt(dt))+jump.factor
  }
  call.payoff[i] = max((s[length(s)] - K),0)
  put.payoff[i] = max((K - s[length(s)]),0)
  
  call.val[i] = mean(call.payoff[1:i])*exp(-(r*Time))
  put.val[i] = mean(put.payoff[1:i])*exp(-(r*Time))
}

plot(x = (1:nSim), y = call.val, type = 'l', main = 'Convergence of Call Price')
plot(x = (1:nSim), y = put.val, type = 'l', main = 'Convergence of Put Price')

#  put.val[length(put.val)]
#  435.3066
#  call.val[length(call.val)]
#  69.48701




