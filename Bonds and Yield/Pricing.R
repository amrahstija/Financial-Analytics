# Feedback from the last assgn 1
# Give Manegerial Insights
# Return on VIX does not make any sense, only
# change in VIX should have been incorporated

#********** Pricing of bonds ***********#

#         Loading the Rates Data        #
setwd("F:/Semester 8/Financial Analytics Lab/ZCYC_NSS_10012018")
rates = read.csv('ZCYC_Rates.csv', header = TRUE)
colnames(rates) = c('maturity', 'rate')

# Bonds data
# Bond Indentifier: GS CG2033 IN0020160100
# Issue Date:     5 Dec 2016
# Maturity Date:  5 Dec 2033
# Last IP Date:   5 Dec 2017
# Next IP Date:   5 Jun 2018
# Frequency:      Half Yearly

# Total Number of Payments = (2033 - 2017)X 2 = 32

# We shall calculate the Price on the Next IP date

FV = 100
coupon = 6.57/2.0
N = 32
daysRemaining = 144
#we shall take 360days convention

price = 0

for (t in 1:(N-1)){
  r1 = rates[t,2]
  r2 = rates[t+1,2]
  r = r1 + (r2 - r1)*(144/180)
  print(r)
  discountPeriod = t -1 + (144/180)
  discountFactor = (1 + r/200)^(-discountPeriod)
  print(discountFactor)
  price = price + coupon*discountFactor
  print(price)
  
  if (t == (N-1)){
    price = price + FV*discountFactor
  }
}

print(price)

# function to calculate the price of a bond
# Inputs: 
#       FV  Face Value
#       C   Coupon rate in percentage Points
#       r   Discounting Rate
#       w   period till the first payment
#       N   Count of payments
#       f   frequency of payment eg. 2 for half- yearly

# function to calculate the price of a bond
bondPrice = function(FV, C, r, w, N,f){
  price = 0
  for (i in 0:(N-1)){
    price = price + (C*FV/f)*((1 + (r/f))^(-(w + i)))
  }
  price = price + FV*((1 + (r/f))^(-(w + N - 1)))
  return(price)
}

# function to calculate the derivative of the price function
bondPriceDerivative = function(FV, C, r, w, N,f){
  p_dash = 0
  for (i in 0:(N-1)){
    p_dash = p_dash + (C*FV/f)*(-(i + w))*(1/f)*((1 + (r/f))^(-(i + 1 + w)))
  }
  p_dash = p_dash + FV*(-(N - 1 + w))*(1/f)*((1 + (r/f))^(-(N + w)))  
  return(p_dash)
  }

#calculation of the YTM of the bond using the Newton Raphson Method

#input values
r0 = 0.10           #initial rate to start with
error = 0.0001      #price error tolerance
BondPrice = 90.76   #requested price to calculate YTM for
FV = 100           
coupon = 0.0657
w = 144/180
N = 32
f = 2


r_old = r0
p_cal = 100
while (abs(BondPrice - p_cal) > error){
  p_cal = bondPrice(FV, coupon, r_old, w, N, f)
  p_dash = bondPriceDerivative(FV, coupon, r_old, w, N, f)
  r_new = r_old  - (p_cal - BondPrice)/p_dash
  r_old = r_new
  print(r_new)
}





















