################################################################
######            FINANCIAL ANALYTICS LAB               ########
######           ASSIGNMENT-2  16 JAN 2018              ########
################################################################

###   OBJECTIVE : FINDING YIELD CURVES FOR CORPORATE BONDS   ###

# The Yield curve will be developed for 14Dec2017
security.master = read.csv('cbm_security_master.csv', header = TRUE)
bonds.data = read.csv('cbm_trd20171214.csv', header = TRUE)

#lets us check the column names of bond.data
names(bonds.data)

# we shall keep only the ISIN, Weighted Average Price and Weighted Average Yield
bonds.data = bonds.data[,c(2,7,8)]
colnames(bonds.data) = c('_ISIN','Price','YTM')

# lets us look into security master and keep only the relevant columns
names(security.master)
security.master = security.master[,c(3,8,9,12,13,14,24)]
colnames(security.master)[1] = 'Coupon.Rate'

bonds.data$ISIN = as.character(bonds.data$`_ISIN`)
for (i in 1:nrow(bonds.data)){
  x = bonds.data$ISIN[i]
  bonds.data$ISIN[i] = substr(x, 1, (nchar(x)-1))
}

# let us merge the two dataset using their ISIN
bonds.master = merge(bonds.data, security.master, by = 'ISIN')
bonds.master = bonds.master[,-2]

# convert dates into date types
bonds.master$Issue.Date = as.Date(bonds.master$Issue.Date, format = '%d-%b-%y')
bonds.master$Maturity.Date = as.Date(bonds.master$Maturity.Date, format = '%d-%b-%y')
bonds.master$Next.Coupon.Date = as.Date(bonds.master$Next.Coupon.Date, format = '%d-%b-%y')

# Calculate days count 
bonds.master$Days.Count = bonds.master$Next.Coupon.Date - as.Date('2017-12-14')
bonds.master = bonds.master[-40,]

for ( i in 1:nrow(bonds.master)){
  if (bonds.master$Coupon.Frequency[i] == 'Yearly'){
    bonds.master$Frequency[i] = 1
  }
  else if (bonds.master$Coupon.Frequency[i] == 'Half Yearly'){
    bonds.master$Frequency[i] = 2
    print(2)
  }
  else if (bonds.master$Coupon.Frequency[i] == 'Quarterly'){
    bonds.master$Frequency[i] = 4
  }
  
}

bonds.master$Coupon.Count = 1 + bonds.master$Frequency*floor((bonds.master$Maturity.Date - bonds.master$Next.Coupon.Date)/365)

for ( i in 1:nrow(bonds.master)){
  couponPeriod = floor(365/bonds.master$Frequency[i])
  if (bonds.master$Days.Count[i] > couponPeriod){
    bonds.master$Days.Count[i] = bonds.master$Days.Count[i] - couponPeriod
    bonds.master$Coupon.Count[i] = bonds.master$Coupon.Count[i] + 1
  }
}

# Cleaning the Coupon rate to convert to numeric
bonds.master$Coupon.Rate = as.character(bonds.master$Coupon.Rate)
for ( i in 1:nrow(bonds.master)){
  temp = (bonds.master$Coupon.Rate[i])
  bonds.master$Coupon.Rate[i]= as.numeric(substr(temp, 1, (nchar(temp)-1)))
}
bonds.master$Coupon.Rate = as.numeric(bonds.master$Coupon.Rate)

# some of the coupon rate were not present
bonds.master = bonds.master[complete.cases(bonds.master),]

# Computation of Accrued Interest
couponPeriod = floor(365/bonds.master$Frequency)
bonds.master$Accrued.Interest = bonds.master$Coupon.Rate*((couponPeriod - bonds.master$Days.Count)/couponPeriod)

#Computation of Dirty Price
bonds.master$Dirty.Price = bonds.master$Price + bonds.master$Accrued.Interest

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
bonds.master$YTM.Calc = 0
for (i in 1:nrow(bonds.master)){
  
  r0 = 0.10           #initial rate to start with
  error = 0.0001      #price error tolerance
  BondPrice = as.numeric(bonds.master$Dirty.Price[i])   #requested price to calculate YTM for
  FV = 100           
  coupon = bonds.master$Coupon.Rate[i]/100
  f = as.numeric(bonds.master$Frequency[i])
  w = as.numeric(bonds.master$Days.Count[i]*f/365)
  N = as.numeric(bonds.master$Coupon.Count[i])
  
  r_old = r0
  p_cal = 100
  while (abs(BondPrice - p_cal) > error){
    p_cal = bondPrice(FV, coupon, r_old, w, N, f)
    p_dash = bondPriceDerivative(FV, coupon, r_old, w, N, f)
    r_new = r_old  - (p_cal - BondPrice)/p_dash
    r_old = r_new
  }
  bonds.master$YTM.Calc[i] = r_new*100
}

bonds.master$Year.to.maturity = as.numeric((bonds.master$Maturity.Date - as.Date('2017-12-14'))/365)

rates = read.csv('rates.csv', header = TRUE)
colnames(rates) = c('YearsMaturity', 'Rate1', 'Rate')
rates = rates[,-2]
yrange = c(6,10)
plot(x = rates[1:33,]$YearsMaturity, y = rates[1:33,]$Rate, 
     type = 'l', col = 'green', lwd = 2, 
     xlab = 'Year to Maturity', ylab = 'YTM',
     ylim = yrange,
     main = 'Yield Curve of Corporate Bonds
     on 14 Dec 2017')
points(bonds.master$Year.to.maturity, bonds.master$YTM.Calc)
lines(lowess(bonds.master$YTM.Calc ~ bonds.master$Year.to.maturity, f = 0.6), col = 'red', lwd = 2)
legend("topright", c('ZCYC-NSS Curve', 'Corporate ZCYC'),  col = c('green', 'red'), lwd = 2)
