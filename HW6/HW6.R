library(tseries)
library(fGarch)
data <- read.csv("C:/Users/HP/Desktop/Lesson/567 Risk Management/HW/HW5/HW5.data.csv", skip = 1)

#1(a)
r<- diff(log(data$Close))

garch112 <- function(x) {  
  sigma = rep(0,length(r))
  sigma[1] = x[4]^2 
  for (i in 1:(length(r)-1)) {
      sigma[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*r[i]^2+x[2]*sigma[i]
    }
  f <-(1/(sqrt(2*pi*sigma)))*exp(-0.5*r^2/sigma)
  negL = -sum(log(f))
  return(negL)
}
guess1 = c(0.1,0.8,0.01,0.01)
result1 = optim(guess1,garch112)
alpha1 = result1$par[1]
beta1 = result1$par[2]
var1 = result1$par[3]
sigma1 = result1$par[4]

#1(b)
var = sum(r^2)/length(r)
std = sqrt(var)
garch112 <- function(x) {  
  sigma = rep(0,length(r))
  sigma[1] = x[3]^2 
  for (i in 1:(length(r)-1)) {
    sigma[i+1] = (1-x[1]-x[2])*var+x[1]*r[i]^2+x[2]*sigma[i]
  }
  f <-(1/(sqrt(2*pi*sigma)))*exp(-0.5*r^2/sigma)
  negL = -sum(log(f))
  return(negL)
}
guess2 = c(0.1,0.8,0.01)
result2 = optim(guess2,garch112)
alpha2 = result2$par[1]
beta2 = result2$par[2]
sigma2 = result2$par[3]

#1(c)
garch113 <- function(x) {  
  sigma = rep(0,length(r))
  sigma[1] = var 
  for (i in 1:(length(r)-1)) {
    sigma[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*r[i]^2+x[2]*sigma[i]
  }
  f <-(1/(sqrt(2*pi*sigma)))*exp(-0.5*r^2/sigma)
  negL = -sum(log(f))
  return(negL)
}
guess3 = c(0.1,0.8,0.01)
result3 = optim(guess3,garch113)
alpha3 = result3$par[1]
beta3 = result3$par[2]
var3 = result3$par[3]

#1(d)
garch114 <- function(x) {  
  sigma = rep(0,length(r))
  sigma[1] = var 
  for (i in 1:(length(r)-1)) {
    sigma[i+1] = (1-x[1]-x[2])*var+x[1]*r[i]^2+x[2]*sigma[i]
  }
  f <-(1/(sqrt(2*pi*sigma)))*exp(-0.5*r^2/sigma)
  negL = -sum(log(f))
  return(negL)
}
guess4 = c(0.1,0.8)
result4 = optim(guess4,garch114)
alpha4 = result4$par[1]
beta4 = result4$par[2]

#1(e)
sigmaa = rep(0,length(r))
sigmaa[1] = sigma1^2
for (i in 1:(length(r))) {
  sigmaa[i+1] = (1-alpha1-beta1)*var1^2+alpha1*r[i]^2+beta1*sigmaa[i]
}
e1 = sqrt(sigmaa[length(r)+1])

sigmab = rep(0,length(r))
sigmab[1] = sigma2^2 
for (i in 1:(length(r))) {
  sigmab[i+1] = (1-alpha2-beta2)*var+alpha2*r[i]^2+beta2*sigmab[i]
}
e2 = sqrt(sigmab[length(r)+1])

sigmac = rep(0,length(r))
sigmac[1] = var
for (i in 1:(length(r))) {
  sigmac[i+1] = (1-alpha3-beta3)*var3^2+alpha3*r[i]^2+beta3*sigmac[i]
}
e3 = sqrt(sigmac[length(r)+1])

sigmad = rep(0,length(r))
sigmad[1] = var
for (i in 1:(length(r))) {
  sigmad[i+1] = (1-alpha4-beta4)*var+alpha4*r[i]^2+beta4*sigmad[i]
}
e4 = sqrt(sigmad[length(r)+1])


#2(a)
garchforecast <- function(x) {  
  day = x[5]
  sigmaf = rep(0,day)
  sigmaf[1] = x[4]
  for (i in 2:day) {
    sigmaf[i] = x[3] + (x[1]+x[2])^(day-1) * (sigmaf[1]-x[3])
  }
  forecast = sum(sigmaf)
  return(forecast)
}

input = c(alpha1,beta1,var1^2,e1^2,21)
rv1 = garchforecast(input)
input = c(alpha2,beta2,var,e2^2,21)
rv2 = garchforecast(input)
input = c(alpha3,beta3,var3^2,e3^2,21)
rv3 = garchforecast(input)
input = c(alpha4,beta4,var,e4^2,21)
rv4 = garchforecast(input)

#2(b)
sq1 = sqrt(rv1)
sqy1 = (252/21)^(1/2)*sq1

sq2 = sqrt(rv2)
sqy2 = (252/21)^(1/2)*sq2

sq3 = sqrt(rv3)
sqy3 = (252/21)^(1/2)*sq3

sq4 = sqrt(rv4)
sqy4 = (252/21)^(1/2)*sq4

#3
garch_a = garch(ts(r),order = c(1,1))
omega_a <- coef(garch_a)[1]
alpha_a <- coef(garch_a)[2]
beta_a <- coef(garch_a)[3]
sigma_a <- sqrt(omega_a/(1-alpha_a-beta_a))

garch_b = garchFit(~ garch(1,1), data = r, include.mean = FALSE)
omega_b <- coef(garch_b)[1]
alpha_b <- coef(garch_b)[2]
beta_b <- coef(garch_b)[3]
sigma_b <- sqrt(omega_b/(1-alpha_b-beta_b))

#4
ngarch11 <- function(x) {
  sigma = rep(0,length(r))
  sigma[1] = x[4]^2 
  for (i in 1:(length(r)-1)) {
      sigma[i+1] = (1-x[1]*(1+x[5]^2)-x[2])*x[3]^2+x[1]*(r[i]-x[5]*sqrt(sigma[i]))^2+x[2]*sigma[i]
    }
    f <-(1/(sqrt(2*pi*sigma)))*exp(-0.5*r^2/sigma)
    negL = -sum(log(f))
  return(negL)
}
guess5 = c(0.1,0.8,0.01,0.01,0)
result5 = optim(guess5,ngarch11)
alpha5 = result5$par[1]
beta5 = result5$par[2]
var5 = result5$par[3]
sigma5 = result5$par[4]
theta = result5$par[5]

#x[1] = alpha
#x[2] = beta
#x[5] = theta










