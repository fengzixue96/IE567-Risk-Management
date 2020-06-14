#1(a)
library(fOptions)
c_ABC=CRRBinomialTreeOption(TypeFlag = "ce",S=101.17,X=100,r=0.01,Time=0.08333,sigma = 0.45, b=0.01,n=1000)@price
p_ABC=CRRBinomialTreeOption(TypeFlag = "pe",S=101.17,X=100,r=0.01,Time=0.08333,sigma = 0.45, b=0.01,n=1000)@price
c_DEF=CRRBinomialTreeOption(TypeFlag = "ce",S=148.97,X=150,r=0.01,Time=0.08333,sigma = 0.37, b=0.01,n=1000)@price
p_DEF=CRRBinomialTreeOption(TypeFlag = "pe",S=148.97,X=150,r=0.01,Time=0.08333,sigma = 0.37, b=0.01,n=1000)@price
value=-(c_ABC+p_ABC)*6000-(c_DEF+p_DEF)*4000

#1(b)
library(MASS)
sigma <- matrix(c(0.028^2,0.028*0.023*0.4,0.028*0.023*0.4,0.023^2),2,2)
sigma
mydata<-mvrnorm(n=1000, c(0.0005, 0.0004), sigma)
mydata <- as.data.frame(mydata)
mc=list()
for(i in 1:1000)
{  
  c1=CRRBinomialTreeOption(TypeFlag = "ce",S=(mydata[i,1]+1)*101.17,X=100,r=0.01,Time=0.08333-1/252,sigma = 0.45, b=0.01,n=1000)@price
  p1=CRRBinomialTreeOption(TypeFlag = "pe",S=(mydata[i,1]+1)*101.17,X=100,r=0.01,Time=0.08333-1/252,sigma = 0.45, b=0.01,n=1000)@price
  c2=CRRBinomialTreeOption(TypeFlag = "ce",S=(mydata[i,2]+1)*148.97,X=150,r=0.01,Time=0.08333-1/252,sigma = 0.37, b=0.01,n=1000)@price
  p2=CRRBinomialTreeOption(TypeFlag = "pe",S=(mydata[i,2]+1)*148.97,X=150,r=0.01,Time=0.08333-1/252,sigma = 0.37, b=0.01,n=1000)@price
  val=-(c1+p1)*6000-(c2+p2)*4000
  mc[i]<-val-value
}
mc<-as.data.frame(mc)
VaR1=-quantile(mc,.01)

#1(c)
hist(as.numeric(mc),breaks=50,xlab="Profit/Loss", ylab = "Number of Realizations",main ="VaR by MC Normal")

#2
sigma21 <- matrix(c(0.028^2*21,0.028*0.023*0.4*21,0.028*0.023*0.4*21,0.023^2*21),2,2)
mydata21 <-mvrnorm(n=1000, c(0.0005*21, 0.0004*21), sigma21)
mydata21 <- as.data.frame(mydata21)
mc21 =list()
for(i in 1:1000)
{  
  c1=max((mydata21[i,1]+1)*101.17-100,0)
  p1=max(100-(mydata21[i,1]+1)*101.17,0)
  c2=max((mydata21[i,2]+1)*148.97-150,0)
  p2=max(150-(mydata21[i,2]+1)*148.97,0)
  val=-(c1+p1)*6000-(c2+p2)*4000
  mc21[i]<-val-value
}
mc21<-as.data.frame(mc21)
VaR2=-quantile(mc21,.01)
hist(as.numeric(mc21),breaks=50,xlab="Profit/Loss", ylab = "Number of Realizations",main ="VaR by MC Normal (21 days)")

#3(a)
install.packages("mvtnorm")
library(mvtnorm)
mydatat <- rmvt(n=1000, sigma*0.5, df=4, c(0.0005,0.0004))
mydatat <- as.data.frame(mydatat)
mct =list()
for(i in 1:1000)
{  
  c1=CRRBinomialTreeOption(TypeFlag = "ce",S=(mydatat[i,1]+1)*101.17,X=100,r=0.01,Time=0.08333-1/252,sigma = 0.45, b=0.01,n=1000)@price
  p1=CRRBinomialTreeOption(TypeFlag = "pe",S=(mydatat[i,1]+1)*101.17,X=100,r=0.01,Time=0.08333-1/252,sigma = 0.45, b=0.01,n=1000)@price
  c2=CRRBinomialTreeOption(TypeFlag = "ce",S=(mydatat[i,2]+1)*148.97,X=150,r=0.01,Time=0.08333-1/252,sigma = 0.37, b=0.01,n=1000)@price
  p2=CRRBinomialTreeOption(TypeFlag = "pe",S=(mydatat[i,2]+1)*148.97,X=150,r=0.01,Time=0.08333-1/252,sigma = 0.37, b=0.01,n=1000)@price
  val=-(c1+p1)*6000-(c2+p2)*4000
  mct[i]<-val-value
}
mct<-as.data.frame(mct)
VaRt=-quantile(mct,.01)

#3(b)
hist(as.numeric(mct),breaks=50,xlab="Profit/Loss", ylab = "Number of Realizations",main ="VaR by MC t_distribution")


