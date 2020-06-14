#1(a)
library(fOptions)
c_ABC=CRRBinomialTreeOption(TypeFlag = "ce",S=101.17,X=100,r=0.01,Time=21/252,sigma = 0.45, b=0.01,n=1000)@price
p_ABC=CRRBinomialTreeOption(TypeFlag = "pe",S=101.17,X=100,r=0.01,Time=21/252,sigma = 0.45, b=0.01,n=1000)@price
c_DEF=CRRBinomialTreeOption(TypeFlag = "ce",S=148.97,X=150,r=0.01,Time=21/252,sigma = 0.37, b=0.01,n=1000)@price
p_DEF=CRRBinomialTreeOption(TypeFlag = "pe",S=148.97,X=150,r=0.01,Time=21/252,sigma = 0.37, b=0.01,n=1000)@price
value=-(c_ABC+p_ABC)*6000-(c_DEF+p_DEF)*4000
library(MASS)
sigma <- matrix(c(0.028^2,0.028*0.023*0.4,0.028*0.023*0.4,0.023^2),2,2)
sigma
mydata<-mvrnorm(n=1000, c(0.0005, 0.0004), sigma)
mydata <- as.data.frame(mydata)
mc=list()
for(i in 1:1000)
{  
  c1=CRRBinomialTreeOption(TypeFlag = "ce",S=exp(mydata[i,1])*101.17,X=100,r=0.01,Time=21/252-1/252,sigma = 0.45, b=0.01,n=1000)@price
  p1=CRRBinomialTreeOption(TypeFlag = "pe",S=exp(mydata[i,1])*101.17,X=100,r=0.01,Time=21/252-1/252,sigma = 0.45, b=0.01,n=1000)@price
  c2=CRRBinomialTreeOption(TypeFlag = "ce",S=exp(mydata[i,2])*148.97,X=150,r=0.01,Time=21/252-1/252,sigma = 0.37, b=0.01,n=1000)@price
  p2=CRRBinomialTreeOption(TypeFlag = "pe",S=exp(mydata[i,2])*148.97,X=150,r=0.01,Time=21/252-1/252,sigma = 0.37, b=0.01,n=1000)@price
  val=-(c1+p1)*6000-(c2+p2)*4000
  mc[i]<-val-value
}
mc<-as.data.frame(mc)
VaR1=-quantile(mc,.05)

#1(b)
S<-0
n<-0
for(i in 1:1000)
{
  if(mc[i]<(-VaR1))
  {
    S=S+mc[i]
    n=n+1
  }
}
ES=-S/n

#2(a)
install.packages("mvtnorm")
library(mvtnorm)
mydatat <- rmvt(n=1000, sigma*0.5, df=4, c(0.0005,0.0004))
mydatat <- as.data.frame(mydatat)
mct =list()
for(i in 1:1000)
{  
  c1=CRRBinomialTreeOption(TypeFlag = "ce",S=exp(mydatat[i,1])*101.17,X=100,r=0.01,Time=21/252-1/252,sigma = 0.45, b=0.01,n=1000)@price
  p1=CRRBinomialTreeOption(TypeFlag = "pe",S=exp(mydatat[i,1])*101.17,X=100,r=0.01,Time=21/252-1/252,sigma = 0.45, b=0.01,n=1000)@price
  c2=CRRBinomialTreeOption(TypeFlag = "ce",S=exp(mydatat[i,2])*148.97,X=150,r=0.01,Time=21/252-1/252,sigma = 0.37, b=0.01,n=1000)@price
  p2=CRRBinomialTreeOption(TypeFlag = "pe",S=exp(mydatat[i,2])*148.97,X=150,r=0.01,Time=21/252-1/252,sigma = 0.37, b=0.01,n=1000)@price
  val=-(c1+p1)*6000-(c2+p2)*4000
  mct[i]<-val-value
}
mct<-as.data.frame(mct)
VaRt=-quantile(mct,.05)

#2(b)
S_t<-0
n_t<-0
for(i in 1:1000)
{
  if(mct[i]<(-VaRt))
  {
    S_t=S_t+mct[i]
    n_t=n_t+1
  }
}
ES_t=-S_t/n_t

#3(a)
u<-0.02
kesai<-0.36
beta<-0.008
p<-250/8000
VaRe<-0.03
alpha=p*((1+kesai*(VaRe-u)/beta)^(-1/kesai))

#3(b)
E=0.016*2-0.012
