library(fOptions)
c=GBSOption(TypeFlag = "c",S=3295.47,X=3300,r=0.01,Time=0.08333,sigma = 0.22, b=-0.01)@price
p=GBSOption(TypeFlag = "p",S=3295.47,X=3300,r=0.01,Time=0.08333,sigma = 0.22, b=-0.01)@price
value=-(c+p)*6000
?GBSOption
cg<-sapply(c('delta','gamma','theta','rho','vega'), function(greek){GBSGreeks(Selection = greek,TypeFlag = "c",S=3295.47,X=3300,r=0.01,Time=0.08333,sigma = 0.22, b=-0.01)})
pg<-sapply(c('delta','gamma','theta','rho','vega'), function(greek){GBSGreeks(Selection = greek,TypeFlag = "p",S=3295.47,X=3300,r=0.01,Time=0.08333,sigma = 0.22, b=-0.01)})
delta=(-as.numeric(cg['delta'])-as.numeric(pg['delta']))*6000
gamma=(-as.numeric(cg['gamma'])-as.numeric(pg['gamma']))*6000
theta=(-as.numeric(cg['theta'])-as.numeric(pg['theta']))*6000
vega=(-as.numeric(cg['vega'])-as.numeric(pg['vega']))*6000

#2
setwd("C:/Users/HP/Desktop/Lesson/567 Risk Management/HW/HW2")
data <- read.csv("Data.csv")
library(readxl)
msft<-subset(data,TICKER=='MSFT',select=c(RET))
aapl<-subset(data,TICKER=='AAPL',select=c(RET))
spy<-subset(data,TICKER=='SPY',select=c(RET))
xlf<-subset(data,TICKER=='XLF',select=c(RET))
date<-subset(data,TICKER=='MSFT',select=c(date))
df<-data.frame(date,msft,aapl,spy,xlf)
colnames(df) <- c('date','msft','aapl','spy','xlf')
View(df)

#3(a)
typeof(msft)
mean <- apply(df[,2:5], 2, mean)
sd<-apply(df[,2:5], 2, sd)
q<-sapply(c(quantile(df[,2],.05),quantile(df[,3],.05),quantile(df[,4],.05),quantile(df[,5],.05))
typeof(q)

#3(b)
df[,'date']<-as.character(df[,'date'])
df[,'date']<-as.Date(df[,'date'],"%Y%m%d")
typeof(df[,'date'])
df08<-subset(df,format(df[,'date'],format = "%Y")=="2008")
mean08 <- apply(df08[,2:5], 2, mean)
sd08<-apply(df08[,2:5], 2, sd)
q08<-c(quantile(df08[,2],.05),quantile(df08[,3],.05),quantile(df08[,4],.05),quantile(df08[,5],.05))

#4
df[,'return']<-1000000*df[,'aapl']+1000000*df[,'msft']+3000000*df[,'xlf']+5000000*df[,'spy']
r0809<-subset(df,format(df[,'date'],format = "%Y")=="2008"|format(df[,'date'],format = "%Y")=="2009",select=c(date,return))
VaRh <- function(day){
  index <- which(df[,'date'] == day)
  return <- df$return[(index-999) : index]  
  VaR = - quantile(return, 0.01) 
  return (VaR)  
}
for(i in r0809$date)
  r0809$varh[which(r0809[,'date'] == i)]<-VaRh(i)

#5
VaRd <- function(day){
  index <- which(df[,'date'] == day)
  return <- df$return[(index-999) : index] 
  mean = mean(return)
  std = sd(return)
  VaR = - (mean - 2.33*std)
  return (VaR) 
}
for(i in r0809$date)
  r0809$vard[which(r0809[,'date'] == i)]<-VaRd(i)

#6
VaRd_w <- function(day){
  index <- which(df[,'date'] == day)
  l <- 0.94
  {
    return <- df$return[(index-999) : index]  
    mean = mean(return)
    var <- 0
    for(i in 0:999){
      var <- var + (return[1000-i] * return[1000-i] * (l^i))
    }
    std = sqrt((1-l)*var)
  }
  VaR = - (mean - 2.33*std)
  return (VaR) 
}
for(i in r0809$date)
  r0809$vard_w[which(r0809[,'date'] == i)]<-VaRd_w(i)

#7
VaRh_w <- function(day){
  index <- which(df[,'date'] == day)
  y <- 0.995
  confidence <- 0.99
  return <- df$return[(index-999) : index] 
  w <- c()
  for(i in 0:999)
    {
      w <- c(w, (y^(999-i))*(1 - y)/(1 - y^1000) )
    }
  return_w <- data.frame(return, w)
  return_w <- return_w[order(return_w$return, decreasing = F),]
  sum_w <- 0
  for(i in 1:1000)
    {
      sum_w <- sum_w + return_w$w[i]
      if(sum_w > (1-confidence) )
        {
          VaR = - return_w$return[i] 
          break
        } 
    }
  return(VaR)
}
for(i in r0809$date)
  r0809$varh_w[which(r0809[,'date'] == i)]<-VaRh_w(i)

plot(r0809$date,r0809$varh,type="l", col="red",ylim=c(0, 1300000),xlab="Date", ylab = "VaR")
lines(r0809$date,r0809$vard,type="l", col="blue")
lines(r0809$date,r0809$vard_w,type="l", col="yellow")
lines(r0809$date,r0809$varh_w,type="l", col="green")
legend( "topright",c("HS", "DN", "DN_W", "HS_W"), fill=c("red", "blue", "yellow", "green"))

  
  
  