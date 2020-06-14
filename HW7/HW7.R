setwd("C:/Users/HP/Desktop/Lesson/567 Risk Management/HW/HW7")
data <- read.csv("data.csv")
library(readxl)
library(lubridate)
data<-subset(data,SYM_SUFFIX==''&TR_SCOND=='@'&TR_CORR == 0)
typeof(data['TIME_M'])
data$TIME_M<-paste0(as.character(hms(data$TIME_M)$hour),":",as.character(hms(data$TIME_M)$minute+1),":00")
data$DATE<-as.Date(as.character(data$DATE),"%Y%m%d")
data$TIME<-paste0(data$DATE," ",as.character(hms(data$TIME_M)$hour),":",as.character(hms(data$TIME_M)$minute+1),":00")
index<-duplicated(data$TIME)
i<-which(!index)-1
#erase the first/add the last
i<-tail(i,-1)
data<-data[i,]

#1
index1=which(!(hms(data$TIME_M)$hour==9&hms(data$TIME_M)$minute<45))
data1<-data[index1,]
RV_op<-c()
r<-c()
for (i in 1:31) 
{
  a<-which(day(data1$DATE)==i)
  data_z<-data1[a,]
  r<-diff(log(data_z$PRICE))
  RV_op[i]<-sum(r^2)
}
mean_RV_op<-sum(RV_op)/20

#2
#15min(last 9)
RV_15<-c()
for (i in 1:31) 
{
  r_2<-rep(0,15)
  a<-which(day(data1$DATE)==i)
  data_z<-data1[a,]
  r<-diff(log(data_z$PRICE),lag = 15)
  if(length(r)!=0)
  {
    for(j in 0:as.integer(length(r)/15))
    {
      r_2[1]<-r[1+j*15]^2+r_2[1]
    }
    for(h in 2:15)
    {
      for(j in 0:(as.integer(length(r)/15)-1))
      {
        r_2[h]<-r[h+j*15]^2+r_2[h]
      }
      r_2[h]<-(as.integer(length(r)/15)+1)/as.integer(length(r)/15)*r_2[h]
    }
    RV_15[i]<-mean(r_2)
  }
}
RV_15<-na.omit(RV_15)
mean_RV_15<-sum(RV_15)/20

#10min(last 4)
RV_10<-c()
for (i in 1:31) 
{
  r_2<-rep(0,10)
  a<-which(day(data1$DATE)==i)
  data_z<-data1[a,]
  r<-diff(log(data_z$PRICE),lag = 10)
  if(length(r)!=0)
  {
    for(h in 1:6)
    {
      for(j in 0:as.integer(length(r)/10))
      {
        r_2[h]<-r[h+j*10]^2+r_2[h]
      }
    }
    for(h in 7:10)
    {
      for(j in 0:(as.integer(length(r)/10)-1))
      {
        r_2[h]<-r[h+j*10]^2+r_2[h]
      }
      r_2[h]<-(as.integer(length(r)/10)+1)/as.integer(length(r)/10)*r_2[h]
    }
    RV_10[i]<-mean(r_2)
  }
}
RV_10<-na.omit(RV_10)
mean_RV_10<-sum(RV_10)/20

#5min(last 4)
RV_5<-c()
for (i in 1:31) 
{
  r_2<-rep(0,5)
  a<-which(day(data1$DATE)==i)
  data_z<-data1[a,]
  r<-diff(log(data_z$PRICE),lag = 5)
  if(length(r)!=0)
  {
    for(j in 0:as.integer(length(r)/5))
    {
      r_2[1]<-r[1+j*5]^2+r_2[1]
    }
    for(h in 2:5)
    {
      for(j in 0:(as.integer(length(r)/5)-1))
      {
        r_2[h]<-r[h+j*5]^2+r_2[h]
      }
      r_2[h]<-(as.integer(length(r)/5)+1)/as.integer(length(r)/5)*r_2[h]
    }
    RV_5[i]<-mean(r_2)
  }
}
RV_5<-na.omit(RV_5)
mean_RV_5<-sum(RV_5)/20

#2min(no last)
RV_2<-c()
for (i in 1:31) 
{
  r_2<-rep(0,2)
  a<-which(day(data1$DATE)==i)
  data_z<-data1[a,]
  r<-diff(log(data_z$PRICE),lag = 2)
  if(length(r)!=0)
  {
    for(h in 1:2)
    {
      for(j in 0:(as.integer(length(r)/2)-1))
      {
        r_2[h]<-r[h+j*2]^2+r_2[h]
      }
    }
    RV_2[i]<-mean(r_2)
  }
}
RV_2<-na.omit(RV_2)
mean_RV_2<-sum(RV_2)/20

#ratio
Ratio_15_1<-mean_RV_15/mean_RV_op
Ratio_15_10<-mean_RV_15/mean_RV_10

#3
print(acf(RV_15))

#4
jpm <- read.csv("JPM.csv")
close<-jpm$Adj.Close
return<-diff(log(close))
RV_24h1<-mean(sum(return^2)/sum(RV_15)*RV_15)

open<-jpm$Open
open<-tail(open,-1)
close<-head(close,-1)
RV_24h2<-mean(log(open/close)^2+RV_15)
