data <- read.csv("C:/Users/HP/Desktop/Lesson/567 Risk Management/HW/HW5/HW5.data.csv", skip = 1)

#1
log_return<- diff(log(data$Close))
loss<- -log_return
u<- seq(0.01,0.05,by=0.002)
excess_loss<- c()
for (i in 1:length(u)) 
{
  excess_loss[i]<- mean(loss[loss>u[i]]-u[i], na.rm = TRUE)
}
plot(u,excess_loss,main="e(u)")

#2
length(loss[loss>=0.022])

#3 density function«Ûµº
el<- loss[loss>0.022]-0.022
p <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  y=0
  for(i in el)
  {
    yy<-log((1/x2)*(1+x1*(i/x2))^((-1/x1)-1))
    y=y+yy
  }
  return(-y)
}
result <- optim(par = c(0.1,0.1),fn=p)
x1<- result$par[1]
x2<- result$par[2]

#4
x<- seq(0.022,0.1,0.001)
plot(x,(1/x2)*(1+x1*(x-0.022)/x2)^((-1/x1)-1),type="l",xlab="loss",ylab="conditional density",main="Conditional Density Function")

#5
P_0.022<- length(loss[loss>=0.022])/length(loss)
P_0.05<- P_0.022*(1+x1*(0.05-0.022)/x2)^(-1/x1)
P_0.1<- P_0.022*(1+x1*(0.1-0.022)/x2)^(-1/x1)
plot(x,P_0.022*(1+x1*(x-0.022)/x2)^(-1/x1),type="l",xlab='loss',ylab="Probability",main="Estimated Probability")






