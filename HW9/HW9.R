#1
#a
u20 <- runif(20)
u8 <- runif(100)
t_d20 <- -log(1-u20)/0.012
t_d8 <- -log(1-u8)/0.012
loss20 <- c()
loss8 <- c()
for(i in 1:20)
{
  if(t_d20[i]<=1)
    loss20<-append(loss20, 20*0.5)
  else
    loss20<-append(loss20, 0)
}
for(i in 1:100)
{
  if(t_d8[i]<=1)
    loss8<-append(loss8, 8*0.5)
  else
    loss8<-append(loss8, 0)
}

#b
m<-rnorm(1)
z20<-rnorm(20)
z8<-rnorm(100)
z_20<-(1-0.3)^0.5*z20+0.3^0.5*m
z_8<-(1-0.3)^0.5*z8+0.3^0.5*m
p20<-pnorm(z_20)
p8<-pnorm(z_8)
t_d20 <- -log(1-p20)/0.012
t_d8 <- -log(1-p8)/0.012
loss20 <- c()
loss8 <- c()
for(i in 1:20)
{
  if(t_d20[i]<=1)
  {
    loss20<-append(loss20, 20*0.5)
  }else
    loss20<-append(loss20, 0)
}
for(i in 1:100)
{
  if(t_d8[i]<=1)
  {
    loss8<-append(loss8, 8*0.5)
  }else
    loss8<-append(loss8, 0)
}

#c
