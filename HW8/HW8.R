# Solution to Fin 567 Homework 8 spring 2020
#
#
# Two useful libraries:
library(xts)
library(rmgarch)

## Question 0: read the data
## The data are in the file F567.s2020.HW8data.csv downloaded from WRDS
WRDSdata <-read.csv("F567.s2020.HW8data.csv")

# WRDS' organization of the data is not convenient, so let's rearrange the data
permnos <- c(84398,89730,91933,90383, 91208, 90448)
tickers <- c("SPY","EEM","HYG","FXI", "USO", "GLD")
returns <- WRDSdata[which(WRDSdata$PERMNO == permnos[1]),c(2,6)]
names(returns)[names(returns)=="RET"] <- tickers[1] #change the variable name from "RET" to the ticker
for(i in 2:6){
  returns <- merge(returns, WRDSdata[which(WRDSdata$PERMNO == permnos[i]),c(2,6)], by = "date")
  names(returns)[names(returns)=="RET"] <- tickers[i]
} # the dataframe returns now has the date in column 1 and the 6 return series in columns 2-7

## Question 1: compute the mean returns
## First need to convert the simple returns to log returns
returns[,2:7] <- log(1+returns[,2:7])
Q1 <- sapply(returns[,2:7],mean)

## Question 2
## Note: The function dccfit() will be used; it estimates the DCC model in two stages
## First a univariate GARCH model is fitted to each return series
## The standardized residuals are then extracted
## Then a model with dynamically changing conditional correlation matrix is fitted

## As an initial step (not required), estimate a univariate GARCH model for each return process
## Specify univariate ARCH(1,1) model and set mean return = 0
uspec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                    distribution.model = "norm") 

## Check the univariate specification for the six series
fit.marg1 <- ugarchfit(spec = uspec, data = returns[,2])
fit.marg2 <- ugarchfit(spec = uspec, data = returns[,3])
fit.marg3 <- ugarchfit(spec = uspec, data = returns[,4])
fit.marg4 <- ugarchfit(spec = uspec, data = returns[,5])
fit.marg5 <- ugarchfit(spec = uspec, data = returns[,6])
fit.marg6 <- ugarchfit(spec = uspec, data = returns[,7])
coef(fit.marg1)
coef(fit.marg2)
coef(fit.marg3)
coef(fit.marg4)
coef(fit.marg5)
coef(fit.marg6)

## Now turn to estimating the DCC model
## Combine univariate specifications of the 6 GARCH models
marginspec <- multispec(replicate(6, uspec))

## Create DCC(1,1) specification
mspec <- dccspec(marginspec, dccOrder = c(1,1), model = "DCC", distribution = "mvnorm")

## Fit the DCC(1,1) model
mod <- dccfit(mspec,returns[,2:7])
mod
coef(mod)


##Question 3
##Use function eigen() to find the eigenvectors and eigenvalues
## Also use the R function prcomp()
## First we need the covariance matrix
covmatrix <- cov.wt(returns[,2:7], wt = rep(1/nrow(returns[,2:7]), nrow(returns[,2:7])), 
                    cor = FALSE, center = FALSE, method = "ML")
eigensystem <- eigen(covmatrix$cov)
V <- eigensystem$vectors
lambda <- eigensystem$values

returns.pca <- prcomp(returns[,2:7], center = FALSE, scale = FALSE)
summary(returns.pca)
returns.pca$rotation

Q3a <- sqrt(lambda[1])
Q3b <- Q3a * V[4,1]
Q3c <- lambda[1]/sum(lambda)

pccov <- outer(V[,1],V[,1])*lambda[1]+outer(V[,2],V[,2])*lambda[2]+outer(V[,3],V[,3])*lambda[3]+outer(V[,4],V[,4])*lambda[4]+outer(V[,5],V[,5])*lambda[5]+outer(V[,6],V[,6])*lambda[6]
pc3cov <- outer(V[,1],V[,1])*lambda[1]+outer(V[,2],V[,2])*lambda[2]+outer(V[,3],V[,3])*lambda[3]
Q3d <- pc3cov[1,4]




