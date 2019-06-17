##----------------------------------------------------------------
## Init
##----------------------------------------------------------------
rm(list=ls())

## Source the scripts with functions in the "functions" folder. Just a neat way of arranging helping functions in R
source("data.R")
files <- dir("functions", full.names=TRUE)
for(i in 1:length(files)) source(files[i])
for(i in 1:n){
  k<-dim(data[[i]])[1]
  data[[i]]<-data[[i]][k:1,]
}
k<-dim(weather)[1]
weather <- weather[k:1,]
## Read the data

i<-6

tmp.dat <- weather[(weather$ObsTime >= head(data[[i]]$ObsTime,1)),]
tmp.dat <- tmp.dat[tmp.dat$ObsTime <= tail(data[[i]]$ObsTime,1),]
tmp <- tmp.dat$Temperature
slr<-tmp.dat$Radiation*tmp.dat$SunHour

Hus<-data[[i]]
Hus$TemperatureIn<-NULL
Hus$TemperatureOut<-NULL
Hus$Temperature<-tmp
Hus$Radiation<-slr



names(Hus)

Xor <- Hus[ ,c("ObsTime","Energy","Temperature","Radiation")]
names(Xor) <- c("t","Qi","Te","G")

## Do daily values
Xday <- resample(Xor, 24*3600, tstart=trunc(Xor$t[1],"days"))
summary(lm(Qi ~ Te + G, Xday))


## Maybe take only a period and resample
## X <- Xor[period("2019-01-01",Xor$t,"2019-01-31"), ]
X <- resample(Xor, 3600, tstart=trunc(Xor$t[1],"days")+24*3600)
##X <- Xor

## Plot the series of interest
Xp <- X[period("2019-01-01",X$t,"2019-01-31"), ]
setpar("ts", mfrow=c(3,1))
plot(Xp$t,Xp$Qi,type="l")
plot(Xp$t,Xp$Te,type="l")
plot(Xp$t,Xp$G,type="l")
axis.POSIXct(1,Xp$t,xaxt="s")


##----------------------------------------------------------------
## 4.1 ARMAX
##----------------------------------------------------------------
#install.packages('marima')
require(marima)

## We need to shift the inputs one lag forward to make it work

## Make a series of ones for the intercept estimate
X$One <- rep(1, nrow(X))

## Maybe lag the input vectors 1 step
#X$Te <- lagVec(X$Te, -1)
#X$G <- lagVec(X$G, -1)

## Try increasing the order
fit1 <- estimateARMAX(outName='Qi', inNames=c('One','Te','G'), pAR=1, pMA=1, noLagPattern="One", pNoLag=1)
fit2 <- estimateARMAX(outName='Qi', inNames=c('One','Te','G'), pAR=2, pMA=1, noLagPattern="One", pNoLag=1)
fit3 <- estimateARMAX(outName='Qi', inNames=c('One','Te','G'), pAR=3, pMA=1, noLagPattern="One", pNoLag=1)
fit4 <- estimateARMAX(outName='Qi', inNames=c('One','Te','G'), pAR=4, pMA=1, noLagPattern="One", pNoLag=1)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)

summary(fit1)
summary(fit2)
summary(fit3)

HLC.Qi.ARX(fit1)
HLC.Qi.ARX(fit2)
HLC.Qi.ARX(fit3)

acfccfPlot(fit2, X)
val <- HLC.Qi.ARX(fit2)
## Standard error
sqrt(val$VarHLC.Ta)
val$HLC.Ta + c(-1,1) * 1.96 * sqrt(val$VarHLC.Ta)


## Plot the series of interest
plot(fit2$residuals)
tmp <- X
tmp$residuals <- NA
tmp[(nrow(tmp) - length(fit2$residuals) + 1):nrow(tmp), "residuals"] <- fit2$residuals
Xp <- tmp[period("2019-01-01",tmp$t,"2019-01-31"), ]
setpar("ts", mfrow=c(4,1))
plot(Xp$t,Xp$residuals,type="l")
abline(h=0)
plot(Xp$t,Xp$Qi,type="l")
abline(h=mean(Xp$Qi))
plot(Xp$t,Xp$Te,type="l")
plot(Xp$t,Xp$G,type="l")
axis.POSIXct(1,Xp$t,xaxt="s")



##----------------------------------------------------------------
## 3.4 HLC and step response
##----------------------------------------------------------------
## The step response for each of the inputs
## Use your best fit
stepResponseARX(fit2,X,"Te")
stepResponseARX(fit2,X,"G")

##----------------------------------------------------------------
## Calculate the estimated HLC-value and gA-value for the box based on the estimated steady-state reponse for Ti and Te, for G.
## Use your best fit. See the function in the file "r/functions/HLC.Qi.ARX.R"
HLC.Qi.ARX(fit2)



##----------------------------------------------------------------
## Use fourier series as input

## Calculate the fourier series
X$tod <- as.POSIXlt(X$t)$hour
X <- cbind(X, fs(X$tod/24, nharmonics=3))

names(X)

## Plot the series of interest
Xp <- X[period("2019-01-01",X$t,"2019-01-31"), ]
setpar("ts", mfrow=c(5,1))
plot(Xp$t,Xp$Qi,type="l")
plot(Xp$t,Xp$Te,type="l")
plot(Xp$t,Xp$G,type="l")
plot(Xp$t,Xp$sin_1,type="l")
lines(Xp$t,Xp$cos_1)
plot(Xp$t,Xp$sin_2,type="l")
lines(Xp$t,Xp$cos_2)
axis.POSIXct(1,Xp$t,xaxt="s")


## Try increasing the order
outName <- 'Qi'
inNames <- c('One','Te','G',pst('sin_',1:2),pst('cos_',1:2))
noLagPattern <- "One|sin|cos"
pNoLag <- 1
fit1 <- estimateARMAX(outName, inNames, pAR=1, pMA=1, noLagPattern, pNoLag)
fit2 <- estimateARMAX(outName, inNames, pAR=2, pMA=1, noLagPattern, pNoLag)
fit3 <- estimateARMAX(outName, inNames, pAR=3, pMA=1, noLagPattern, pNoLag)

BIC(fit1)
BIC(fit2)
BIC(fit3)

summary(fit1)
summary(fit2)
summary(fit3)

HLC.Qi.ARX(fit1)
HLC.Qi.ARX(fit2)
HLC.Qi.ARX(fit3)

acfccfPlot(fit2, X)
