##----------------------------------------------------------------
## Init
##----------------------------------------------------------------
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

par(mfrow=c(1,3))

for(i in c(55,18,6)){
  
  tmp.dat <- weather[(weather$ObsTime >= head(data[[i]]$ObsTime,1)),]
  tmp.dat <- tmp.dat[tmp.dat$ObsTime <= tail(data[[i]]$ObsTime,1),]
  tmp <- tmp.dat$Temperature
  slr<-tmp.dat$Radiation*tmp.dat$SunHour
  
  Hus<-data[[i]]
  Hus$TemperatureIn<-NULL
  Hus$TemperatureOut<-NULL
  Hus$Temperature<-tmp
  Hus$Radiation<-slr
  
  #names(Hus)
  
  Xor <- Hus[ ,c("ObsTime","Energy","Temperature","Radiation")]
  names(Xor) <- c("t","Qi","Te","G")
  
  plotpoints<-matrix(rep(0,15),nrow = 3)
  
  ## Do daily values
  Xday <- resample(Xor, 24*3600, tstart=trunc(Xor$t[1],"days"))
  pd<-summary(lm(Qi ~ Te + G, Xday))
  plotpoints[,1]<-(pd$coefficients[2,1]+1.96*c(0,-pd$coefficients[2,2],+pd$coefficients[2,2]))
  
  ## Maybe take only a period and resample
  ## X <- Xor[period("2019-01-01",Xor$t,"2019-01-31"), ]
  X <- resample(Xor, 3600, tstart=trunc(Xor$t[1],"days")+24*3600)
  ##X <- Xor
  
  ## Plot the series of interest
  Xp <- X[period("2019-01-01",X$t,"2019-01-31"), ]
  # setpar("ts", mfrow=c(3,1))
  # plot(Xp$t,Xp$Qi,type="l")
  # plot(Xp$t,Xp$Te,type="l")
  # plot(Xp$t,Xp$G,type="l")
  # axis.POSIXct(1,Xp$t,xaxt="s")
  
  
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
  fit1 <- estimateARMAX(outName='Qi', inNames=c('One','Te','G'), pAR=1, pMA=1, noLagPattern="One|Te|G", pNoLag=1,printit = FALSE)
  fit2 <- estimateARMAX(outName='Qi', inNames=c('One','Te','G'), pAR=2, pMA=1, noLagPattern="One|Te|G", pNoLag=1,printit = FALSE)
  fit3 <- estimateARMAX(outName='Qi', inNames=c('One','Te','G'), pAR=3, pMA=1, noLagPattern="One|Te|G", pNoLag=1,printit = FALSE)
  fit4 <- estimateARMAX(outName='Qi', inNames=c('One','Te','G'), pAR=4, pMA=1, noLagPattern="One|Te|G", pNoLag=1,printit = FALSE)
  
  bics<-rep(0,5)
  
  bics[2]<-BIC(fit1)
  bics[3]<-BIC(fit2)
  bics[4]<-BIC(fit3)
  bics[5]<-BIC(fit4)
  
  summary(fit1)
  summary(fit2)
  summary(fit3)
  summary(fit4)
  
  h1<-HLC.Qi.ARX(fit1)
  h2<-HLC.Qi.ARX(fit2)
  h3<-HLC.Qi.ARX(fit3)
  h4<-HLC.Qi.ARX(fit4)
  
  plotpoints[,2]<--1*(h1$HLC.Ta+1.96*c(0,sqrt(h1$VarHLC.Ta),-sqrt(h1$VarHLC.Ta)))
  plotpoints[,3]<--1*(h2$HLC.Ta+1.96*c(0,sqrt(h2$VarHLC.Ta),-sqrt(h2$VarHLC.Ta)))
  plotpoints[,4]<--1*(h3$HLC.Ta+1.96*c(0,sqrt(h3$VarHLC.Ta),-sqrt(h3$VarHLC.Ta)))
  plotpoints[,5]<--1*(h4$HLC.Ta+1.96*c(0,sqrt(h4$VarHLC.Ta),-sqrt(h4$VarHLC.Ta)))
  
  BestIndex=which(bics==min(bics[-1]))
  Delta<-plotpoints[3,]-plotpoints[2,]
  BestIndex2=which(Delta==min(Delta[-1]))
  
  plot(c(.5,2,3.5),plotpoints[1,c(1,BestIndex,BestIndex2)],ylim=range(plotpoints),xlim=c(0,4),ylab="HCL incl. 95% CI",xaxt='n',xlab=paste("House:",i))
  
  lines(c(.5,.5),c(plotpoints[2,1],plotpoints[3,1]),col=Wcol[2])
  lines(c(2,2),c(plotpoints[2,BestIndex],plotpoints[3,BestIndex]),col=Wcol[2])
  lines(c(3.5,3.5),c(plotpoints[2,BestIndex2],plotpoints[3,BestIndex2]),col=Wcol[2])
  axis(1, at=c(.5,2,3.5), labels=c("Day","BIC","CI"))
  print(paste(i,bics[2],bics[3],bics[4],bics[5]))
}


# Med Fourier
for(i in c(18)){
  
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
  
  plotpoints<-matrix(rep(0,15),nrow = 3)
  
  ## Do daily values
  Xday <- resample(Xor, 24*3600, tstart=trunc(Xor$t[1],"days"))
  (pd<-summary(lm(Qi ~ Te + G, Xday)))
  plotpoints[,1]<-(pd$coefficients[2,1]+1.96*c(0,-pd$coefficients[2,2],+pd$coefficients[2,2]))
  
  ## Maybe take only a period and resample
  ## X <- Xor[period("2019-01-01",Xor$t,"2019-01-31"), ]
  X <- resample(Xor, 3600, tstart=trunc(Xor$t[1],"days")+24*3600)
  ##X <- Xor
  
  ## Plot the series of interest
  Xp <- X[period("2019-01-01",X$t,"2019-01-31"), ]
  # setpar("ts", mfrow=c(3,1))
  # plot(Xp$t,Xp$Qi,type="l")
  # plot(Xp$t,Xp$Te,type="l")
  # plot(Xp$t,Xp$G,type="l")
  # axis.POSIXct(1,Xp$t,xaxt="s")
  
  
  ##----------------------------------------------------------------
  ## 4.1 ARMAX
  ##----------------------------------------------------------------
  #install.packages('marima')
  require(marima)
  
  ## We need to shift the inputs one lag forward to make it work
  
  ## Make a series of ones for the intercept estimate
  X$One <- rep(1, nrow(X))
  X$tod <- as.POSIXlt(X$t)$hour
  X <- cbind(X, fs(X$tod/24, nharmonics=3))
  
  ## Maybe lag the input vectors 1 step
  #X$Te <- lagVec(X$Te, -1)
  #X$G <- lagVec(X$G, -1)
  
  ## Try increasing the order
  outName <- 'Qi'
  inNames <- c('One','Te','G',pst('sin_',1:3),pst('cos_',1:3))
  noLagPattern <- "One|sin|cos|Te|G"
  pNoLag <- 1
  fit1 <- estimateARMAX(outName, inNames, pAR=1, pMA=1, noLagPattern, pNoLag)
  fit2 <- estimateARMAX(outName, inNames, pAR=2, pMA=1, noLagPattern, pNoLag)
  fit3 <- estimateARMAX(outName, inNames, pAR=3, pMA=1, noLagPattern, pNoLag)
  fit4 <- estimateARMAX(outName, inNames, pAR=4, pMA=1, noLagPattern, pNoLag)
  
  bics<-rep(0,5)
  
  (bics[2]<-BIC(fit1))
  (bics[3]<-BIC(fit2))
  (bics[4]<-BIC(fit3))
  (bics[5]<-BIC(fit4))
  
  summary(fit1)
  summary(fit2)
  summary(fit3)
  summary(fit4)
  
  (h1<-HLC.Qi.ARX(fit1))
  (h2<-HLC.Qi.ARX(fit2))
  (h3<-HLC.Qi.ARX(fit3))
  (h4<-HLC.Qi.ARX(fit4))
  
  plotpoints[,2]<--1*(h1$HLC.Ta+1.96*c(0,sqrt(h1$VarHLC.Ta),-sqrt(h1$VarHLC.Ta)))
  plotpoints[,3]<--1*(h2$HLC.Ta+1.96*c(0,sqrt(h2$VarHLC.Ta),-sqrt(h2$VarHLC.Ta)))
  plotpoints[,4]<--1*(h3$HLC.Ta+1.96*c(0,sqrt(h3$VarHLC.Ta),-sqrt(h3$VarHLC.Ta)))
  plotpoints[,5]<--1*(h4$HLC.Ta+1.96*c(0,sqrt(h4$VarHLC.Ta),-sqrt(h4$VarHLC.Ta)))
  
  BestIndex=which(bics==min(bics[-1]))
  Delta<-plotpoints[3,]-plotpoints[2,]
  BestIndex2=which(Delta==min(Delta[-1]))
  
  plot(c(.5,2,3.5),plotpoints[1,c(1,BestIndex,BestIndex2)],ylim=range(plotpoints),xlim=c(0,4),ylab="HCL incl. 95% CI",xaxt='n',xlab=paste("House:",i))
  
  lines(c(.5,.5),c(plotpoints[2,1],plotpoints[3,1]),col=Wcol[4])
  lines(c(2,2),c(plotpoints[2,BestIndex],plotpoints[3,BestIndex]),col=Wcol[4])
  lines(c(3.5,3.5),c(plotpoints[2,BestIndex2],plotpoints[3,BestIndex2]),col=Wcol[4])
  axis(1, at=c(.5,2,3.5), labels=c("Day","BIC","CI"))
  
}
(h4<-HLC.Qi.ARX(fit1))

print(paste(h4$gA,'&',h4$sd.gA,'&',h4$gA-1.96*h4$sd.gA,'&',h4$HLC.Ta+1.96*h4$sd.gA))

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
par(mar=c(1,3,1,2), mgp=c(2,0.7,0),mfrow=c(1,1),xpd=FALSE)
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
fit4 <- estimateARMAX(outName, inNames, pAR=4, pMA=1, noLagPattern, pNoLag)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

HLC.Qi.ARX(fit1)
HLC.Qi.ARX(fit2)
HLC.Qi.ARX(fit3)
HLC.Qi.ARX(fit4)

acfccfPlot(fit2, X)
