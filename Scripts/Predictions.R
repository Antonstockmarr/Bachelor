setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if(!exists("n")){
  source("data.R")
}
source("TrainTest.R")
source("BSplines.R")

# Daily predictions ----------------------------------

k <-1:n
Long <- k[Datalengths>=360]
Short <- k[Datalengths<360]

#Flip WeatherCons
for(i in 1:n){
  k<-dim(weatherCons[[i]])[1]
  weatherCons[[i]]<-weatherCons[[i]][k:1,]
}

# Defining data used for modeling
model.data <- weatherCons
# Various attributes are removed
for (i in 1:n)
{
  model.data[[i]]$Date <- NULL
  model.data[[i]]$PrecipitationProbability <- NULL
  model.data[[i]]$SunHour <- NULL
  model.data[[i]]$UltravioletIndex <- NULL
  model.data[[i]]$Condition <- NULL
}

ttd<-TrainTest(model.data,31)

lmMultipleNoP <- vector(mode = "list", length = n)

par(mfrow = c(1,1))
k<-0
for (i in c(Long,Short)) {
  k<-k+1
  if(k<=length(Long)){
    print(paste('Modeling long house ',i))
  }else{
    print(paste('Modeling short house ',i))
  }
  model.tmp <- ttd[[1]][[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  Splinebasis <- BSplines(model.tmp$WindDirection)
  
  tmp.wind <- Splinebasis*model.tmp$WindSpeed
  
  model.tmp$North <- tmp.wind[,3]
  model.tmp$East <- tmp.wind[,4]
  model.tmp$South <- tmp.wind[,1]
  model.tmp$West <- tmp.wind[,2]
  lmMultipleNoP[[i]] <- lm(Consumption ~ Temperature*(North + East + South + West)+
                             Radiation, data = model.tmp)
  newData = ttd[[2]][[i]]
  Splinetest <- BSplines(newData$WindDirection)
  test.wind <- Splinetest*newData$WindSpeed
  newData$North <- test.wind[,3]
  newData$East <- test.wind[,4]
  newData$South <- test.wind[,1]
  newData$West <- test.wind[,2]
  newData$Consumption<-NULL
  Pred<-data.frame(predict(object=lmMultipleNoP[[i]], newdata=newData, interval = "prediction", level = 0.95))
  
  if(k>length(Long)){
    mm<-paste("Short house: ",i)
  }else{
    mm<-paste("Long house: ",i)
  }
  
  plot(Pred$fit,type='l',ylim=range(Pred$lwr,Pred$upr),main=mm)
  lines(Pred$upr,lty=2)
  lines(Pred$lwr,lty=2)
  lines(ttd[[2]][[i]]$Consumption,lty=1,col=2)
  legend(x = "topleft", legend = c("Prediction", "95% PI", "Data"), lty = c(1,2,1), col = c(1,1,2))
}

# Hourly Predictions ----------------------------------------

for(i in 1:n){
  nas<-which(!is.na(data[[i]]$Flow))
  tmp.dat<-data[[i]][nas[1]:tail(nas,1),]
  nas<-which(is.na(tmp.dat$Flow))
  m<-dim(tmp.dat)[2]
  for(j in nas){
    tmp.dat[j,2:m]<-(data[[i]][j-1,2:m]+data[[i]][j+1,2:m])/2
  }
  data[[i]]<-tmp.dat
}

for(i in 1:n){
  k<-dim(data[[i]])[1]
  data[[i]]<-data[[i]][k:1,]
}
k<-dim(weather)[1]
weather <- weather[k:1,]

tth<-TrainTest(data,7*24)
load("arimax.Rdata")
load("marimax.Rdata")

arimax[[4]][[1]]

i<-55


a <- 12
tmp.dat <- weather[(weather$ObsTime >= head(tth[[1]][[i]]$ObsTime,1)),]
tmp.dat <- tmp.dat[tmp.dat$ObsTime <= tail(tth[[1]][[i]]$ObsTime,1),]
tmp <- tmp.dat$Temperature
Temperature <- (tmp<a)*(a-tmp)
arima.dat <- data.frame(Temperature = Temperature, Consumption = tth[[1]][[i]]$CoolingDegree*tth[[1]][[i]]$Volume)
A <- arima(arima.dat$Consumption, order =c(1,0,1), seasonal = list(order = c(1,1,1), period = 24),xreg=arima.dat$Temperature)

tmp.dat <- weather[(weather$ObsTime >= head(tth[[2]][[i]]$ObsTime,1)),]
tmp.dat <- tmp.dat[tmp.dat$ObsTime <= tail(tth[[2]][[i]]$ObsTime,1),]
tmp <- tmp.dat$Temperature
Temperature <- (tmp<a)*(a-tmp)

p<-predict(A,n.ahead=length(tmp),se.fit=TRUE,newxreg = Temperature)

plot(p$pred,ylim=c(min(p$pred-2*p$se,tth[[2]][[i]]$CoolingDegree*tth[[2]][[i]]$Volume),max(p$pred+2*p$se,tth[[2]][[i]]$CoolingDegree*tth[[2]][[i]]$Volume)))
lines(p$pred+2*p$se,lty=2)
lines(p$pred-2*p$se,lty=2)
lines(length(tth[[1]][[i]]$ObsTime)+(1:length(tth[[2]][[i]]$ObsTime)),tth[[2]][[i]]$CoolingDegree*tth[[2]][[i]]$Volume,col=2)
