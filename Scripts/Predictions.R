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
  if(k>length(Long)){
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
  legend(x = "topright", legend = c("Prediction", "95% PI", "Data"), lty = c(1,2,1), col = c(1,1,2))
}

# Hourly Predictions ----------------------------------------

tth<-




(o4<-arima(sqrt(tab$O3[train]), order=c(2,0,0),seasonal = (list(order=c(0,1,1),period=24))))
po<-predict(o4,n.ahead=48,se.fit=TRUE)
o.sd<-po$se
o.pred<-po$pred

o.low<-o.pred-2*o.sd
o.high<-o.pred+2*o.sd
plot(time[train[600:703]],tab$O3[train[600:703]],type="l",xlim=c(605,751),ylim = c(0,150), xlab = "Time", ylab = "O3 concentration")
lines(time[test],tab$O3[test], col = 2)
lines(time[test],(po$pred)^2, col = 3)
lines(time[test],(o.low)^2, col = 3, lty = 2)
lines(time[test],(o.high)^2, col = 3, lty = 2)
legend("topleft", c("Data", "Prediction", "95% PI"), lty = c(1,1,2), col = c(2,3,3))

