setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("data.R")

for(k in 1:n){
  day.tmp <- day.weather[(day.weather$Date <= as.Date(EndDays[k],tz="GMT")),]
  day.tmp <- day.tmp[day.tmp$Date >= as.Date(StartDays[k],tz="GMT"),] 
  t <- day.tmp$Temperature
  q <- day.data[[k]]$CoolingDegree*day.data[[k]]$Volume
  
  fitted.temp <- 0:max(day.weather$Temperature)
  plot.points <-rbind(rep(0,length(fitted.temp)),rep(0,length(fitted.temp)),rep(0,length(fitted.temp)))
  
  for (i in fitted.temp){
    tmp.fit<-lm(q[t<=i]~t[t<=i])
    plot.points[,i+1]<-tmp.fit$coefficients[2]+c(2,0,-2)*sigma(tmp.fit)
  }
  
  plot(fitted.temp,plot.points[2,],main=k)#,ylim=c(-1,.7))
}


for(k in 1:n){
  day.tmp <- day.weather[(day.weather$Date <= as.Date(EndDays[k],tz="GMT")),]
  day.tmp <- day.tmp[day.tmp$Date >= as.Date(StartDays[k],tz="GMT"),] 
  t <- day.tmp$Temperature
  q <- day.data[[k]]$CoolingDegree*day.data[[k]]$Volume
  
  fitted.temp <- (floor(min(day.tmp$Temperature))+2):ceiling(max(day.tmp$Temperature))
  plot.points <-rep(0,length(fitted.temp))
  obs <-rep(0,length(fitted.temp))
  
  j=1
  for (i in fitted.temp){
    q.tmp <- q[t<=i]
    t.tmp <- t[t<=i]
    q.tmp <- q.tmp[t.tmp>i-2]
    plot.points[j]<-min(q.tmp)
    obs[j]<-length(q.tmp)
    j=j+1
  }
  plot(fitted.temp,plot.points,main=k)#,ylim=c(-1,.7))
}
