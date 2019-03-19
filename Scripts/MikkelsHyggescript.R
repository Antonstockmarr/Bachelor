setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("data.R")

# 68 vælges da det er godt:

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
# points(fitted.temp,plot.points[1,],col=2)
# points(fitted.temp,plot.points[3,],col=2)
# 
# plot(q~t)

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












if("Til"=="Ida"){
tot.time.pts <-seq(from=min(StartDays), to=max(EndDays), by="hour")
avgconsumption<-rep(0,length(tot.time.pts))
weightavg<-rep(0,length(tot.time.pts))

for (i in 1:n) {
  tmp.index<-1+difftime(StartDays[i],min(StartDays), units ="hours"):difftime(EndDays[i],min(StartDays), units ="hours")
  tmp.data=data[[i]]
  tmp.data[is.na(data[[i]])] <- 0
  avgconsumption[tmp.index] <- avgconsumption[tmp.index] + tmp.data$Flow*tmp.data$CoolingDegree
  weightavg[tmp.index] <- weightavg[tmp.index] + rep(1,length(tmp.data$Flow)) - is.na(data[[i]]$Flow)
}

avgconsumption<-avgconsumption/weightavg
head(avgconsumption)

m=7

avgdata <- data[[2]]
avgdata[,1]<-seq(from=min(StartDays), to=max(EndDays), by="hour")
for(j in 2:m){
  avgdata[,j] <- rep(0,length(avgdata[[1]]))
  for (i in 1:n){
    tmp.index<-1+difftime(StartDays[i],min(StartDays), units ="hours"):difftime(EndDays[i],min(StartDays), units ="hours")
    tmp.data=data[[i]][,j]
    tmp.data[is.na(data[[i]])] <- 0
    avgdata[[j]][tmp.index] <- avgdata[[j]][tmp.index] + tmp.data
  }
  avgdata[[j]] <- avgdata[[j]]/weightavg
}
avgdata[,9]<-avgconsumption

pairs(avgdata[,2:9])

}