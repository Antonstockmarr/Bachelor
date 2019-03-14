setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("data.R")

# 64 vælges da det er godt:

str(day.data[[64]])
i=64
day.tmp <- day.weather[(day.weather$Date <= as.Date(EndDays[i],tz="GMT")),]
day.tmp <- day.tmp[day.tmp$Date >= as.Date(StartDays[i],tz="GMT"),] 
t <- day.tmp$Temperature
q <- day.data[[i]]$CoolingDegree*day.data[[i]]$Volume



















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