source("data.R")

#library("ggplot2")

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
avgdata[,8]<-avgconsumption

ggplot(avgdata[,c(2,7)])

}

head(data[[1]])
head(as.Date(data[[1]]$StartDateTime,tz="GMT"))

tmp.dat <- data[[1]]
tmp.dat$StartDateTime <- as.Date(tmp.dat$StartDateTime,tz="GMT")
tmp.dateseq <- as.list(seq(from=tail(tmp.dat$StartDateTime,n=1), to=tmp.dat$StartDateTime[1], by="day"))
aggregate(tmp.dat,by=as.list(tmp.dat$StartDateTime),FUN = mean)
