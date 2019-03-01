source("data.R")
n <- 69

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

plot(avgconsumption)

