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
aggregate(x=data.frame(y=tmp.dat[,2]),by=as.list(tmp.dat[,1]),FUN = mean)
length(as.list(tmp.dat[,1]))
head(tmp.dat[,2:5])

categories <- data.frame(category = c("a", "a", "a", "a", "a","b", "b", "b", "b", "b","c", "c", "c", "c"))
observations <- data.frame(observation = c(rnorm(5, mean = 3, sd = 0.2),rnorm(5, mean = -2, sd = 0.4),rnorm(4, mean = 0, sd = 1)))

distr.estimate <- aggregate(x = observations,by = categories,FUN = mean)
distr.estimate


categories <- data.frame(StartDateTime = tmp.dat[,1])
observations <- tmp.dat[,2:7]

distr.estimate <- aggregate(x = observations,by = categories,FUN = mean)
head(distr.estimate)

tmp.wd <-weekdays(tmp.dat$StartDateTime,abbreviate = TRUE)
tmp.wd <-grepl("ø",tmp.wd)

x=1:4
x=x<=2
y=1:4
y=y>=4

(x&&y)

tmp.dat$StartDateTime <- as.Date(tmp.dat$StartDateTime,tz="GMT")
tmp.wd <-weekdays(tmp.dat$StartDateTime,abbreviate = TRUE)
grepl("ø",z)
