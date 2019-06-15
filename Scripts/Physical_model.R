source("data.R")
source("TrainTest.R")
library("forecast")
library("marima")

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

tth<-TrainTest(data,14*24)

# Physical MARIMA
i<- 20
Consumption <- tth[[1]][[i]]$Volume*tth[[1]][[i]]$CoolingDegree*cc

a <- 12
tmp.dat <- weather[(weather$ObsTime >= head(tth[[1]][[i]]$ObsTime,1)),]
tmp.dat <- tmp.dat[tmp.dat$ObsTime <= tail(tth[[1]][[i]]$ObsTime,1),]
tmp <- tmp.dat$Temperature
Temperature <- (tmp<a)*(a-tmp)
Radiation <- tmp.dat$Radiation

len <- length(data[[i]]$ObsTime)
sin1 <- sin(((1:len)-hour(tth[[1]][[i]]$ObsTime[1]))*2*pi/24)
sin2 <- sin(((1:len)-hour(tth[[1]][[i]]$ObsTime[1]))*2*pi/12)
cos1 <- cos(((1:len)-hour(tth[[1]][[i]]$ObsTime[1]))*2*pi/24)
cos2 <- cos(((1:len)-hour(tth[[1]][[i]]$ObsTime[1]))*2*pi/12)

plot(cos1[1:100],type='l',col=1)
lines(cos2[1:100],col=2)
lines(sin1[1:100],col=3)
lines(sin2[1:100],col=4)

dat <- cbind(Consumption,Temperature,Radiation,sin1,sin2,cos1,cos2)
dm <- define.model(kvar=7,ar =c(1,2,3), ma =c(1), reg.var = c(2,3,4,5,6,7))
dm$ar.pattern[1,c(2,3,4,5,6,7),c(3,4)]<- 0
m2 <- marima(dat, ar.pattern = dm$ar.pattern, ma.pattern = dm$ma.pattern, Plot = 'log.det',penalty=0)
short.form(m2$ar.estimates)
short.form(m2$ma.estimates)
short.form(m2$ar.stdv)
short.form(m2$ma.stdv)


plot(m2$ar.estimates[1,4,2]*sin1[1:24]+m2$ar.estimates[1,5,2]*sin2[1:24]
     +m2$ar.estimates[1,6,2]*cos1[1:24]+m2$ar.estimates[1,7,2]*cos2[1:24],type='l',ylab='')
plot(Houravg[,i],type='l')


# marima estimere ma delen p?? en iterativ m??de, normalt skal man bruge kalman filtering
# referer artikel

# AIC og BIC
# BIC :  RMSE + log(p)

