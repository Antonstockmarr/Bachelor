source("data.R")
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

Marima_model <- function(houses)
{
# (1,0,1)x(1,1,1)
differencing = c(1, 24)
result <- vector(mode='list', length = n)
for (i in houses)
{
  a <- 12
  tmp.dat <- weather[(weather$ObsTime <= head(data[[i]]$ObsTime,1)),]
  tmp.dat <- tmp.dat[tmp.dat$ObsTime >= tail(data[[i]]$ObsTime,1),]
  tmp <- tmp.dat$Temperature
  Temperature <- (tmp<a)*(a-tmp)
  arima.dat <- cbind(data[[i]]$CoolingDegree*data[[i]]$Volume,Temperature)
  dd <- define.dif(arima.dat, differencing)
  dm1 <-define.model(kvar=2, ar = c(1,24,25), ma = c(1,24,25),reg.var = 2)
  dm1$ar.pattern[1,2,25:26] <- 0
  m1 <- marima(dd$y.dif, ar.pattern = dm1$ar.pattern, ma.pattern = dm1$ma.pattern, Plot = 'log.det',penalty=0)
  #short.form(m1$ar.estimates,leading = F)
  #short.form(m1$ma.estimates,leading = F)
  #m1$ar.pvalues[1,,]
  #m1$ma.pvalues[1,,]
  days = 7
  par(mar=c(3,3,2,1), mgp=c(2,0.7,0),mfrow=c(2,1),xpd=FALSE,pty = 'm')
  lagmax = days*24+2
  print(i)
  acf((m1$residuals[,colSums(is.na(m1$residuals)) == 0])[1,],panel.first = c(abline(v=(1:days)*24,col=Wcol[4],lty=2)),lag.max=lagmax,main="")
  title(paste("House ",i))
  acf((m1$residuals[,colSums(is.na(m1$residuals)) == 0])[1,],"partial",lag.max = lagmax,panel.first = c(abline(v=(1:days)*24,col="red",lty=2)),main="")
  result[[i]]<- m1
}
result
}


ARIMAX_model <- function(two_sd,three_sd,nonseas,seas,houses,xreg)
{
  models <- vector(mode = 'list',length = n)
  logavg <- 0
  for(i in houses){
    if (xreg)
    {
    a <- 12
    tmp.dat <- weather[(weather$ObsTime >= head(data[[i]]$ObsTime,1)),]
    tmp.dat <- tmp.dat[tmp.dat$ObsTime <= tail(data[[i]]$ObsTime,1),]
    tmp <- tmp.dat$Temperature
    Temperature <- (tmp<a)*(a-tmp)
    arima.dat <- data.frame(Temperature = Temperature, Consumption = data[[i]]$CoolingDegree*data[[i]]$Volume)
    A <- arima(arima.dat$Consumption, order =nonseas, seasonal = list(order = seas, period = 24),xreg=arima.dat$Temperature)
    }
    else
    {
    A <- arima(arima.dat$Consumption, order =nonseas, seasonal = list(order = seas, period = 24))
    }
    lagmax = 4*24
    print(i)
    par(mfrow=c(2,1))
    acf(A$residuals,panel.first = c(abline(v=(1:4)*24,col=Wcol[4],lty=2)),lag.max=lagmax,main="")
    title(paste("House ",i))
    acf(A$residuals,"partial",lag.max = lagmax,panel.first = c(abline(v=(1:4)*24,col="red",lty=2)),main="")
    logavg = logavg + A$loglik/n
    
    plot(A)
    models[[i]]=A
    names(two_sd)[length(two_sd)] <- names(A$coef)[length(names(A$coef))]
    names(three_sd)[length(three_sd)] <- names(A$coef)[length(names(A$coef))]
    for (j in names(A$coef))
    {
      
      if (abs(A$coef[j])< 2*sqrt(abs(diag(vcov(A))))[j])
      {
        two_sd[j]=two_sd[j]+1
      }
      if (abs(A$coef[j])< 3*sqrt(abs(diag(vcov(A))))[j])
      {
        three_sd[j]=three_sd[j]+1
      }
    }
    
  }
  result <- vector(mode='list',length=4)
  result[[1]] <- two_sd
  result[[2]] <- three_sd
  result[[3]] <- logavg
  result[[4]] <- models
  result
}


# The first model
two_sd <- data.frame("ar1"=0,"ma1"=0,'sma1'=0,'sar1'=0,'sma2'=0, "Temperature"=0)
three_sd <- data.frame("ar1"=0,"ma1"=0,'sma1'=0,'sar1'=0,'sma2'=0,"Temperature"=0)

model1 <- ARIMAX_model(two_sd,three_sd,c(1,0,1),c(1,1,2))


# The second model
two_sd <- data.frame("ar1"=0,"ma1"=0,'sma1'=0,'sar1'=0, "Temperature"=0)
three_sd <- data.frame("ar1"=0,"ma1"=0,'sma1'=0,'sar1'=0, "Temperature"=0)

model2 <- ARIMAX_model(two_sd,three_sd,c(1,0,1),c(1,1,1),c(1:10),T)


model2marima <- Marima_model(c(1:10))

i=10
short.form(model2marima[[i]]$ar.estimates)
short.form(model2marima[[i]]$ma.estimates)
model2[[4]][[i]]

# The third model
two_sd <- data.frame("ar1"=0,"ma1"=0,'ma2'=0,'sma1'=0,'sar1'=0, "Temperature"=0)
three_sd <- data.frame("ar1"=0,"ma1"=0,'ma2'=0,'sma1'=0,'sar1'=0, "Temperature"=0)

model3 <- ARIMAX_model(two_sd,three_sd,c(1,0,2),c(1,1,1))


# The third model
two_sd <- data.frame("ar1"=0,"ma1"=0,'ma2'=0,'sma1'=0,'sar1'=0,'sma2'=0, "Temperature"=0)
three_sd <- data.frame("ar1"=0,"ma1"=0,'ma2'=0,'sma1'=0,'sar1'=0,'sma2'=0, "Temperature"=0)

model4 <- ARIMAX_model(two_sd,three_sd,c(1,0,2),c(1,1,2))

for (i in 1:38)
{
  Testmodel[[4]][[i]] <- model2_2[[4]][[i]]
}

for (i in 40:n)
{
  Testmodel[[4]][[i]] <- model2[[4]][[i]]
}


for (i in c(1:38,40:n))
{
  plot(Testmodel[[4]][[i]])
}

days = 7
lagmax = days*24+2
for (i in c(1:38,40:n))
{
  par(mfrow=c(2,1))
  acf(Testmodel[[4]][[i]]$residuals,panel.first = c(abline(v=(1:days)*24,col=Wcol[4],lty=2)),lag.max=lagmax,main="")
  title(paste("House ",i))
  acf(Testmodel[[4]][[i]]$residuals,"partial",lag.max = lagmax,panel.first = c(abline(v=(1:days)*24,col="red",lty=2)),main="")
  
}


# Physical MARIMA
i<- 20
Consumption <- data[[i]]$Volume*data[[i]]$CoolingDegree

a <- 12
tmp.dat <- weather[(weather$ObsTime >= head(data[[i]]$ObsTime,1)),]
tmp.dat <- tmp.dat[tmp.dat$ObsTime <= tail(data[[i]]$ObsTime,1),]
tmp <- tmp.dat$Temperature
Temperature <- (tmp<a)*(a-tmp)
Radiation <- tmp.dat$Radiation

len <- length(data[[i]]$ObsTime)
sin1 <- sin(((1:len)-hour(data[[i]]$ObsTime[1]))*2*pi/24)
sin2 <- sin(((1:len)-hour(data[[i]]$ObsTime[1]))*2*pi/12)
cos1 <- cos(((1:len)-hour(data[[i]]$ObsTime[1]))*2*pi/24)
cos2 <- cos(((1:len)-hour(data[[i]]$ObsTime[1]))*2*pi/12)
cos3 <- cos((((1:len)-hour(data[[i]]$ObsTime[1])))*2*pi/6)
sin3 <- sin(((1:len)-hour(data[[i]]$ObsTime[1]))*2*pi/6)

plot(cos1[1:100],type='l',col=1)
lines(cos2[1:100],col=2)
lines(sin1[1:100],col=3)
lines(sin2[1:100],col=4)

dat <- cbind(Consumption,Temperature,Radiation,sin1,sin2,cos1,cos2,sin3,cos3)
dm <- define.model(kvar=9,ar =c(1,2,3), ma =c(1), reg.var = c(2,3,4,5,6,7,8,9))
dm$ar.pattern[1,c(2,3,4,5,6,7,8,9),c(3,4)]<- 0
m2 <- marima(dat, ar.pattern = dm$ar.pattern, ma.pattern = dm$ma.pattern, Plot = 'log.det',penalty=0)
short.form(m2$ar.estimates)
short.form(m2$ma.estimates)
short.form(m2$ar.stdv)
short.form(m2$ma.stdv)


plot(m2$ar.estimates[1,4,2]*sin1[1:24]+m2$ar.estimates[1,5,2]*sin2[1:24]
     +m2$ar.estimates[1,6,2]*cos1[1:24]+m2$ar.estimates[1,7,2]*cos2[1:24]+m2$ar.estimates[1,8,2]*sin3[1:24]+m2$ar.estimates[1,9,2]*cos3[1:24],type='l',ylab='')
plot(m2$ar.estimates[1,4,2]*sin1[1:24])
lines()
plot(Houravg[,i],type='l')

# marima estimere ma delen p?? en iterativ m??de, normalt skal man bruge kalman filtering
# referer artikel

# AIC og BIC
# BIC :  RMSE + log(p)

