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

ttd<-TrainTest(data,7*24)

Marima_model <- function(houses)
{
# (1,0,1)x(1,1,1)
differencing = c(1, 24)
result <- vector(mode='list', length = n)
for (i in houses)
{
  a <- 12
  tmp.dat <- weather[(weather$ObsTime >= head(ttd[[1]][[i]]$ObsTime,1)),]
  tmp.dat <- tmp.dat[tmp.dat$ObsTime <= tail(ttd[[1]][[i]]$ObsTime,1),]
  tmp <- tmp.dat$Temperature
  Temperature <- (tmp<a)*(a-tmp)
  arima.dat <- cbind(ttd[[1]][[i]]$CoolingDegree*ttd[[1]][[i]]$Volume*cc,Temperature)
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
    tmp.dat <- weather[(weather$ObsTime >= head(ttd[[1]][[i]]$ObsTime,1)),]
    tmp.dat <- tmp.dat[tmp.dat$ObsTime <= tail(ttd[[1]][[i]]$ObsTime,1),]
    tmp <- tmp.dat$Temperature
    Temperature <- (tmp<a)*(a-tmp)
    arima.dat <- data.frame(Temperature = Temperature, Consumption = cc*ttd[[1]][[i]]$CoolingDegree*ttd[[1]][[i]]$Volume)
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
two_sd <- data.frame("ar1"=0,"ma1"=0,'sma1'=0,'sar1'=0,'intercept'=0, "Temperature"=0)
three_sd <- data.frame("ar1"=0,"ma1"=0,'sma1'=0,'sar1'=0,'intercept'=0,"Temperature"=0)
model1 <- ARIMAX_model(two_sd,three_sd,c(1,0,1),c(1,0,1),c(1:n),T)


# The second model
two_sd <- data.frame("ar1"=0,"ma1"=0,'sma1'=0,'sar1'=0, "Temperature"=0)
three_sd <- data.frame("ar1"=0,"ma1"=0,'sma1'=0,'sar1'=0, "Temperature"=0)

arimax <- ARIMAX_model(two_sd,three_sd,c(1,0,1),c(1,1,1),c(1:n),T)


save(arimax,file="arimax.Rdata")

marimax <- Marima_model(c(1:n))

save(marimax,file="marimax.Rdata")
