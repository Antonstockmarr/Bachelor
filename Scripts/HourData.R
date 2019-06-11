setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("data.r")
HourData <- vector("list",length=n)
rotate <- function(x) t(apply(x, 2, rev))

if(FALSE){
for (i in 1:n)
{
  print(i)
  Cons <- data.frame(matrix(ncol=25,nrow = length(day.data[[i]]$Date)))#  
  names(Cons) <- c("Date",levels(as.factor(hour(data[[i]]$ObsTime))))
  Cons$Date <- day.data[[i]]$Date
  for (j in 0:23)
  {
    Cons[j+2] <- data[[i]]$Volume[hour(data[[i]]$ObsTime) == j]*data[[i]]$CoolingDegree[hour(data[[i]]$ObsTime) == j]
  }
  # Byt fuck
  HourTmp <- vector("list",length=length(weather))
  for (k in 2:length(weather))
  {
    Attribute <- data.frame(matrix(ncol=25,nrow = length(day.data[[i]]$Date)))  
    names(Attribute) <- c("Date",levels(as.factor(hour(data[[i]]$ObsTime))))
    Attribute$Date <- day.data[[i]]$Date
    for (j in 0:23)
    {
      tmp <- weather[weather$ObsTime <= data[[i]]$ObsTime[1],]
      tmp <- tmp[tmp$ObsTime >= tail(data[[i]]$ObsTime,1),]
      Attribute[j+2] <- tmp[(hour(data[[i]]$ObsTime) == j),k]
    }
    HourTmp[[k-1]] <- data.matrix(Attribute)
  }
  
  HourData[[i]]<- list(Consumption = data.matrix(Cons), Temperature = HourTmp[[1]], WindSpeed = HourTmp[[2]],
                            WindDirection = HourTmp[[3]], SunHour = HourTmp[[4]], Condition = HourTmp[[5]],
                            UltravioletIndex = HourTmp[[6]], MeanSeaLevelPressure = HourTmp[[7]], DewPoint = HourTmp[[8]],
                            Humidity = HourTmp[[9]], PrecipitationProbability = HourTmp[[10]],
                            IsHistoricalEstimated = HourTmp[[11]], Radiation = HourTmp[[12]])
}
}

load("HourData.Rdata")


# Plot of the hour values as percentage of day
#Avgcons <- c(1:n)
#Sumcons <- c(1:n)
Houravg = matrix(c(rep(0,24*n)),nrow = 24)
#for (i in 1:n)
#{
#  Sumcons[i] <- sum(weatherCons[[i]]$Consumption,na.rm = TRUE)
#  Avgcons[i] <- mean(weatherCons[[i]]$Consumption,na.rm = TRUE)
#}
for (i in 1:n)
{
  Houravg[,i] <- colMeans(HourData[[i]]$Consumption,na.rm = TRUE)[-1]
  Houravg[,i] <- Houravg[,i]/sum(Houravg[,i])
}
Houravg <- Houravg[c(2:24,1),]

rownames(Houravg) <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
colnames(Houravg) <- c(1:n)
tt <- Houravg
library('fields')

image.plot(t(tt[rev(order(row.names(tt))),]), axes=FALSE, 
           lab.breaks=NULL,main = 'Average consumption of all houses during the day')
axis(2, at=seq(1+1/48,0-1/48, length=13), labels=c('00','02','04','06','08','10','12','14','16','18','20','22','24'), lwd=0.1, pos=-0.01,las=1)
abline(h=c(seq(1,0, length=24)+1/48),lwd=0.75)


# Consumption in the summer period
for (i in 1:n)
  {
  SummerDays <- day.weather$Date[day.weather$Temperature >= 15]
  tmp <- HourData[[i]]$Consumption[,'Date']
  tmp_index <- sapply(tmp,function(x) x %in% SummerDays)
  Houravg[,i] <- colMeans(HourData[[i]]$Consumption[tmp_index,],na.rm = TRUE)[-1]
  Houravg[,i] <- Houravg[,i]/sum(Houravg[,i])
}
Houravg <- Houravg[c(2:24,1),]
summeravg <- Houravg


rownames(Houravg) <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
colnames(Houravg) <- c(1:n)
tt <- Houravg
image.plot(t(tt[rev(order(row.names(tt))),]), axes=FALSE, 
           lab.breaks=NULL,main = 'Average consumption of all houses (summer period)')
axis(2, at=seq(1+1/48,0-1/48, length=13), labels=c('00','02','04','06','08','10','12','14','16','18','20','22','24'), lwd=0.1, pos=-0.01,las=1)
abline(h=c(seq(1,0, length=24)+1/48),lwd=0.75)


mcons_summer <-apply(Houravg,1,mean)

#Finns plot
ttavg<-tt[,1]
for(i in 2:n){
  ttavg<-ttavg+tt[,i]
}
ttavg<-ttavg/n

k=1:24
plot(k[ttavg>=quantile(ttavg)[2] &  ttavg<=quantile(ttavg)[4]],ttavg[ttavg>=quantile(ttavg)[2] & ttavg<=quantile(ttavg)[4]],col=Wcol[3],pch=19,ylab ="average consumption per hour (normalized)",xlab="Hour", xaxt ="n",type='h',xlim=c(0,24),ylim=c(0.035,0.053))
points(k[ttavg>=quantile(ttavg)[4]],ttavg[ttavg>=quantile(ttavg)[4]],col=Wcol[4],type='h')
points(k[ttavg<=quantile(ttavg)[2]],ttavg[ttavg<=quantile(ttavg)[2]],col=Wcol[2],type = 'h')
points(k[ttavg>=quantile(ttavg)[2] &  ttavg<=quantile(ttavg)[4]],ttavg[ttavg>=quantile(ttavg)[2] & ttavg<=quantile(ttavg)[4]],col=Wcol[3],pch=19)
points(k[ttavg>=quantile(ttavg)[4]],ttavg[ttavg>=quantile(ttavg)[4]],col=Wcol[4],pch=19)
points(k[ttavg<=quantile(ttavg)[2]],ttavg[ttavg<=quantile(ttavg)[2]],col=Wcol[2],pch=19)
abline(h=quantile(ttavg)[4],lty=2,col=Wcol[4],lwd=2)
abline(h=quantile(ttavg)[3],lty=2,col=Wcol[3],lwd=2)
abline(h=quantile(ttavg)[2],lty=2,col=Wcol[2],lwd=2)
legend(x="topright",legend = c("75th percentile","50th percentile","25th percentile"),lty = c(2,2,2),col = c(Wcol[4],Wcol[3],Wcol[2]),lwd=2)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),labels=c("00","03","06","09","12","15","18","21","24"))

# Consumption in the winter period
for (i in 1:n)
{
  WinterDays <- day.weather$Date[day.weather$Temperature < 12]
  tmp <- HourData[[i]]$Consumption[,'Date']
  tmp_index <- sapply(tmp,function(x) x %in% WinterDays)
  Houravg[,i] <- colMeans(HourData[[i]]$Consumption[tmp_index,],na.rm = TRUE)[-1]
  Houravg[,i] <- Houravg[,i]/sum(Houravg[,i])
}
Houravg <- Houravg[c(2:24,1),]
winteravg <- Houravg

rownames(Houravg) <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
colnames(Houravg) <- c(1:n)
tt <- Houravg
image.plot(t(tt[rev(order(row.names(tt))),]), axes=FALSE, 
           lab.breaks=NULL,main = 'Average consumption of all houses (winter period)')
axis(2, at=seq(1+1/48,0-1/48, length=13), labels=c('00','02','04','06','08','10','12','14','16','18','20','22','24'), lwd=0.1, pos=-0.01,las=1)
abline(h=c(seq(1,0, length=24)+1/48),lwd=0.75)


library('reshape2')
tmp <- data.frame("cons"=Houravg)
tmp$id <- rownames(Houravg)
newdata <- melt(tmp)
model <- lm(value ~ id, data=newdata)

Houravg['06',]+Houravg['07',]+Houravg['08',]+Houravg['09',]

mcons_winter <-apply(winteravg,1,mean)
sum(mcons_winter[c(1:5,24)])

mcons_summer <-apply(summeravg,1,mean)
sum(mcons_summer[c(1:5,24)])

plot(mcons_winter,ylim=c(0.035,0.055),type='l',col="blue",cex.axis = 1,xaxt="n",ylab = "Fraction of daily consumption per hour",xlab = "Hour")
axis(side=1,at=c(0,6,12,18,24),labels=c("00","06","12","18","24"))
lines(mcons_summer,col='red')
legend(x="topright",legend=c("Winter period","Summer period"),lty=c(1,1),col =c("blue","red"))
for (i in 1:n)
{
plot(summeravg[,i],ylim=c(0,0.1),type='l',col="red")
lines(winteravg[,i],col='blue')
}

4/24

# Consumption in winter minus summer
for (i in 1:n)
{
  WinterDays <- day.weather$Date[day.weather$Temperature < 12]
  tmp <- HourData[[i]]$Consumption[,'Date']
  tmp_index <- sapply(tmp,function(x) x %in% WinterDays)
  Houravg[,i] <- colMeans(HourData[[i]]$Consumption[tmp_index,],na.rm = TRUE)[-1]
  SummerDays <- day.weather$Date[day.weather$Temperature >= 15]
  tmp <- HourData[[i]]$Consumption[,'Date']
  tmp_index <- sapply(tmp,function(x) x %in% SummerDays)
  Houravg[,i] <- Houravg[,i] - colMeans(HourData[[i]]$Consumption[tmp_index,],na.rm = TRUE)[-1]
  Houravg[,i] <- Houravg[,i]/sum(Houravg[,i])
}
Houravg <- Houravg[c(2:24,1),]

rownames(Houravg) <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
colnames(Houravg) <- c(1:n)
tt <- Houravg
image.plot(t(tt[rev(order(row.names(tt))),]), axes=FALSE, 
           lab.breaks=NULL,main = 'Average consumption of all houses (winter - summer)')
axis(2, at=seq(1+1/48,0-1/48, length=13), labels=c('00','02','04','06','08','10','12','14','16','18','20','22','24'), lwd=0.1, pos=-0.01,las=1)
abline(h=c(seq(1,0, length=24)+1/48),lwd=0.75)

head(tt)
ttn<-tt
for(i in 1:n){
  quantile(tt[,i])[4]
  ttn[,i]<-tt[,i]>quantile(tt[,i])[4]
}
  
head(ttn)

image.plot(t(ttn[rev(order(row.names(ttn))),]), axes=FALSE, 
           lab.breaks=NULL,main = 'Average consumption of all houses (winter - summer)')
axis(2, at=seq(1+1/48,0-1/48, length=13), labels=c('00','02','04','06','08','10','12','14','16','18','20','22','24'), lwd=0.1, pos=-0.01,las=1)
abline(h=c(seq(1,0, length=24)+1/48),lwd=0.75)

