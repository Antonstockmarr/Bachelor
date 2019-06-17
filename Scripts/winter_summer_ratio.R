weathertmp <- day.weather[day.weather$Date >= tail(day.avg$Date,1),]
weathertmp <- weathertmp[weathertmp$Date <= head(day.avg$Date,1),]
tmpdata <- day.avg[weathertmp$Temperature <=12,]

winterdays <- (dim(tmpdata)[1])
winteravg <- sum(tmpdata$Volume*tmpdata$CoolingDegree)/winterdays


tmpdata <- day.avg[weathertmp$Temperature >=15,]

summerdays <-(dim(tmpdata)[1])
summeravg <- sum(tmpdata$Volume*tmpdata$CoolingDegree)/summerdays


winterratio <- winteravg/summeravg

tapwater <- summeravg*356

total_cons <- sum(day.avg$Volume*day.avg$CoolingDegree)

weathertmp <- day.weather[day.weather$Date >= tail(day.avg$Date,1),]
weathertmp <- weathertmp[weathertmp$Date <= head(day.avg$Date,1),]
plot(day.avg$Volume*day.avg$CoolingDegree, col = 1+c(weathertmp$Temperature>=15)+2*c(weathertmp$Temperature<12))
abline(h=c(summeravg,winteravg,total_cons/356))

tapwater_ratio <-tapwater/total_cons
tapwater_ratio_winter <- summeravg/winteravg


plot(data[[18]]$Volume*data[[18]]$CoolingDegree)
plot(weatherCons[[18]]$Consumption)

warmt <-weatherCons[[55]]$Temperature[weatherCons[[55]]$Temperature>=12]
warm <-weatherCons[[55]]$Consumption[weatherCons[[55]]$Temperature>=12]
coldt <-weatherCons[[55]]$Temperature[weatherCons[[55]]$Temperature<12]
cold <-weatherCons[[55]]$Consumption[weatherCons[[55]]$Temperature<12]
plot(coldt,cold,xlab='Temperature',ylab='Consumption', main='Consumption vs. Temperature (house 55)',ylim=c(0,90),xlim=c(-10,27),col=Wcol[2])
points(warmt,warm,col=Wcol[4])
abline(v=c(12),col=Wcol[3])
