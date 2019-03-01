tmp <- weather[(weather$StartDateTime <= EndDays[1]),]
tmp <- tmp[tmp$StartDateTime >= StartDays[1],]

temp <- tmp$Temperature
tempq <- data[[1]]$CoolingDegree*data[[1]]$Flow
linfit <- lm(temp ~ tempq)
segfit <- segmented(linfit, seg.Z = ~tempq,psi=13.5)


plot(temp,tempq)
plot(segfit,add=T,col='red')
abline(v = segfit$psi[2], lty = 2, col = 2)

tempdivide = 13.5

lowtemp <- tmp$Temperature[tmp$Temperature<tempdivide]
lowtempq <- (data[[1]]$CoolingDegree*data[[1]]$Flow)[tmp$Temperature<tempdivide]
hightemp <- tmp$Temperature[tmp$Temperature>=tempdivide]
hightempq <- (data[[1]]$CoolingDegree*data[[1]]$Flow)[tmp$Temperature>=tempdivide]
plot(lowtemp,lowtempq, xlim=c(min(lowtemp),max(hightemp)),col=Wcol[3],xlab='Temperature',ylab='Q-values')
points(hightemp,hightempq, xlim=c(min(lowtemp),max(hightemp)),col=Wcol[2])
abline(v=tempdivide)
fit = lm(lowtempq~lowtemp)
segments(min(lowtemp),fit$coefficients[1]+min(lowtemp)*fit$coefficients[2],tempdivide,tempdivide*fit$coefficients[2]+fit$coefficients[1])
segments(tempdivide,tempdivide*fit$coefficients[2]+fit$coefficients[1],max(hightemp),tempdivide*fit$coefficients[2]+fit$coefficients[1])

