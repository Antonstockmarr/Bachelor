newopti <- function(a)
{
  x <-(temp2<a)*(temp2-a)
  fit <- lm(tempq2 ~ 1+x)
  res <- sum(fit$residuals^2)
  res
}



# Given the temperature and consumption, make the piecewise optimization, plot it and return the breakpoint.
PiecewiseOpti <- function(i,t,q,makeplot=FALSE)
  {
  bestpar <- optimize(f=newopti,c(5,20))
  a <- bestpar$minimum
  fit <- lm(q ~ 1+I((t<a)*(t-a)))
  newdata <- seq(min(t),max(t),length=30)
  conf_interval <- predict(fit, newdata=data.frame(t=newdata), interval='confidence')
  
  if (makeplot==TRUE)
    {
      plot(newdata,conf_interval[,1],ylab='Daily consumption [kWh]',xlab=expression(paste('Temperature [',degree, 'C]')), lwd=2,
           main = paste('House number', i),col=Wcol[4],type='l',ylim=c(0,max(q)))
      points(q~t)
      lines(newdata,conf_interval[,2],col=Wcol[2],lwd=2,lty='dashed')
      lines(newdata,conf_interval[,3],col=Wcol[2],lwd=2,lty='dashed')
      #legend(x='topright',legend = c('Fitted line','Data points','Confidence interval'),lty=c(1,NA,'dashed'),pch=c(NA,1,NA),col=c('red','black','green'))
    }
  result = c(breakpoint = a,CSlope = fit$coefficients[2], highTempC = fit$coefficients[1],dimnames=NULL)
  }



AnalyzeConsumption <- function(houselist,hourly=FALSE,makeplot=FALSE)
{
  n = length(houselist)
  breakpoint = c(rep(NA,n))
  CSlope = c(rep(NA,n))
  highTempC = c(rep(NA,n))
  for (i in houselist)
  {
    if (hourly==TRUE)
    {
      tmp <- weather[(weather$ObsTime <= EndDays[i]),]
      tmp <- tmp[tmp$ObsTime >= StartDays[i],]
      temp <- tmp$Temperature
      tempq <- data[[i]]$CoolingDegree*data[[i]]$Volume
      # Removing NA's
      assign("temp2", value = temp[!(is.na(tempq))], envir = parent.frame())
      assign("tempq2", value = tempq[!(is.na(tempq))], envir = parent.frame())
    }
    else
    {
      day.tmp <- day.weather[(day.weather$Date <= as.Date(EndDays[i],tz="GMT")),]
      day.tmp <- day.tmp[day.tmp$Date >= as.Date(StartDays[i],tz="GMT"),] 
      temp <- day.tmp$Temperature
      tempq <- day.data[[i]]$CoolingDegree*day.data[[i]]$Volume
      # Removing NA's
      assign("temp2", value = temp[!(is.na(tempq))], envir = parent.frame())
      assign("tempq2", value = tempq[!(is.na(tempq))], envir = parent.frame())
    }
      result <- PiecewiseOpti(i,temp2,tempq2,makeplot)
      breakpoint[i]=result[1]
      CSlope[i]=result[2]
      highTempC[i]=result[3]
  }
  
  rm(temp2,tempq2,envir=parent.frame())
  returnData <- data.frame(breakpoint=breakpoint,CSlope=CSlope,highTempC=highTempC)
}

