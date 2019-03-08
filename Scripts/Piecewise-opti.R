newopti <- function(a)
{
  x <-(temp2<a)*(temp2-a)
  fit <- lm(tempq2 ~ 1+x)
  res <- sum(fit$residuals^2)
  res
}



# Given the temperature and consumption, make the piecewise optimization, plot it and return the breakpoint.
PiecewiseOpti <- function(i,temp,tempq,makeplot=FALSE)
  {
  bestpar <- optimize(f=newopti,c(5,20))
  x <-(temp<bestpar$minimum)*(temp-bestpar$minimum)
  fit <- lm(tempq ~ 1+x)
#  newx = seq(min(temp),max(temp),by = 0.05)
#  conf_interval <- predict(fit, newdata=data.frame(x=newx), interval="confidence",
#                           level = 0.95)
  if (makeplot==TRUE)
    {
      plot((fit$coefficients[1]+fit$coefficients[2]*x)~temp,ylab='Consumption',xlab='Temperature',main = paste('House number', i),col='red',type='l',ylim=c(0,max(tempq)))
      points(tempq~temp)
#      matlines(newx, conf_interval[,2:3], col = "blue", lty=2)
    }
  result = c(breakpoint = bestpar$minimum,CSlope = fit$coefficients[2], highTempC = fit$coefficients[1],dimnames=NULL)
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

