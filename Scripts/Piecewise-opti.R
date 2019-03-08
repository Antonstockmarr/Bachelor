# Computing estimates 
fun <- function(par,x)
{
  # The first intercept
  y1 <- x^0 * par["i1"]
  # After breakpoint should be constant
  y1[x >= par["x1"]] <- par["i2"]
  # Should interpolate between intercept and breakpoint.
  r <- x < par["x1"]
  y1[r] <- par["i1"] - (par["i1"] - par["i2"])/(par["x1"] - x[1]) * (x[r] - x[1])
  y1
}

newopti <- function(a)
{
  x <-(temp2<a)*(temp2-a)
  fit <- lm(tempq2 ~ 1+x)
  res <- sum(fit$residuals^2)
  res
}

SSR <- function(par) {
  sum((tempq2 - fun(par,temp2))^2)
}


# Given the temperature and consumption, make the piecewise optimization, plot it and return the breakpoint.
PiecewiseOpti <- function(i,temp,tempq,makeplot=FALSE){


bestpar <- optimize(f=newopti,c(5,20))

x <-(temp2<bestpar$minimum)*(temp2-bestpar$minimum)
fit <- lm(tempq2 ~ 1+x)


#bestpar <- optimx(par = c(x1 = 13.5, i1 = 6.5, i2 = 3), 
#         fn = SSR, 
#         method = "Nelder-Mead")
  
if (makeplot==TRUE)
{
  plot((fit$coefficients[1]+fit$coefficients[2]*x)~temp2,ylab='Consumption',xlab='Temperature',main = paste('House number', i),col='red',type='l',ylim=c(0,max(tempq2)))
  points(tempq2~temp2)
  
#  plot(temp2,tempq2,ylab='Consumption',xlab='Temperature',main = paste('House number', i))
#  lines(seq(ceiling(min(temp2)),floor(max(temp2)),by=0.01), 
#        fun(c(x1 = bestpar$x1, i1 = bestpar$i1, i2 = bestpar$i2), seq(ceiling(min(temp2)),floor(max(temp2)),by=0.01)),col='red')
}


result = c(breakpoint = bestpar$minimum,ConsumptionSlope = fit$coefficients[2], highTempQ = fit$coefficients[1])
}



AnalyzeConsumption <- function(houselist,onlyDay=FALSE,onlyWinter=FALSE,makeplot=FALSE)
{
  n = length(houselist)
  InactiveQ = c(rep(NA,n))
  InactiveTemp = c(rep(NA,n))
  for (i in houselist)
  {
    tmp <- weather[(weather$ObsTime <= EndDays[i]),]
    tmp <- tmp[tmp$ObsTime >= StartDays[i],]
    temp <- tmp$Temperature
    tempq <- data[[i]]$CoolingDegree*data[[i]]$Flow
    # Removing NA's
    assign("temp2", value = temp[!(is.na(tempq))], envir = parent.frame())
    assign("tempq2", value = tempq[!(is.na(tempq))], envir = parent.frame())
    result <- PiecewiseOpti(i,temp2,tempq2,makeplot)
    InactiveQ[i]=result[3]
    InactiveTemp[i]=result[1]
  }
  
  rm(temp2,tempq2,envir=parent.frame())
  returnData <- data.frame(InactiveQ=InactiveQ,InactiveTemp=InactiveTemp)
}

