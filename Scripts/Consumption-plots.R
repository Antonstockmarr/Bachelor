source("data.R")
source("Piecewise-opti.R")

# Getting the weather and data for 1st house
tmp <- weather[(weather$StartDateTime <= EndDays[1]),]
tmp <- tmp[tmp$StartDateTime >= StartDays[1],]


# Defining subsets
temp <- tmp$Temperature
tempq <- data[[1]]$CoolingDegree*data[[1]]$Flow
tmp$Temperature[200] <- NA
result <- consumption_plot(temp,tempq)


InactiveQ = c(rep(NA,n))
for (i in 1:n)
{
  tmp <- weather[(weather$StartDateTime <= EndDays[i]),]
  tmp <- tmp[tmp$StartDateTime >= StartDays[i],]
  temp <- tmp$Temperature
  tempq <- data[[i]]$CoolingDegree*data[[i]]$Flow
  result <- consumption_plot(temp,tempq,makeplot=FALSE)
  InactiveQ[i]=result[1]
}
