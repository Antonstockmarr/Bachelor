rm(list=ls())
source("data.R")
source("Piecewise-opti.R")

# Getting the weather and data for 1st house
tmp <- weather[(weather$StartDateTime <= EndDays[1]),]
tmp <- tmp[tmp$StartDateTime >= StartDays[1],]


# Defining subsets
temp <- tmp$Temperature
tempq <- data[[1]]$CoolingDegree*data[[1]]$Flow

# Removing NA's
temp2 <- temp#[!(is.na(tempq))]
tempq2 <- tempq#[!(is.na(tempq))]

result <- consumption_plot(temp2,tempq2)


InactiveQ = c(rep(NA,n))
InactiveTemp = c(rep(NA,n))
for (i in 1:n)
{
  tmp <- weather[(weather$StartDateTime <= EndDays[i]),]
  tmp <- tmp[tmp$StartDateTime >= StartDays[i],]
  temp <- tmp$Temperature
  tempq <- data[[i]]$CoolingDegree*data[[i]]$Flow
  # Removing NA's
  temp2 <- temp[!(is.na(tempq))]
  tempq2 <- tempq[!(is.na(tempq))]
  result <- consumption_plot(temp2,tempq2)
  InactiveQ[i]=result[3]
  InactiveTemp[i]=result[1]
}


