rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")

# Farveeksempel
Wcol=c(1,rgb(132,202,41,maxColorValue = 255),rgb(231,176,59,maxColorValue = 255),rgb(229,56,50,maxColorValue = 255))
plot(data[[1]]$Flow,col=Wcol[2])

pairs(data[[1]])

plot(data[[1]]$Flow)
plot.months <- c("January", "February", "March", "April", "May", "June", "July", "August",
                 "September", "November", "December")

par(mfrow = c(1,1))
time <- seq(as.Date(StartDays[1]),as.Date(EndDays[1]), by = "1 mon")
plot(data[[1]]$StartDateTime, data[[1]]$Flow, "l", xaxt = 'n')
drawxaxis(data[[1]]$StartDateTime, tick.tstep="months")
axis.Date(1, at = seq(min(time), max(time), by = "12 mon"), format = "%m")

# Investigating each house's flow behaviour
for (i in 1:1){
  #data[[i]]$StartDateTime.new <- as.POSIXlt(data[[i]]$StartDateTime)
  #data[[i]]$StartDateTime <- as.Date(data[[i]]$StartDateTime, "%Y-%m-%d")
  #data[[i]]$StartDateTime2 <- as.Date(cut(data[[i]]$StartDateTime, breaks = "month"))
  plot(data[[i]]$StartDateTime, data[[i]]$Flow, type = "l", xlab ="Time", ylab = "Flow")
  axis(1, at = unique(months(data[[i]]$StartDateTime)), las = 2)
}

tmp <- weather[(weather$StartDateTime <= EndDays[1]),]
tmp <- tmp[tmp$StartDateTime >= StartDays[1],]

plot(tmp$Temperature,data[[1]]$CoolingDegree*data[[1]]$Flow)


lowtemp = tmp$Temperature[tmp$Temperature<15]
lowtempq = (data[[1]]$CoolingDegree*data[[1]]$Flow)[tmp$Temperature<15]
hightemp = tmp$Temperature[tmp$Temperature>=15]
hightempq = (data[[1]]$CoolingDegree*data[[1]]$Flow)[tmp$Temperature>=15]
plot(lowtemp,lowtempq, xlim=c(min(lowtemp),max(hightemp)))
