rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")
library(ggplot2) 
library(gridExtra)

# Watts colors
Wcol=c(1,rgb(132,202,41,maxColorValue = 255),rgb(231,176,59,maxColorValue = 255),rgb(229,56,50,maxColorValue = 255))
# Example using Watts colors
plot(data[[1]]$Flow,col=Wcol[2])

# Correlation between consumption and wind
plot(tmp$WindSpeed,data[[1]]$CoolingDegree*data[[1]]$Flow)
plot(tmp$WindDirection,data[[42]]$CoolingDegree*data[[42]]$Flow)


avgconsumption<-rep(0,difftime(max(EndDays),min(StartDays), units ="hours"))
weightavg<-rep(0,difftime(max(EndDays),min(StartDays), units ="hours"))


difftime(max(EndDays),min(StartDays), units ="hours")
avgconsumption[difftime(max(EndDays),min(StartDays), units ="hours")]

difftime(StartDays[1],min(StartDays), units ="hours")
difftime(EndDays[1],min(StartDays), units ="hours")

for (i in 1:n) {
  avgconsumption[difftime(StartDays[i],min(StartDays), units ="hours"):difftime(EndDays[i],min(StartDays), units ="hours")]<-
    avgconsumption[difftime(StartDays[i],min(StartDays), units ="hours"):difftime(EndDays[i],min(StartDays), units ="hours")]+
    data[[i]]$Flow*data[[i]]$CoolingDegree
  weightavg[difftime(StartDays[i]+1,min(StartDays), units ="hours"):difftime(EndDays[i]+1,min(StartDays), units ="hours")]<-
    weightavg[difftime(StartDays[i]+1,min(StartDays), units ="hours"):difftime(EndDays[i]+1,min(StartDays), units ="hours")]+
    rep(1,length(data[[1]]$Flow))
}

length(difftime(StartDays[i],min(StartDays), units ="hours"):difftime(EndDays[i],min(StartDays), units ="hours"))
length(data[[i]]$Flow*data[[i]]$CoolingDegree)

maxskip<-rep(0,n)
sumskip<-rep(0,n)
for (i in 1:n) {
  ntmp=length(data[[i]]$StartDateTime)
  tmpdiftest=difftime(data[[i]]$StartDateTime[1:ntmp-1],data[[i]]$StartDateTime[2:ntmp], units ="hours")
  maxskip[i]=max(tmpdiftest)
  if(max(tmpdiftest)>1){
    print(match(2:max(tmpdiftest),tmpdiftest))
  }
}

n69=length(data[[69]]$StartDateTime)
tmpdiftest=difftime(data[[69]]$StartDateTime[1:n69-1],data[[69]]$StartDateTime[2:n69], units ="hours")

# Investigating pairs plots 
day.tmp <- day.weather[(day.weather$Date <= as.Date(day.avg$Date[1],tz="GMT")),]
day.tmp <- day.tmp[day.tmp$Date >= as.Date(tail(day.avg$Date,1),tz="GMT"),]

# House pairs
{	
  pdf(file = "../figures/house_attri.pdf",width = 4.5,height = 2.8,pointsize = 9)
  par(mar=c(3,3,2,1), mgp=c(2,0.7,0))
  pairs(day.avg[c('Date','Energy','Flow','Volume','TemperatureIn','TemperatureOut','CoolingDegree','Consumption')])
  dev.off()
}

# Weather pairs
{
  pdf(file = "../figures/weather_cons_focus.pdf",width = 4.5,height = 2.8,pointsize = 9)
  par(mar=c(3,3,2,1), mgp=c(2,0.7,0)) 
  pairs(c(day.avg['Consumption'], day.tmp[c('Date','Temperature','WindSpeed','WindDirection','SunHour','Condition','UltravioletIndex','MeanSeaLevelPressure','DewPoint','PrecipitationProbability')]))
  dev.off()
}

# Possible multicolinarity between Temperature and DewPoint
cor(day.tmp['Temperature'],day.tmp['DewPoint'])
# = 0.936

# Removing DewPoint
weather$DewPoint <- NULL
day.weather$DewPoint <- NULL

# Possible multicolinarity between Condition and PrecipitationProbability
cor(day.tmp['PrecipitationProbability'],day.tmp['Condition'])
# Not enough

# Possible multicolinarity between UltravioletIndex and Temperature
cor(day.tmp['Temperature'],day.tmp['UltravioletIndex'])
# 0.823 - close, but not enough

# Average consumption for all houses during a year
avg.plot1 <- ggplot(data = day.avg, mapping = aes(Date, Consumption)) + geom_point() +
  ggtitle("Average consumption for all houses during a year ") + xlab("Time") + 
  ylab("Average consumption (kwh)") +
  geom_smooth(col=Wcol[2], se = T)

# Selected houses based on wether they follow the trend
day.plot.flot <- ggplot(data = day.data[[18]], mapping = aes(Date, (CoolingDegree*Volume))) + geom_point() +
        ggtitle(paste("Daily consumption for house 18")) + xlab("Time") + 
        ylab("Daily consumption (kwh)") +
        geom_smooth(col=Wcol[2], se = T)

day.plot.gak <- ggplot(data = day.data[[42]], mapping = aes(Date, (CoolingDegree*Volume))) + geom_point() +
        ggtitle(paste("Daily consumption for house 42")) + xlab("Time") + 
        ylab("Daily consumption (kwh)") +
        geom_smooth(col=Wcol[2], se = T)
{
  pdf(file = "../figures/daily_cons.pdf",width = 4.5,height = 2.8,pointsize = 9)
  par(mar=c(3,3,2,1), mgp=c(2,0.7,0)) 
  grid.arrange(avg.plot1, day.plot.flot, day.plot.gak, nrow = 3)
  dev.off()
}

# Daily consumption for the 69 houses
for (i in 1:n) {
  if (length(day.data[[i]]$Flow) > 365) {
    print(ggplot(data = day.data[[i]], mapping = aes(Date, (CoolingDegree*Volume),color=Holiday)) + geom_point() +
      ggtitle(paste("Daily consumption for house ", i)) + xlab("Time") + 
      ylab("Average consumption (kwh)") +
      geom_smooth(col=Wcol[2], se = T))
  }
}

# Heat map
heatmap(day.data[[1]]$Volume*day.data[[1]]$CoolingDegree)
