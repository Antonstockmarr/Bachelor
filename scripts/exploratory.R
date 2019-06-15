rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")
source("BBR.R")
library(ggplot2) 
library(gridExtra)
library(tidyverse)
library(grid)


# Watts colors
Wcol=c(1,rgb(132,202,41,maxColorValue = 255),rgb(231,176,59,maxColorValue = 255),rgb(229,56,50,maxColorValue = 255))
# Example using Watts colors
plot(data[[1]]$Flow,col=Wcol[2])


# Average consumption -----------------------------------------------------
# Average consumption for all houses during a year
avg.plot1 <- ggplot(data = day.avg, mapping = aes(Date, Consumption)) + geom_point() +
  ggtitle("Average consumption for all houses during a year ") + xlab("Time [days]") + 
  ylab("Avg. consumption [kWh]") +
  geom_smooth(col=Wcol[2], se = T)

# Selected houses based on wether they follow the trend
day.plot.gak <- ggplot(data = day.data[[18]], mapping = aes(Date, (CoolingDegree*Volume))) + geom_point() +
  ggtitle(paste("Daily consumption for house 18")) + xlab("Time [days]") + 
  ylab("Daily consumption [kWh]") + 
  geom_smooth(col=Wcol[2], se = T)

day.plot.flot <- ggplot(data = day.data[[55]], mapping = aes(Date, (CoolingDegree*Volume))) + geom_point() +
  ggtitle(paste("Daily consumption for house 55")) + xlab("Time [days]") + 
  ylab("Daily consumption [kWh]") +
  geom_smooth(col=Wcol[2], se = T)
{
  pdf(file = "../figures/daily_cons.pdf",width = 4.5,height = 2.8,pointsize = 9)
  par(mar=c(3,3,2,1), mgp=c(2,0.7,0)) 
  grid.arrange(avg.plot1, day.plot.gak, day.plot.flot, nrow = 3)
  dev.off()
  }

# Daily consumption for the n houses
for (i in 1:n) {
  if (length(day.data[[i]]$Flow) > 365) {
    print(ggplot(data = day.data[[i]], mapping = aes(Date, (CoolingDegree*Volume),color=Holiday)) + geom_point() +
            ggtitle(paste("Daily consumption for house ", i)) + xlab("Time [days]") + 
            ylab("Average consumption [kWh]") +
            geom_smooth(col=Wcol[2], se = T))
  }
}


# Pairs plots -------------------------------------------------------------
# Investigating pairs plots 
day.tmp <- day.weather[(day.weather$Date <= as.Date(day.avg$Date[1],tz="GMT")),]
day.tmp <- day.tmp[day.tmp$Date >= as.Date(tail(day.avg$Date,1),tz="GMT"),]
day.tmp$Consumption <- day.avg$Consumption

# House pairs
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))
pairs(day.avg[c('Date','Energy','Flow','Volume','TemperatureIn','TemperatureOut','CoolingDegree','Consumption')])
pairs(day.avg[c('Consumption', 'Flow','Volume','TemperatureIn','TemperatureOut','CoolingDegree')])

plot1 <- ggplot(data = day.avg, aes(x = Flow, y= Consumption)) + geom_point() + xlab(expression(paste("Flow [",m^3/"hour]", sep=""))) + ylab("Consumption [kWh]")
plot2 <- ggplot(data = day.avg, aes(x = Volume, y= Consumption)) + geom_point() + xlab(expression(paste("Volume [",m^3,"]", sep=""))) + ylab("Consumption [kWh]")
plot3 <- ggplot(data = day.avg, aes(x = TemperatureIn, y= Consumption)) + geom_point() + xlab(expression(paste("TemperatureIn [",degree,"C]", sep=""))) + ylab("Consumption [kWh]")
plot4 <- ggplot(data = day.avg, aes(x = TemperatureOut, y= Consumption)) + geom_point() + xlab(expression(paste("TemperatureOut [",degree,"C]", sep=""))) + ylab("Consumption [kWh]")
plot5 <- ggplot(data = day.avg, aes(x = CoolingDegree, y= Consumption)) + geom_point() + xlab(expression(paste("CoolingDegree [",degree,"C]", sep=""))) + ylab("Consumption [kWh]")
grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 3, ncol = 2)

# Weather pairs
par(mar=c(3,3,2,1), mgp=c(2,0.7,0)) 
pairs(c(day.avg['Consumption'], day.tmp[c('Date','Temperature','WindSpeed','WindDirection','SunHour','Condition','MeanSeaLevelPressure','DewPoint','PrecipitationProbability')]))

plot6 <- ggplot(data = day.tmp, aes(x = Temperature, y= Consumption)) + geom_point() + xlab(expression(paste("Temperature [",degree,"C]", sep=""))) + ylab("Consumption [kWh]")
plot7 <- ggplot(data = day.tmp, aes(x = WindSpeed, y= Consumption)) + geom_point() +  xlab(expression(paste("WindSpeed [",m/"s]", sep=""))) + ylab("Consumption [kWh]")
plot8 <- ggplot(data = day.tmp, aes(x = WindDirection, y= Consumption)) + geom_point() + xlab(expression(paste("WindDirection [",degrees,"]", sep=""))) + ylab("Consumption [kWh]")
plot9 <- ggplot(data = day.tmp, aes(x = MeanSeaLevelPressure, y= Consumption)) + geom_point() + xlab(expression(paste("MeanSeaLevelPressure [",mbar,"]", sep=""))) + ylab("Consumption [kWh]")
plot10 <- ggplot(data = day.tmp, aes(x = DewPoint, y= Consumption)) + geom_point() + xlab(expression(paste("DewPoint [",degree,"C]", sep=""))) + ylab("Consumption [kWh]")
plot11 <- ggplot(data = day.tmp, aes(x = Radiation, y= Consumption)) + geom_point() + xlab(expression(paste("SolarRadiation [W"/m^2,"]", sep=""))) + ylab("Consumption [kWh]")
plot12 <- ggplot(data = day.tmp, aes(x = SunHour, y= Consumption)) + geom_point() + xlab(expression(paste("SunHour [",hours,"]", sep=""))) + ylab("Consumption [kWh]")
grid.arrange(plot6, plot7, plot8, plot9, plot10, plot11, plot12, nrow = 4, ncol = 2)


# Sun pairs
GGally::ggpairs(day.weather[c('SunHour','Condition','Radiation')])
# Removing SunHour
weather$SunHour <- NULL
day.weather$SunHour <- NULL

# Possible multicolinarity between Temperature and DewPoint
cor(day.tmp['Temperature'],day.tmp['DewPoint'])
# = 0.936
# Removing DewPoint
weather$DewPoint <- NULL
day.weather$DewPoint <- NULL

# Possible multicolinarity between Condition and PrecipitationProbability
cor(day.tmp['PrecipitationProbability'],day.tmp['Condition'])
# Not enough


# Investigating BBR data --------------------------------------------------
HouseType <- c(BBR$HouseType)
legend11 <- "Type 1: Industry \n Type 2: Public \n Type 3: Apartment \n Type 4: Parcel \n Type 5: Terrace"
my_grob = grid.text(legend11, x=0.1,  y=0.8, gp=gpar(col="black", fontsize=8, fontface="bold"), just = "left")
plot11 <- ggplot(data = data.frame(HouseType), aes(x = HouseType)) + geom_histogram(bins = 15) + theme(legend.position = "topleft",legend.direction = "horizontal") + annotation_custom(my_grob)
plot12 <- ggplot(data = BBR, aes(x = TotalArea)) + geom_histogram(bins = 15) + xlab(expression(paste("TotalArea [",m^2,"]", sep="")))
plot13 <- ggplot(data = BBR, aes(x = ConstructionYear)) + geom_histogram(bins = 15)
plot14 <- ggplot(data = BBR, aes(x = ReconstructionYear)) + geom_histogram(bins = 15)
grid.arrange(plot11, plot12, plot13, plot14, nrow = 2, ncol = 2)

par(mar=c(3,4,2,1), mgp=c(2,0.7,0))
plot(Construction.Year,cons.areal,col="hotpink",pch = 19,xlab='Year of Construction',ylab = expression(paste("Consumption [",kWh/m^2,"]", sep = "")))
points(Construction.Year[cons.areal>break.points[1]],cons.areal[cons.areal>break.points[1]],col="hotpink", pch = 19)
points(Construction.Year[cons.areal>break.points[2]],cons.areal[cons.areal>break.points[2]],col="hotpink", pch = 19)
