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
  ggtitle("Average consumption for all houses during a year ") + xlab("Time [day]") + 
  ylab("Avg. consumption [kWh]") +
  geom_smooth(col=Wcol[2], se = T)

# Selected houses based on wether they follow the trend
day.plot.gak <- ggplot(data = day.data[[18]], mapping = aes(Date, (CoolingDegree*Volume))) + geom_point() +
  ggtitle(paste("Daily consumption for house 18")) + xlab("Time [day]") + 
  ylab("Daily consumption [kWh]") + 
  geom_smooth(col=Wcol[2], se = T)

day.plot.flot <- ggplot(data = day.data[[55]], mapping = aes(Date, (CoolingDegree*Volume))) + geom_point() +
  ggtitle(paste("Daily consumption for house 55")) + xlab("Time [day]") + 
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
            ggtitle(paste("Daily consumption for house ", i)) + xlab("Time [day]") + 
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
GGally::ggpairs(day.avg[c('Date','Energy','Flow','Volume','TemperatureIn','TemperatureOut','CoolingDegree','Consumption')])
pairs(day.avg[c('Consumption', 'Flow','Volume','TemperatureIn','TemperatureOut','CoolingDegree')])

plot1 <- ggplot(data = day.avg, aes(x = Flow, y= Consumption)) + geom_point() + ylab("Consumption")
plot2 <- ggplot(data = day.avg, aes(x = Volume, y= Consumption)) + geom_point() + ylab("Consumption")
plot3 <- ggplot(data = day.avg, aes(x = TemperatureIn, y= Consumption)) + geom_point() + ylab("Consumption")
plot4 <- ggplot(data = day.avg, aes(x = TemperatureOut, y= Consumption)) + geom_point() + ylab("Consumption")
plot5 <- ggplot(data = day.avg, aes(x = CoolingDegree, y= Consumption)) + geom_point() + ylab("Consumption")
grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 3, ncol = 2)

# Weather pairs
par(mar=c(3,3,2,1), mgp=c(2,0.7,0)) 
pairs(c(day.avg['Consumption'], day.tmp[c('Date','Temperature','WindSpeed','WindDirection','SunHour','Condition','MeanSeaLevelPressure','DewPoint','PrecipitationProbability')]))
GGally::ggpairs(c(day.avg['Consumption'], day.tmp[c('Date','Temperature','WindSpeed','WindDirection','SunHour','Condition','MeanSeaLevelPressure','DewPoint','PrecipitationProbability')]))

plot6 <- ggplot(data = day.tmp, aes(x = Temperature, y= Consumption)) + geom_point() + ylab("Consumption")
plot7 <- ggplot(data = day.tmp, aes(x = WindSpeed, y= Consumption)) + geom_point() + ylab("Consumption")
plot8 <- ggplot(data = day.tmp, aes(x = WindDirection, y= Consumption)) + geom_point() + ylab("Consumption")
plot9 <- ggplot(data = day.tmp, aes(x = MeanSeaLevelPressure, y= Consumption)) + geom_point() + ylab("Consumption")
plot10 <- ggplot(data = day.tmp, aes(x = DewPoint, y= Consumption)) + geom_point() + ylab("Consumption")
plot11 <- ggplot(data = day.tmp, aes(x = Radiation, y= Consumption)) + geom_point() + ylab("Consumption")
plot12 <- ggplot(data = day.tmp, aes(x = SunHour, y= Consumption)) + geom_point() + ylab("Consumption")
grid.arrange(plot6, plot7, plot8, plot9, plot10, plot11, plot12, nrow = 5, ncol = 2)


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
plot12 <- ggplot(data = BBR, aes(x = TotalArea)) + geom_histogram(bins = 15)
plot13 <- ggplot(data = BBR, aes(x = ConstructionYear)) + geom_histogram(bins = 15)
plot14 <- ggplot(data = BBR, aes(x = ReconstructionYear)) + geom_histogram(bins = 15)
grid.arrange(plot11, plot12, plot13, plot14, nrow = 2, ncol = 2)

par(mar=c(3,4,2,1), mgp=c(2,0.7,0))
plot(Construction.Year,cons.areal,col=Wcol[2],main='Year of Construction Consumption',xlab='Year of Construction',ylab = expression(paste("Consumption pr.  ", m^2, sep = "")))
points(Construction.Year[cons.areal>break.points[1]],cons.areal[cons.areal>break.points[1]],col=Wcol[3])
points(Construction.Year[cons.areal>break.points[2]],cons.areal[cons.areal>break.points[2]],col=Wcol[4])
legend('topright', legend = c('Highest consumption', 'Middle consumption', 'Lowest consumption'), col = c(Wcol[4],Wcol[3],Wcol[2]), pch = 1, bty = 'n')

