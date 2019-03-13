rm(list=ls())
source("data.R")
source("Piecewise-opti.R")

# Plotting the first house
AnalyzeConsumption(houselist=64,hourly=FALSE,makeplot=TRUE)

# Plotting ALL houses
AnalyzeConsumption(houselist=1:n,makeplot=TRUE)

# Getting data from all houses without plotting
result <- AnalyzeConsumption(houselist=1:n,makeplot=FALSE)


# Plotting min inactive consumption
minQ = which(result[,3]==min(result[,3]))
AnalyzeConsumption(houselist = minQ,makeplot = TRUE)

# Plotting max inactive consumption
maxQ = which(result[,3]==max(result[,3]))
AnalyzeConsumption(houselist = maxQ,makeplot = TRUE)


# Plotting flow for the minQ house
plot(data[[minQ]]$Volume)


plot(data[[1]]$Flow, data[[1]]$Volume)



k=20
plot((4.186/3.6)*data[[k]]$Volume*(data[[k]]$TemperatureIn-data[[k]]$TemperatureOut)~data[[k]]$Energy)
plot((4.186/3.6)*data[[k]]$Flow*(data[[k]]$TemperatureIn-data[[k]]$TemperatureOut)~data[[k]]$Energy)


plot(TemperatureIn-TemperatureOut~CoolingDegree,data[[1]])

sum(abs(((data[[1]]$TemperatureIn-data[[1]]$TemperatureOut)-data[[1]]$CoolingDegree)))
