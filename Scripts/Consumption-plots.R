rm(list=ls())
source("data.R")
source("Piecewise-opti.R")

# Plotting the first house
AnalyzeConsumption(data,houselist=1,makeplot=TRUE)

# Plotting ALL houses
AnalyzeConsumption(data,houselist=1:n,makeplot=TRUE)

# Getting data from all houses without plotting
result <- AnalyzeConsumption(data,houselist=1:n,makeplot=FALSE)


# Plotting min inactive consumption
minQ = which(result[,1]==min(result[,1]))
AnalyzeConsumption(data,houselist = minQ,makeplot = TRUE)

# Plotting max inactive consumption
maxQ = which(result[,1]==max(result[,1]))
AnalyzeConsumption(data,houselist = maxQ,makeplot = TRUE)


# Plotting flow for the minQ house
plot(data[[minQ]]$Flow)
