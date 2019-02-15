rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

library("data.table")

# Loading all data
data.path = "../Watts_DistrictHeatingData_2018/"
file.names <- dir(data.path, pattern =".csv")
n <- length(file.names)
Datalengths = rep(c(1,n),nrow=n)
data <- vector(mode="list", length = n)

for(i in 1:n){
  if (i == 5){
    dt.tmp <- read.table(paste(data.path,file.names[i], sep = ""), sep="\t", stringsAsFactors=FALSE, header = TRUE, dec=',')
    names(dt.tmp)[1] = 'StartDateTime'
    dt.tmp$StartDateTime <- strptime(dt.tmp$StartDateTime, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
    dt.tmp$EndDateTime <- strptime(dt.tmp$EndDateTime, format = "%d/%m/%Y %H.%M", tz = "GMT")
 }
  else
  {
    dt.tmp <- read.table(paste(data.path,file.names[i], sep = ""), sep=";", stringsAsFactors=FALSE, header = TRUE, dec=',')
    dt.tmp$X <- NULL
    names(dt.tmp)[1] = 'StartDateTime'
    dt.tmp$StartDateTime <- strptime(dt.tmp$StartDateTime, format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
    dt.tmp$EndDateTime <- strptime(dt.tmp$EndDateTime, format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
  }
    Datalengths[i] = length(dt.tmp)
    data[[i]] <- dt.tmp
    
   
}

# Reading weather data  
weather <- read.table('../WeatherData_01-01-2018_09-05-2019.csv', sep="\t", stringsAsFactors=FALSE, header = TRUE, dec=',')
str(weather)
weather$IsHistoricalEstimated <- type.convert(weather$IsHistoricalEstimated, as.is = TRUE)

Weather <- read.table('../WeatherData_01-01-2018_09-05-2019.csv', sep="\t", stringsAsFactors=FALSE, header = TRUE, dec=',')

Weather$StartDateTime = strptime(Weather$StartDateTime,format='%d/%m/%Y %H.%M',tz = 'GMT')

rm(i,n,file.names,data.path,dt.tmp,Datalengths)
