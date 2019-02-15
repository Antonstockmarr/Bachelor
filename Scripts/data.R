rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

library("data.table")
## Loading data
dat <- read.csv('../DistrictHeatingTestSample.csv', header=TRUE, sep = "\t")
head(dat)
str(dat)

## Loading all data
data.path = "../Watts_DistrictHeatingData_2018/"
cons <- data.table()
file.names <- dir(data.path, pattern =".csv")

for(i in 1:length(file.names)){
  
  df.temp <- read.table(paste(data.path,file.names[i], sep = ""),sep=";", stringsAsFactors=FALSE, header = TRUE)
  cons <- rbind(cons, df.temp)
  
}

