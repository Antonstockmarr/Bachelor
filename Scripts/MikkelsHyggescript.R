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
file.names <- dir(data.path, pattern =".csv")
DATA <- vector(mode="list",length=length(file.names))

for(i in 1:length(file.names)){
  df.temp <- read.csv(paste(data.path,file.names[i], sep = ""),sep=";", stringsAsFactors=FALSE, header = TRUE)
  if(dim(df.temp)[2]==1){
    df.temp <- read.csv(paste(data.path,file.names[i], sep = ""),sep="\t", stringsAsFactors=FALSE, header = TRUE)
  }
  DATA[[i]] <- df.temp
}

