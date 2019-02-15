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
n <- length(file.names)
Datalengths = matrix(c(1,n),nrow=n)
cons <- vector(mode="list", length = n)

for(i in 1:n){
  if (i == 5){
    df.temp <- read.table(paste(data.path,file.names[i], sep = ""),sep="\t", stringsAsFactors=FALSE, header = TRUE)
 }
  else
  {
    df.temp <- read.table(paste(data.path,file.names[i], sep = ""),sep=";", stringsAsFactors=FALSE, header = TRUE)
    df.temp$X <- NULL
  }
    cons[[i]] <- df.temp
    names(cons[[i]])[1] = 'StartDateTime'
    Datalengths[i] = length(df.temp)
    
}
