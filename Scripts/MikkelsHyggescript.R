setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("data.R")

cons.areal <- rep(-0.01,n)

for(i in 1:n){
  if(!is.na(BBR$Samlet.areal[i]))
  cons.areal[i] <- mean(weatherCons[[i]]$Consumption[!is.na(weatherCons[[i]]$Consumption)])/BBR$Samlet.areal[i]
}
plot(cons.areal)

match(max(cons.areal),cons.areal)
