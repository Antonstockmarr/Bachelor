setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if(!exists("n")){
  source("data.R")
}
model.data <- weatherCons
for (i in 1:n) {
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  model.data[[i]]<-model.tmp
}

cons.areal <- rep(-0.01,n)

for(i in 1:n){
  if(!is.na(BBR$Samlet.areal[i])){
    cons.areal[i] <- mean(model.data[[i]]$Consumption[!is.na(model.data[[i]]$Consumption)])/BBR$Samlet.areal[i]
  }
}
tmp<-1:n
tmp<- tmp[-10]
cons.areal<-cons.areal[tmp]
plot(tmp,cons.areal)
tmp2<-BBR$Hustype=="Parcel"
tmp2<-tmp2[-10]
points(tmp[tmp2],cons.areal[tmp2],col=2,pch=19)

match(max(cons.areal),cons.areal)

BBR[62,]
