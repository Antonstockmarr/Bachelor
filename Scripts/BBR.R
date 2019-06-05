# Using BBR-data

#Using consumption data from where the temperature is below 12 degrees.
model.data <- weatherCons
for (i in 1:n) {
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  model.data[[i]]<-model.tmp
}

# Number of days and consumption pr. areal is initialized.
No.days<-rep(0,n)
cons.areal <- rep(-0.01,n)

# And calculated
for(i in 1:n){
  if(!is.na(BBR$TotalArea[i])){
    cons.areal[i] <- mean(model.data[[i]]$Consumption[!is.na(model.data[[i]]$Consumption)])/BBR$TotalArea[i]
    No.days[i]<-length(model.data[[i]]$Date)
  }
}

# House number 10 has no areal in the BBR data, and thereby can't be used here.
tmp<-1:n
tmp<- tmp[-10]
cons.areal<-cons.areal[tmp]
No.days<-No.days[tmp]
plot(tmp,cons.areal)
tmp2<-BBR$HouseType=="Parcel"
tmp2<-tmp2[-10]
points(tmp[tmp2],cons.areal[tmp2],col=2,pch=19)


break.points<-as.numeric(quantile(cons.areal, c(.33,.67)))


# The last year of construction is saved for a plot.
Construction.Year <- tmp

for(i in 1:length(tmp)){
  if(!is.na(BBR$ReconstructionYear[tmp[i]])){
    Construction.Year[i]<-BBR$ReconstructionYear[tmp[i]]
  }else{
    Construction.Year[i]<-BBR$ConstructionYear[tmp[i]]
  }
}

