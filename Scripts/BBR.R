par(mar=c(3,4,2,1), mgp=c(2,0.7,0))

model.data <- weatherCons
for (i in 1:n) {
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  model.data[[i]]<-model.tmp
}

No.days<-rep(0,n)
cons.areal <- rep(-0.01,n)

for(i in 1:n){
  if(!is.na(BBR$Samlet.areal[i])){
    cons.areal[i] <- mean(model.data[[i]]$Consumption[!is.na(model.data[[i]]$Consumption)])/BBR$Samlet.areal[i]
    No.days[i]<-length(model.data[[i]]$Date)
  }
}
tmp<-1:n
tmp<- tmp[-10]
cons.areal<-cons.areal[tmp]
No.days<-No.days[tmp]
plot(tmp,cons.areal)
tmp2<-BBR$Hustype=="Parcel"
tmp2<-tmp2[-10]
points(tmp[tmp2],cons.areal[tmp2],col=2,pch=19)


break.points<-as.numeric(quantile(cons.areal, c(.33,.67)))

Construction.Year <- tmp

for(i in 1:length(tmp)){
  if(!is.na(BBR$Ombygningsaar[tmp[i]])){
    Construction.Year[i]<-BBR$Ombygningsaar[tmp[i]]
  }else{
    Construction.Year[i]<-BBR$Byggeaar[tmp[i]]
  }
}

