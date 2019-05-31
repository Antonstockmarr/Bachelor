TrainTest=function(Dataset,days){
  n=length(Dataset)
  Train=vector(mode="list", length = n)
  Test=vector(mode="list", length = n)
  for(i in 1:n){
    ni<-dim(Dataset[[i]])[1]
    Train[[i]]<-Dataset[[i]][1:(ni-days),]
    Test[[i]]<-Dataset[[i]][ni+(1:days),]
  }
  return(list(Train,Test))
}
