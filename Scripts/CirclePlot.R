CirclePlot <- function(WindPred){
  # Making the costumer wind dependency plot.
  # Input: Prediction for each degree, with 33,3% prediction interval
  WindPred<-WindPred-min(WindPred)
  x<- -cos((1:360)/180*pi+pi/2)*WindPred$fit
  y<- sin((1:360)/180*pi+pi/2)*WindPred$fit
  CircleCol<-rep(Wcol[3],length(WindPred$fit))
  WindPredC<-WindPred-mean(WindPred$fit)
  CircleCol[WindPredC$upr<0]<-Wcol[2]
  CircleCol[WindPredC$lwr>0]<-Wcol[4]
  plot(x,y,col=CircleCol,yaxt='n',xaxt='n',xlab='',ylab='',pch=19)
  abline(v=0,h=0)
}
