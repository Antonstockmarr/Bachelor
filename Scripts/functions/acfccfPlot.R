acfccfPlot <- function(fit,X)
{
    ## Analyze the residuals
    eps <- fit$residuals
    ## Setup a 1x3 plot
    par(mfrow=c(1,3))
    ## The ACF
    acf(eps,lag.max=50,main="ACF(eps)")
    ## The cross-correlation function between the residuals and the external temperature
    ccf(eps,X[,"Te"],lag.max=100,xlim=c(1,50),ylab="CCF",main="CCF(eps,Te)",ylim=c(-0.5,0.5))
    ## The cross-correlation function between the residuals and the vertical global radiation
    ccf(eps,X[,"G"],lag.max=100,xlim=c(1,50),ylab="CCF",main="CCF(eps,G)",ylim=c(-0.5,0.5))
}
