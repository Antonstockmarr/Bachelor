###########################################################################
## Function for calculating the step response of the estimated transfer function of the given input
stepResponseARX <- function(fit,X,inputName="Te",outputName="Qi",ylim=NA,...)
{
    ## Sample period in hours
    sp <- as.double(diff(X$t[1:2]),units="hours")
    print(paste("Sample period in hours:",sp))
    ## Find the roots in the characteristic polynomial
    i <- grep(paste0("^",outputName),names(fit$coefficients))
    roots <- polyroot(rev(c(1,-fit$coefficients[i])))
    ## Stable system
    print(paste("Stable system if abs(roots) < 1: ", abs(roots)))
    ## Time constants in hours
    timeconstants <- sp * -1/log(abs(roots))
    ## Print it
    print(paste("Time constants in hours:",paste(format(timeconstants,digits=3),collapse=", "))) 
    
    
    ## Coefficients of the polynomials
    coefOutput <- fit$coefficients[grep(paste0("^",outputName),names(fit$coefficients))]
    coefInput <- fit$coefficients[grep(paste("^",inputName,sep=""),names(fit$coefficients))]
                                        # browser()
    ##
    input <- c(rep(0,10),rep(1,50))
    outputHat <- rep(0,length(input))
    for(i in 10:length(input))
    {
        ## The index from now and back
        outputHat[i] <- sum(c(coefOutput * outputHat[(i-1):(i-length(coefOutput))], coefInput * input[i:(i-length(coefInput)+1)]))
    }
    ## The x-axis
    tHours <- (1:length(input))*sp
    ## Plot
    if(is.na(ylim[1])) ylim <- range(input,outputHat)
    plot(tHours-(10*sp),input,ylim=ylim,main=paste("Step response for",inputName),xlab="hours",ylab="Input and output",type="s",...)
    lines(tHours-(10*sp),outputHat,col=2,type="s")
    legend("topright",c(inputName,"Qi"),lty=1,col=1:2,bg="white")
}
