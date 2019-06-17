resample <- function(object, ...){
    UseMethod("resample")
}

resample.data.frame <- function(object, ts, tstart, tend=NA, timename="t", includeNA=TRUE, quantizetime=TRUE, rmNAinmean=FALSE)
{
    X <- object
    ###########################################################################
    ## Do nothing if ts is NA
    if(is.na(ts)){ return(X) }
    
    ###########################################################################
    ## Resample a dataframe.
    ## It is assumed that the time points represent the past period.
    ## The start time is the left limit of the first sample period.

    ###########################################################################
    ## Test values
    #(X <- C2[c(1:20,40:50),])
    #X[3,4] <- NA
    #ts=3600
    #timename <- "t"
    #includeNA <- TRUE
    #tstart= round(C2[1,"t"],units="hours")#asP("2009-1-1")
    #tend <- asP("2008-02-05 04:00:00")

    ###########################################################################
    ## Convert to POSIXct
    tstart <- asp(tstart)
    tend <- asp(tend)

    ###########################################################################
    ## The start time has to be given.
##    if(is.na(tstart)){ tstart <- X[1,timename]}
    ## set the end time if not specified
    if(is.na(tend)){ tend <- X[nrow(X),timename]}
    
    ###########################################################################
    ## Cut out the time period
    X <- X[tstart<X[,timename] & X[,timename]<=tend,]
    ## Remove values with a NA value in time
    X <- X[!is.na(X[,timename]), ]
    
    ###########################################################################
    ## Split into periods of length ts, and take the mean of each period
    X[,timename] <- (as.numeric(X[,timename], units="secs")-as.numeric(tstart, units="secs"))
    iSplit <- -(X[,timename] %/% -ts)
    ## Do the resampling
    Xres <- aggregate(X, list(iSplit), mean, na.rm = rmNAinmean)
    ## Remove the "Group" column
    Xres <- Xres[,-1]
    ## Convert time to POSIXct
    Xres[,timename] <- tstart + Xres[,timename]

    if(includeNA)
      {
        ## Include intervals with NA in the result
        Xres <- cbind(Xres,iSplit=unique(iSplit))
        iSplit <- 1:-((as.numeric(tend, units="secs")-as.numeric(tstart, units="secs")) %/% -ts)
        withNA <- data.frame(iSplit=iSplit)
        Xres <- merge(Xres,withNA,all=TRUE)
        ## Remove the iSplit column
        Xres <- Xres[,-match("iSplit",names(Xres))]
        if(quantizetime)
          {
            ## Set the time points to the end of each interval
            time <- seq(tstart,by=ts,length.out=nrow(Xres)) + ts
            Xres[,timename] <- time
          }
      }
    
    return(Xres)
  }
