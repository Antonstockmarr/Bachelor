## Function for making a matrix of lagged signals up to lags from a vector
## or lagging a matrix
lagMat <- function(x,lag)
{
  ## For lagging vectors
  lg <- function(x,lag)
    {
      if(lag>0){ return(c(rep(NA,lag),x[1:(length(x)-lag)]))}
      else if(lag<0){ return(c(x[(abs(lag)+1):length(x)],rep(NA,abs(lag)))) }
      else if(lag==0){ return(x) }  
    }

  if(is.null(dim(x)))
    {
      ## x is a vector
      sapply(lag, function(k){lg(x,k)})
    }
  else
    {
      ## x is a matrix, lag it
      if(lag > 0)
        {
          x[(lag+1):nrow(x), ] <-  x[1:(nrow(x)-lag), ]
          x[1:lag,] <- NA
          return(x)
        }
      else if(lag < 0)
        {
          lag <- -lag 
          x[1:(nrow(x)-lag), ] <-  x[(lag+1):nrow(x), ]
          x[(nrow(x)-lag+1):nrow(x), ] <- NA
          return(x)
        }
      else if(lag==0){ return(x) }
    }
}
