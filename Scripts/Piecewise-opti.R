library(optimx)
# Computing estimates 
fun <- function(par,x)
{
  # The first intercept
  y1 <- x^0 * par['i1']
  # After breakpoint should be constant
  y1[x >= par["x1"]] <- par["i2"]
  # Should interpolate between intercept and breakpoint.
  r <- x < par["x1"]
  y1[r] <- par["i1"] - (par['i1'] - par['i2'])/(par['x1'] - x[1]) * (x[r] - x[1])
  y1
}

SSR <- function(par) {
  sum((tempq2 - fun(par,temp2))^2)
}


# Given the temperature and consumption, make the piecewise optimization, plot it and return the breakpoint.
consumption_plot <- function(temp,tempq,makeplot=FALSE){


bestpar <- optimx(par = c(x1 = 13.5, i1 = 6.5, i2 = 3), 
         fn = SSR, 
         method = "Nelder-Mead")
  
if (makeplot==TRUE)
{
  plot(temp2,tempq2)
  lines(seq(ceiling(min(temp2)),floor(max(temp2)),by=0.01), 
        fun(c(x1 = bestpar$x1, i1 = bestpar$i1, i2 = bestpar$i2), seq(ceiling(min(temp2)),floor(max(temp2)),by=0.01)),col='red')
}


result = c(breakpoint = bestpar$x1,minTempQ = bestpar$i1, highTempQ = bestpar$i2)
}

