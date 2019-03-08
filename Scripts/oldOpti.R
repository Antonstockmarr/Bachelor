# Computing estimates 
fun <- function(par,x)
{
  # The first intercept
  y1 <- x^0 * par["i1"]
  # After breakpoint should be constant
  y1[x >= par["x1"]] <- par["i2"]
  # Should interpolate between intercept and breakpoint.
  r <- x < par["x1"]
  y1[r] <- par["i1"] - (par["i1"] - par["i2"])/(par["x1"] - x[1]) * (x[r] - x[1])
  y1
}

SSR <- function(par) {
  sum((tempq2 - fun(par,temp2))^2)
}


#bestpar <- optimx(par = c(x1 = 13.5, i1 = 6.5, i2 = 3), 
#         fn = SSR, 
#         method = "Nelder-Mead")

#  plot(temp2,tempq2,ylab='Consumption',xlab='Temperature',main = paste('House number', i))
#  lines(seq(ceiling(min(temp2)),floor(max(temp2)),by=0.01), 
#        fun(c(x1 = bestpar$x1, i1 = bestpar$i1, i2 = bestpar$i2), seq(ceiling(min(temp2)),floor(max(temp2)),by=0.01)),col='red')

