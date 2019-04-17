Sun <- function(StartTime,EndTime)
{
  library(solaR)
  (ts <- seq(as.POSIXct(StartTime), as.POSIXct(EndTime), by="hours"))
  sol.rad <- calcSol(lat = 55.5, BTi = ts)
  sol.rad <- as.data.frameI(sol.rad)
  sol.rad$Bo0
}
