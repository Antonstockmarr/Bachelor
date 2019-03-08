# Function that determines if the data set is lacking too much, if it is suitable to be included in the model or not
DataChecking <- function(df,par)
{
  n <- length(df[,1])
  S <- TRUE
  
  # It should have a certain number of observations
  if (par['min_obs'] > n)
    S = FALSE
  
  
  # The amount of missing data should not exceed a certain fraction of the data
  misses <- sum(is.na(df$TemperatureIn))
  if (par['miss_fraction'] < misses/n)
    S = FALSE
  
  
  vr <- rle(df$Flow)
  if (vr$lengths[1] > 1000)
      df <- df[-(1:vr$lengths[1]),]
  if (vr$lengths[length(vr$lengths)]>1000)
      df <- df[-(n-vr$lengths[1]:n),]
  
  
  
 S 
}
