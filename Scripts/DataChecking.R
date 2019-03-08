# Function that determines if the data set is lacking too much, if it is suitable to be included in the model or not
DataChecking <- function(df,par)
{
  n <- length(df$EndDateTime)
  S <- TRUE
  
  # It should have a certain number of observations
  if (par['min_obs'] > n)
    S = FALSE
  
  
  # The amount of missing data should not exceed a certain fraction of the data
  misses <- sum(is.na(df$TemperatureIn))
  if (par['miss_fraction'] < misses/n)
    S = FALSE
  
  
  
  
  
  
  
 S 
}
