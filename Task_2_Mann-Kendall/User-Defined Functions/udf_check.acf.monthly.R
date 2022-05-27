# Define function to check autocorrelation

check.acf.monthly <- function( DF ){
  
  # Check for case in which all reported values are equal (no variability in data)
  if( length(unique( DF$Value ))==1 ){
    autocorrelated <- FALSE
    return( autocorrelated )
  }  # // end if()
  
  # Compute autocorrelation function
  acf.result <- acf( DF$Value, plot=F, na.action=na.pass, lag.max=12 )
  acf.6mo  <- acf.result$acf[7]
  acf.12mo <- acf.result$acf[13]
  # Compute critical ACF value for significance
  acf.crit <- 1.96 / sqrt(acf.result$n.used)
  # Compare 6- and 12-month ACF values to critical value
  if( acf.6mo<=(-acf.crit) | acf.12mo>=acf.crit ){
    autocorrelated <- TRUE
  } else {
    autocorrelated <- FALSE
  }  # // end if()
  return( autocorrelated )
  
}  # // check.acf()