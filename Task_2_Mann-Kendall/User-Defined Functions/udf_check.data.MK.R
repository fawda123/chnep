# Define function to check data requirements for the Mann-Kendall (MK) trend
# test and return the dataframe to be used for the analysis.
# Arguments:
#   DF           Dataframe with time-aggregated data (e.g. monthly data)
#                for a single analyte/station pair, with columns Date, Value,
#                Non_detect.
#   POR.maxgap   Maximum allowable monitoring gap, in time units of input
#                data (e.g. months). Data prior to a gap exceeding POR.maxgap
#                in length are excluded from the MK.
#   n.min        Min required number of observations. MK will halt if DF
#                contains fewer than n.min data points. 
#   POR.min      Minimum required time period, in time units of input data
#                (e.g. months). MK will halt if POR < POR.min.
#   POR.last     Most recent required year (e.g. 2020). MK will halt if POR
#                ends before POR.last.
#   max.MDL      Maximum allowable proportion of non-detects.


check.data.MK <- function( DF, POR.maxgap = 18,  # time units in input data (e.g. months)
                           n.min = 30,  # number of observations
                           POR.min = 60,  # time units in input data (e.g. months)
                           POR.last = 2020,  # calendar year
                           max.MDL = 1  # proportion [0,1]
){
  
  # Load libraries
  if(!require(lubridate)) { install.packages('lubridate') }; library(lubridate)
  if(!require(tidyr)) { install.packages('tidyr') }; library(tidyr)
  if(!require(dplyr)) { install.packages('dplyr') }; library(dplyr)
  
  # Initialize dataframe to store results of data checks
  checks <- c( n.min = TRUE,
               POR.min = TRUE,
               POR.last = TRUE,
               max.MDL = TRUE )
  
  
  # Copy input dataframe and add column for cumulative time count
  dat1 <- DF
  dat1$c.time <- 1:nrow(dat1)
  
  # POR.maxgap: Check for large monitoring gaps and remove data prior to the most recent one 
  dat1.diff <- dat1$c.time[ which(!is.na(dat1$Value)) ] %>% diff()
  large.gaps <- dat1.diff[ which( dat1.diff > POR.maxgap ) ]
  if( length(large.gaps)>0 ){  # If any gaps >POR.maxgap, remove data from prior to the gap
    gap.idx <- max( which( dat1.diff == last(large.gaps) ) )
    dat2 <- dat1[ (sum(dat1.diff[1:gap.idx])+1):nrow(dat1) ,]
  } else {
    dat2 <- dat1
  }
  
  # n.min: Check that sample size
  n.dat <- length( which(!is.na( dat2$Value )) )
  if( n.dat < n.min ){ checks["n.min"] <- FALSE }
  
  # POR.min: Check length of POR
  n.POR <- diff(range( dat2$c.time ))+1
  if( n.POR < POR.min ){ checks["POR.min"] <- FALSE }
  
  # POR.last: Check last year of POR
  dat.last <- year( max( dat2$Date, na.rm=TRUE ) )
  if( dat.last < POR.last ){ checks["POR.last"] <- FALSE }
  
  # Max MDLs
  n.MDL <- length( which( dat2$Non_detect==TRUE ) ) / nrow( dat2 )
  if( n.MDL > max.MDL ){ checks["max.MDL"] <- FALSE }
  
  
  return( list( checks = checks,
                proceed = !any(checks==FALSE),
                dat = dat2 ) )
  
}  # // end check.data.MK()



# set.seed(394) ;  nd = sample( c(TRUE,FALSE), size=120, replace=TRUE, prob=c(0.50,0.50) )
# DF <- data.frame( Date = seq.Date( as.Date('2011-01-01'), as.Date('2020-12-31'), by='month' ),
#                   Value = 1:120,
#                   Non_detect = nd )
# par(mfrow=c(2,1))
# plot(DF)
# DF$Value[c(5:25,40:48,88:90)] <- NA
# plot(DF)
