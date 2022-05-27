# Define function that aggregates observations into a weekly time series,
# by computing the arithmetic or geometric mean, as appropriate. 
# Arguments:
#   DF           Input dataframe containing values for a single analyte. Must
#                contain columns Date, Analyte, Value.
#   DATES        Date range for aggregating time series, range(DF$Date) by default
#   ARITHMETIC   Character vector specifying values for values should be aggregated
#                using the arithmetic mean (e.g. pH, Temp). By default, the
#                function aggregates using the geometric mean.

aggregate.weekly <- function( DF, DATES=range(DF$Date),
                              ARITHMETIC=c('DO Sat','DO Conc','pH','Temp') ){
  
  # Load libraries
  if(!require(lubridate)) { install.packages('lubridate') }; library(lubridate)
  if(!require(tidyr)) { install.packages('tidyr') }; library(tidyr)
  if(!require(dplyr)) { install.packages('dplyr') }; library(dplyr)
  
  # Define function to compute geometric mean of numeric vector X.
  # By default, the function disregards any NA values.
  geomean <- function(X,na.rm=TRUE){
    input <- X
    if( na.rm ){
      input <- na.omit( input )
    }
    output <- exp(mean(log(X)))
    return( output )
  }  # // end geomean()
  
  # Subset input data by specified dates
  this.dat <- DF[ which( DF$Date>=min(DATES) & DF$Date<=max(DATES) ), ]
  
  # Generate new column 'Week'
  this.dat$Week <- floor_date( this.dat$Date, unit = 'week' )
  
  # Aggregate data using arithmetic or geometric mean
  this.analyte <- unique(this.dat$Analyte)
  if( this.analyte %in% ARITHMETIC ){
    df <- this.dat %>% group_by(Week) %>%
      summarise_each( funs(mean), 'Value' ) %>% as.data.frame() 
  } else {
    df <- this.dat %>% group_by(Week) %>%
      summarise_each( funs(geomean), 'Value' ) %>% as.data.frame()
  }  # // end if()
  
  # Generate time series
  dates <- data.frame( Week = seq.Date( this.dat$Week[1], DATES[2], by='week') )
  out <- left_join( dates, df, by = 'Week' )
  
  return( out )
  
}  # // end aggregate.weekly()

# test.dat <- data.frame( Date = c( seq.Date( as.Date('2020-01-01'), as.Date('2020-03-31'), 'day' ), as.Date('2020-05-05') ),
#                         Value = c( c(rep(2,30),100),
#                                    c(rep(2,28),100),
#                                    c(rep(2,30),100),
#                                    100 ),
#                         Analyte = rep('TN',92)
#                         )
# 
# result <- aggregate.weekly(test.dat)
# result
