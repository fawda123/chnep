# Define function that aggregates observations into a monthly time series,
# by computing the arithmetic or geometric mean, as appropriate. 
# Arguments:
#   DF           Input dataframe containing values for a single analyte. Must
#                contain columns Date, Analyte, Station, Value.
#   DATES        Date range for aggregating time series, range(DF$Date) by default
#   ARITHMETIC   Character vector specifying values for values should be aggregated
#                using the arithmetic mean (e.g. pH, Temp). By default, the
#                function aggregates using the geometric mean.

aggregate.monthly <- function( DF, DATES=range(DF$Date),
                               ARITHMETIC=c('DO Sat','DO Conc','pH','Temp') ){
  
  # Check that DF includes data on a single Analyte, Station, and Unit
  this.station <- unique( DF$Station )
  this.analyte <- unique( DF$Analyte )
  this.unit <- unique( DF$Unit )
  if( length(this.station) != 1 ){ stop("In input data, number of stations != 1.") }
  if( length(this.analyte) != 1 ){ stop("In input data, number of analytes != 1.") }
  if( length(this.unit) != 1 ){ stop("In input data, number of units != 1.") }
  
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
  
  # Generate new column 'Month'
  this.dat$Month <- floor_date( this.dat$Date, unit = 'month' )
  
  # Aggregate values using arithmetic or geometric mean
  if( this.analyte %in% ARITHMETIC ){
    values <- this.dat %>% group_by(Month) %>%
      summarise_each( funs(mean), 'Value' ) %>% as.data.frame() 
  } else {
    values <- this.dat %>% group_by(Month) %>%
      summarise_each( funs(geomean), 'Value' ) %>% as.data.frame()
  }  # // end if()
  
  # Aggregate non_detect (logical)
  nondetects <- this.dat %>% group_by(Month) %>% 
    summarise_each( funs(all), 'Non_detect' ) %>% as.data.frame()
  
  # Join values and nondetect columns
  df <- inner_join( values, nondetects, by = 'Month' )
  
  # Generate time series
  dates <- data.frame( Month = seq.Date( min(this.dat$Month), DATES[2], by='month') )
  out <- left_join( dates, df, by = 'Month' )
  colnames( out )[ which(colnames(out)=='Month') ] <- 'Date'
  
  return( list( dat = out, station = this.station, analyte = this.analyte, unit = this.unit ) )
  
}  # // end aggregate.monthly()

# test.dat <- data.frame( Date = c( seq.Date( as.Date('2020-01-01'), as.Date('2020-03-31'), 'day' ), as.Date('2020-05-05') ),
#                         Value = c( c(rep(2,30),100),
#                                    c(rep(2,28),100),
#                                    c(rep(2,30),100),
#                                    100 ),
#                         Analyte = rep('TN',92),
#                         Station = rep('AA',92)
#                         )
# 
# result <- aggregate.monthly(test.dat)
# result
