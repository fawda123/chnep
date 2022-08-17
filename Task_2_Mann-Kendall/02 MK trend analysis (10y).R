# Master script for CHNEP MK/Sens trend analysis


rm(list=ls(all=TRUE)) 

# Set directories
dir.udf <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\User-Defined Functions')
dir.dat <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 1')
dir.out <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\Output')

# Load libraries
if(!require(tidyr)) { install.packages('tidyr') }; library(tidyr)
if(!require(dplyr)) { install.packages('dplyr') }; library(dplyr)
if(!require(lubridate)) { install.packages('lubridate') }; library(lubridate)

# Load user-defined functions
setwd(dir.udf)
source('udf_MK.trend.R')

# Load clean dataset  
setwd( dir.dat )
load('clean-data_2011-2020.RData')

# Format input data
input.dat <- select( clean.data, c(Date,Unique.StationID,Analyte,Result_Value,Result_Unit,Non_detect) )
colnames( input.dat ) <- c( 'Date', 'Station', 'Analyte', 'Value', 'Unit', 'Non_detect')

# Subset data by date
min.date <- as.Date('2011-01-01')
max.date <- as.Date('2020-12-31')
input.dat <- input.dat[ which( input.dat$Date>=min.date & input.dat$Date<=max.date ), ]

# Get station and analyte names
stations <- unique( input.dat$Station )
analytes <- unique( input.dat$Analyte )

# Initialize dataframe to store trend analysis results.
# Initial values in first row will be overwritten.
results.df <- data.frame( Station=character(),
                          Analyte=character(),
                          Unit=character(),
                          N=numeric(),
                          Date.min=Date(),
                          Date.max=Date(),
                          SMK=logical(),
                          MK.tau=numeric(),
                          MK.pval=numeric(),
                          MK.trend=integer(),
                          Sen.y_int=numeric(),
                          Sen.slope=numeric(),
                          Sen.pval=numeric(),
                          median=numeric(),
                          slope.ratio=numeric(),
                          trend.large=logical() )
result.row <- 1 # initialize counter for result.df row index 

# Run trend analysis on each station/analyte pair (or stratum/analyte pair)
for( i in 1:length(stations) ){
  # Subset data by station
  this.station <- stations[i]
  dat.i <- input.dat[ which( input.dat$Station==this.station ), ]
  for( j in 1:length(analytes) ){
    # Subset data by analyte
    this.analyte <- analytes[j]
    dat.ij <- dat.i[ which( dat.i$Analyte==this.analyte ), ]
    dat.ij <- dat.ij[ order(dat.ij$Date), ]
    # Run trend analysis (MK or SMK)
    if( nrow(dat.ij)>0 ){ 
      this.result <- MK.trend( dat.ij, dir.udf=dir.udf, POR.min=120 )  # set min POR to 120 months (10 years)
      if( !is.null(this.result) ){
        # Write trend results to results.df
        results.df[ result.row, 'Station' ] <- this.result$Station
        results.df[ result.row, 'Analyte' ] <- this.result$Analyte
        results.df[ result.row, 'Unit' ] <- this.result$Unit
        results.df[ result.row, 'N' ] <- this.result$N
        results.df[ result.row, 'Date.min' ] <- as.Date(this.result$Date.min,origin='1970-01-01')
        results.df[ result.row, 'Date.max' ] <- this.result$Date.max
        results.df[ result.row, 'SMK' ] <- this.result$SMK 
        results.df[ result.row, 'MK.tau' ] <- this.result$tau 
        results.df[ result.row, 'MK.pval' ] <- this.result$MK.pval
        results.df[ result.row, 'MK.trend' ] <- this.result$MK.trend 
        results.df[ result.row, 'Sen.y_int' ] <- this.result$Sen.y_int
        results.df[ result.row, 'Sen.slope' ] <- this.result$Sen.slope
        results.df[ result.row, 'Sen.pval' ] <- this.result$Sen.pval
        results.df[ result.row, 'median' ] <- this.result$median
        results.df[ result.row, 'slope.ratio' ] <- this.result$slope.ratio
        results.df[ result.row, 'trend.large' ] <- this.result$trend.large
      } else {
        # Record 'insufficient data' to results.df
        results.df[ result.row, 'Station' ] <- as.character( this.station )
        results.df[ result.row, 'Analyte' ] <- as.character( this.analyte )
        results.df[ result.row, 'Unit' ] <- NA
        results.df[ result.row, 'N' ] <- 0  # 0 means insufficient data for trend analysis
        results.df[ result.row, 'Date.min' ] <- NA
        results.df[ result.row, 'Date.max' ] <- NA
        results.df[ result.row, 'SMK' ] <- NA
        results.df[ result.row, 'MK.tau' ] <- NA
        results.df[ result.row, 'MK.pval' ] <- NA
        results.df[ result.row, 'MK.trend' ] <- NA
        results.df[ result.row, 'Sen.y_int' ] <- NA
        results.df[ result.row, 'Sen.slope' ] <- NA
        results.df[ result.row, 'Sen.pval' ] <- NA
        results.df[ result.row, 'median' ] <- median( dat.ij$Value, na.rm=TRUE )
        results.df[ result.row, 'slope.ratio' ] <- NA
        results.df[ result.row, 'trend.large' ] <- NA
      }  # // end if(MK.trend)
    } else {
      # Record 'no data' to results.df
      results.df[ result.row, 'Station' ] <- as.character( this.station )
      results.df[ result.row, 'Analyte' ] <- as.character( this.analyte )
      results.df[ result.row, 'Unit' ] <- NA
      results.df[ result.row, 'N' ] <- NA  # NA means no data on this station/analyte pair
      results.df[ result.row, 'Date.min' ] <- NA
      results.df[ result.row, 'Date.max' ] <- NA
      results.df[ result.row, 'SMK' ] <- NA
      results.df[ result.row, 'MK.tau' ] <- NA
      results.df[ result.row, 'MK.pval' ] <- NA
      results.df[ result.row, 'MK.trend' ] <- NA
      results.df[ result.row, 'Sen.y_int' ] <- NA
      results.df[ result.row, 'Sen.slope' ] <- NA
      results.df[ result.row, 'Sen.pval' ] <- NA
      results.df[ result.row, 'median' ] <- NA
      results.df[ result.row, 'slope.ratio' ] <- NA
      results.df[ result.row, 'trend.large' ] <- NA
    }  # // end if(dat.ij)
    # Increment result.df row counter
    result.row <- result.row+1
  }  # // end j loop (analytes)
}  # // end i loop (stations)

# Write trend analysis results to file
setwd(dir.out)
write.csv( results.df, "MK.trend.10y.results.csv", row.names=FALSE )
save( results.df, file="MK.trend.10y.results.RData" )

