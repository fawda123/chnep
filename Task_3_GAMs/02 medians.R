rm(list=ls(all=TRUE)) 

# Set directories
dir.dat <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 3\\R files\\Data')
dir.out <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 3\\R files\\Output')

# Load packages
if(!require(tidyr)) { install.packages('tidyr') }; library(tidyr)
if(!require(lubridate)) { install.packages('lubridate') }; library(lubridate)

# Load clean dataset  
setwd( dir.dat )
load('clean-data_2011-2020.RData')  

# Get station and analyte names
stations <- unique( clean.data$Unique.StationID )
analytes <- unique( clean.data$Analyte )

# Specify year bounds
bounds.5y  <- c(2016,2020)
bounds.10y <- c(2011,2020)

# Initialize medians dataframe
medians.df <- data.frame( Station=character(),
                          Analyte=character(),
                          median.5y=numeric(),
                          median.10y=numeric() )
medians.row <- 1

# Loop over stations and analytes to compute medians
for( i in 1:length(stations) ){
  this.station <- stations[i]
  dat.i <- clean.data[ which( clean.data$Unique.StationID==this.station ), ]
  for( j in 1:length(analytes) ){
    this.analyte <- analytes[j]
    dat.ij <- dat.i[ which( dat.i$Analyte==this.analyte ), ]
    # Compute 5y and 10y medians
    if( nrow(dat.ij)>0 ){
      idx.5y  <- which( year(dat.ij$Date) %in% (min(bounds.5y):max(bounds.5y)) )
      idx.10y <- which( year(dat.ij$Date) %in% (min(bounds.10y):max(bounds.10y)) )
      median.5y  <- median( dat.ij$Result_Value[ idx.5y ] )
      median.10y <- median( dat.ij$Result_Value[ idx.10y ] )
    } else {
      median.5y  <- NA
      median.10y <- NA
    }
    # Add row to medians.df
    medians.df[ medians.row, "Station" ] <- this.station 
    medians.df[ medians.row, "Analyte" ] <- as.character(this.analyte)
    medians.df[ medians.row, "median.5y" ] <- median.5y
    medians.df[ medians.row, "median.10y" ] <- median.10y
    medians.row <- medians.row+1
  }  # end j loop
  
}  # end i loop

# Write medians.df to file
setwd(dir.out)
save( medians.df, file="medians.RData" )
