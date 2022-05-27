# Define function that computes annual mean values for water quality analytes
# at each monitoring station appearing in the input dataframe DF. By default,
# the function computes the annual geometric mean, since most water quality
# variables are log-normally distributed. The annual arithmetic mean
# is computed for analytes passed via the ARITHMETIC argument (for analytes
# like DO and pH that are closer to a normal distribution). Note that the
# arithmetic mean of pH is equivalent to the geometric mean of [H+].
#
# Arguments:
#    DF           Dataframe with columns Station, Analyte, Date, Value, Unit
#    DATES        Date range for computing annual means, range(DF$Date) by default
#    MIN.OBS      Minimum number of observations from which to estimate mean. If
#                 fewer than MIN.OBS available, returns NA as mean.
#    ARITHMETIC   Character vector specifying analyte (DF$Analyte) for which to
#                 compute arithmetic, rather than geometric, means

annual.means <- function( DF, DATES=range(DF$Date), MIN.OBS=6,
                          ARITHMETIC=c('DO Sat','DO Conc','pH','Temp') ){
  
  # Load libraries
  if(!require(lubridate)) { install.packages('lubridate') }; library(lubridate)
  if(!require(tidyr)) { install.packages('tidyr') }; library(tidyr)
  
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
  
  # Get station and analyte names
  stations <- unique( DF$Station ) %>% sort()
  analytes <- unique( DF$Analyte ) %>% sort()
  years <- unique( year( DF$Date ) ) %>% sort()
  
  # Initialize output dataframe
  out <- data.frame( matrix(ncol=(length(years)+4),nrow=0) )
  colnames( out ) <- c( "Station", "Analyte", "Unit", "Mean", as.character(years) )
  out.row <- 1  # initialize counter for output row index 
  
  # Loop over stations, analytes, and years
  for( i in 1:length(stations) ){
    this.station <- stations[i]
    this.dat.i <- this.dat[ which(this.dat$Station==this.station), ]
    for( j in 1:length(analytes) ){
      this.analyte <- analytes[j]
      this.dat.ij <- this.dat.i[ which(this.dat.i$Analyte==this.analyte), ] 
      if( nrow(this.dat.ij)==0 ){ next } 
      this.unit <- unique( this.dat.ij$Unit )
      for( k in 1:length(years) ){
        # Compute mean for ijk data (geometric mean by default)
        this.year <- years[k]
        this.dat.ijk <- this.dat.ij[ which(year(this.dat.ij$Date)==this.year), ]
        # Compute mean if data requirement is met
        if( nrow(this.dat.ijk)<MIN.OBS | nrow(this.dat.ijk)==0 ){
          this.mean <- NA
          this.calc <- NA
        } else {
          # Compute geometric mean unless analyte in ARITHMETIC
          if( this.analyte %in% ARITHMETIC ){
            this.mean <- mean( this.dat.ijk$Value )
            this.calc <- "Arithmetic"
          } else {
            this.mean <- geomean( this.dat.ijk$Value )
            this.calc <- "Geometric"
          } 
        }
        # Write result to output dataframe
        out[ out.row, 1 ] <- this.station
        out[ out.row, 2 ] <- this.analyte
        out[ out.row, 3 ] <- this.unit
        out[ out.row, 4 ] <- this.calc
        out[ out.row, 4+k ] <- this.mean
      }  # // end k loop (years)
      # Increment output row counter
      out.row <- out.row+1
    }  # // end j loop (analytes)
  }  # // end i loop (stations)
  
  return( out )
  
}  # // end annual.means()
