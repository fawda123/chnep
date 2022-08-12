rm(list=ls(all=TRUE)) 


# Set directories
dir.dat <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 3\\R files\\Data')
dir.out <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 3\\R files\\Output')

# Load data 
setwd( dir.out )
load("GAM.results.RData")  # trend results in results.df dataframe
load("medians.RData")  # 5y and 10y medians in medians.df dataframe

# Initialize trends.df to store trend directions and categories
trends.df <- data.frame( Station=character(),
                         Analyte=character(),
                         date.min=Date(),
                         date.max=Date(),
                         median.5y=numeric(),
                         trend.5y=integer(),
                         trend.5y.large=logical(),
                         slope.ratio.5y=numeric(),
                         median.10y=numeric(),
                         trend.10y=integer(),
                         trend.10y.large=logical(),
                         slope.ratio.10y=numeric() )


trend.cat <- function( RESULTS, MEDIANS=medians.df ){
  
  this.station <- RESULTS[["Station"]]
  this.analyte <- RESULTS[["Analyte"]]
  
  # 5-year trend
  median.5y <- MEDIANS$median.5y[ which( MEDIANS$Station==this.station &
                                           MEDIANS$Analyte==this.analyte ) ]
  if( all(!is.na( c(RESULTS[["mix5.P.slope"]],RESULTS[["mix5.slope"]]) )) ){
    # Trend direction (up or down)
    if( RESULTS[["mix5.P.slope"]]<0.05 & RESULTS[["mix5.slope"]]>0 ){
      trend.5y <- 1
    } else if( RESULTS[["mix5.P.slope"]]<0.05 & RESULTS[["mix5.slope"]]<0 ){
      trend.5y <- -1
    } else {
      trend.5y <- 0
    } 
    # Trend category (large or small)
    if( trend.5y != 0 ){
      slope.ratio.5y <- as.numeric(RESULTS[["mix5.slope"]]) / median.5y
      if( abs(slope.ratio.5y) > 0.10 ){  
        trend.5y.large <- TRUE
      } else {
        trend.5y.large <- FALSE
      }  
    } else {  
      slope.ratio.5y <- NA
      trend.5y.large <- NA
    } 
  } else {
    trend.5y <- NA
    trend.5y.large <- NA
    slope.ratio.5y <- NA
  }
  
  # 10-year trend
  median.10y <- MEDIANS$median.10y[ which( MEDIANS$Station==this.station &
                                             MEDIANS$Analyte==this.analyte ) ]
  if( all(!is.na( c(RESULTS[["mix10.P.slope"]],RESULTS[["mix10.slope"]]) )) ){
    # Trend direction (up or down)
    if( RESULTS[["mix10.P.slope"]]<0.05 & RESULTS[["mix10.slope"]]>0 ){
      trend.10y <- 1
    } else if( RESULTS[["mix10.P.slope"]]<0.05 & RESULTS[["mix10.slope"]]<0 ){
      trend.10y <- -1
    } else {
      trend.10y <- 0
    } 
    # Trend category (large or small)
    if( trend.10y != 0 ){
      slope.ratio.10y <- as.numeric(RESULTS[["mix10.slope"]]) / median.10y
      if( abs(slope.ratio.10y) > 0.10 ){  
        trend.10y.large <- TRUE
      } else {
        trend.10y.large <- FALSE
      }  
    } else {  
      slope.ratio.10y <- NA
      trend.10y.large <- NA
    } 
  } else {
    trend.10y <- NA
    trend.10y.large <- NA
    slope.ratio.10y <- NA
  }
  
  # Output
  out <- data.frame( Station=this.station,
                     Analyte=this.analyte,
                     date.min=RESULTS[["Date.min"]],
                     date.max=RESULTS[["Date.max"]],
                     median.5y=median.5y,
                     trend.5y=trend.5y,
                     trend.5y.large=trend.5y.large,
                     slope.ratio.5y=abs(slope.ratio.5y),
                     median.10y=median.10y,
                     trend.10y=trend.10y,
                     trend.10y.large=trend.10y.large,
                     slope.ratio.10y=abs(slope.ratio.10y) )
  return( out )
  
}  # // end trend.cat()

# Loop over each row of results.df to get trend categories
# Caution: apply(results.df,1,trend.cat) returns incorrect results
for( i in 1:nrow(results.df) ){
  trends.df[i,] <- trend.cat( results.df[i,] )
}

# Write trends.df to file
setwd(dir.out)
save( trends.df, file="trends.RData" )