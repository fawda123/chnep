# Define function to perform trend analysis with Mann-Kendall test for monotonic
# trend and Theil-Sen slope estimation.
# The function calls on other user-defined functions to aggregate input data
# into a monthly time series, check data requirements, and check for autocorrelation,
# and then performs the MK test (or Seasonal MK) and Theil-Sen slope estimation and
# reports results.
# The input dataframe should contain data on a single analyte/station (or
# analyte/stratum) pair and must contain columns Date, Analyte, Station, Value,
# Unit, and Non_detect.

MK.trend <- function( DAT,
                      dir.udf = c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\User-Defined Functions'),
                      POR.min = 60
                      ){
  
  # Load libraries
  if(!require(Kendall)) { install.packages('Kendall') }; library(Kendall)
  if(!require(mblm)) { install.packages('mblm') }; library(mblm) # Sen
  
  # Load user-defined functions
  setwd(dir.udf)
  source('udf_aggregate.monthly.R')
  source('udf_check.data.MK.R')
  source('udf_check.acf.monthly.R')
  
  # Aggregate data
  ts1 <- aggregate.monthly( DAT )
  this.station <- ts1$station
  this.analyte <- ts1$analyte
  this.unit <- ts1$unit
  
  # Check data requirements
  ts2 <- check.data.MK( ts1$dat, POR.min = POR.min )
  
  # If data requirements are met...
  if( ts2$proceed ){
    # Check for autocorrelation
    autocorr <- check.acf.monthly( ts2$dat )
    # Run Mann-Kendall test (MK, or SMK if autocorrelated)
    if( autocorr ){
      this.MK <- SeasonalMannKendall( ts(ts2$dat$Value) )
    } else {
      this.MK <- MannKendall( ts(ts2$dat$Value) )
    }  # // end if(autocorrelated)
    
    # MK tau and p-value
    MK.tau <- this.MK$tau
    MK.pval <- this.MK$sl
    
    # MK trend (1 = trend up; -1 = trend down; 0 = no trend)
    if( MK.pval<0.05 & MK.tau>0 ){
      MK.trend <- 1
    } else if( MK.pval<0.05 & MK.tau<0 ){
      MK.trend <- -1
    } else {
      MK.trend <- 0
    }  # end if(trend)
    
    # Run Theil-Sen slope estimation
    this.Sen <- mblm( Value~c.time, dataframe=na.omit(ts2$dat), repeated=FALSE )
    
    Sen.y_int <- this.Sen$coefficients[1]
    Sen.slope <- this.Sen$coefficients[2]
    Sen.pval <- summary.mblm(this.Sen)$coefficients['c.time','Pr(>|V|)']
    
    # Compute median and classify trend magnitude (larger or smaller)
    this.median <- median( ts2$dat$Value, na.rm=TRUE )
    slope.ratio <- Sen.slope * 12 / this.median  # ratio of slope (annual) to median value
    if( MK.trend != 0 ){
      if( abs(slope.ratio) > 0.10 ){  # if significant MK trend and 'large' slope trend 
        trend.large <- TRUE
      } else {
        trend.large <- FALSE
      }  # // end if(slope.ratio)
    } else {   # if MK trend is not significant, assign NA
      trend.large <- NA
    }  # // end if(MK.pval)
    
    return( list( Station=as.character(this.station),
                  Analyte=as.character(this.analyte),
                  Unit=as.character(this.unit),
                  N=nrow(na.omit(ts2$dat)),
                  Date.min=min(ts2$dat$Date), Date.max=max(ts2$dat$Date),
                  SMK=autocorr, tau=MK.tau, MK.pval=MK.pval, MK.trend=MK.trend,
                  Sen.y_int = Sen.y_int, Sen.slope = Sen.slope, Sen.pval = Sen.pval,
                  median = this.median, slope.ratio = slope.ratio, trend.large = trend.large) ) 
  } else {
    return(NULL)
  }  # // end if(proceed)
  
  
}  # // end MK.trend()
