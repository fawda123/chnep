rm(list=ls(all=TRUE)) 

# Set directories
dir.udf <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 3\\R files\\User-Defined Functions')
dir.dat <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 3\\R files\\Data')
dir.out <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 3\\R files\\Output')

# Load packages
if(!require(tidyr)) { install.packages('tidyr') }; library(tidyr)
if(!require(mgcv)) { install.packages('mgcv') }; library(mgcv)
if(!require(dplyr)) { install.packages('dplyr') }; library(dplyr)
if(!require(lubridate)) { install.packages('lubridate') }; library(lubridate)
if(!require(mixmeta)) { install.packages('mixmeta') }; library(mixmeta)
if(!require(wqtrends)) { install.packages('wqtrends',
                                          repos = 'https://tbep-tech.r-universe.dev' )
                        }; library(wqtrends)

# Load user-defined functions
setwd(dir.udf)
source('anlz_gam_SDI.R')
source('show_gam_ts.R')
source('show_dat_ts.R')
source('show_mixmod.R')
source('anlz_pval.R')
source('gam.residuals.check.R')

# Load clean dataset  
setwd( dir.dat )
load('clean-data_2011-2020.RData')  

# Format input data
input.dat <- select( clean.data, c(Date,Unique.StationID,Analyte,Result_Value,Result_Unit) )
colnames( input.dat ) <- c( 'date', 'station', 'param', 'value', 'unit' )

# Add time columns
input.dat$doy <- yday( input.dat$dat )
input.dat$cont_year <- decimal_date( input.dat$date )
input.dat$yr <- lubridate::year( input.dat$date )
input.dat$mo <- lubridate::month( input.dat$date, label=TRUE, abbr=TRUE )

# Subset data by date
min.date <- as.Date('2011-01-01')
max.date <- as.Date('2020-12-31')
input.dat <- input.dat[ which( input.dat$date>=min.date & input.dat$date<=max.date ), ]

# Get station and analyte names
stations <- unique( input.dat$station )
analytes <- unique( input.dat$param )

# Specify whether to export plots to png
write.png <- TRUE

# Initialize dataframe to store results
results.df <- data.frame( Station=character(),
                          Analyte=character(),
                          Unit=character(),
                          N=numeric(),
                          Date.min=Date(),
                          Date.max=Date(),
                          Median=numeric(),
                          Trans=character(),
                          gam.success=logical(),
                          gam.k.cont_year=numeric(),
                          gam.k.doy=numeric(),
                          gam.ksuff.cont_year=logical(),
                          gam.ksuff.doy=logical(),
                          gam.ksuff.ti=logical(),
                          gam.P.cont_year=numeric(),
                          gam.P.doy=numeric(),
                          gam.P.ti=numeric(),
                          gam.ks.P=numeric(),
                          gam.sw.P=numeric(),
                          gam.Rsq=numeric(),
                          mix10.fun=character(),
                          mix10.slope=numeric(),
                          mix10.logslope=numeric(),
                          mix10.P.slope=numeric(),
                          mix5.fun=character(),
                          mix5.slope=numeric(),
                          mix5.logslope=numeric(),
                          mix5.P.slope=numeric() )
result.row <- 1 # initialize counter for result.df row index 


# Loop over station/analyte pairs to run trend analysis
setwd(dir.out)
for( i in 1:length(stations) ){
this.station <- stations[i]
dat.i <- input.dat[ which( input.dat$station==this.station ), ]
# Generate station label string for plots
this.strsplit <- strsplit( this.station, ": " ) %>% unlist()
if( length(this.strsplit)==2 ){
  this.station.lab <- paste0( "Station ",
                              this.strsplit[2], " (",
                              this.strsplit[1], ")")
} else if( length(this.strsplit)==1 ) {
  this.station.lab <- paste0( this.strsplit," Stratum" )
} else {
  stop( paste0("Station string split not recognized (i=",i,")\n") )
}

  for( j in 1:length(analytes) ){
  # Subset data by analyte
    this.analyte <- analytes[j]
    dat.ij <- dat.i[ which( dat.i$param==this.analyte ), ]
    dat.ij <- dat.ij[ order(dat.ij$date), ]
  # Create new record in results.df
    results.df[result.row,"Station"] <- this.station
    results.df[result.row,"Analyte"] <- as.character(this.analyte)
    
  # Run GAM model
    # Fit GAM
    if( nrow(dat.ij)>0 ){
      # Attempt to fit GAM
      mod <- anlz_gam_SDI( dat.ij )
      # Write data results to results.df
      results.df[result.row,"Unit"] <- unique(dat.ij$unit)
      results.df[result.row,"N"] <- nrow(dat.ij)
      results.df[result.row,"Date.min"] <- min(dat.ij$date)
      results.df[result.row,"Date.max"] <- max(dat.ij$date)
      results.df[result.row,"Median"] <- median(dat.ij$value)
      results.df[result.row,"Trans"] <- mod$trans
      results.df[result.row,"gam.success"] <- mod$success
    } else {
      mod <- list()
      mod$success <- FALSE
      # Write data results to results.df
      results.df[result.row,"Unit"] <- NA
      results.df[result.row,"N"] <- 0
      results.df[result.row,"Date.min"] <- NA
      results.df[result.row,"Date.max"] <- NA
      results.df[result.row,"Median"] <- NA
      results.df[result.row,"Trans"] <- NA
      results.df[result.row,"gam.success"] <- mod$success
    }  # end GAM fit block
    
    # Set graphics device
    if( write.png ){
      png( filename = paste0(this.station.lab,", ",as.character(this.analyte),".png"),
           width = 6.5, height = 7, units = 'in', res = 400 )
    }
    par(mfrow=c(3,1),mar=c(3,5,2.5,1))
    # Plot observed data and GAM fit
    if( mod$success ){  # If GAM fit successful...
      # Plot data and GAM fit
      show_gam_ts( mod, station.lab = this.station.lab )
      # Write GAM results to results.df
      results.df[result.row,"gam.k.cont_year"] <- mod$knots["cont_year"]
      results.df[result.row,"gam.k.doy"] <- mod$knots["doy"]
      results.df[result.row,"gam.ksuff.cont_year"] <- mod$k.suff$cont_year
      results.df[result.row,"gam.ksuff.doy"] <- mod$k.suff$doy
      results.df[result.row,"gam.ksuff.ti"] <- mod$k.suff$ti
      results.df[result.row,"gam.P.cont_year"] <- summary(mod)$s.table["s(cont_year)","p-value"]
      results.df[result.row,"gam.P.doy"] <- summary(mod)$s.table["s(doy)","p-value"]
      results.df[result.row,"gam.P.ti"] <- summary(mod)$s.table["ti(cont_year,doy)","p-value"]
      results.df[result.row,"gam.ks.P"] <- mod$ks$p.value
      results.df[result.row,"gam.sw.P"] <- mod$sw$p.value
      results.df[result.row,"gam.Rsq"] <- summary(mod)$r.sq
    } else {  # If GAM fit not successful...
      # Write NAs to results.df
      results.df[result.row,"gam.k.cont_year"] <- NA
      results.df[result.row,"gam.k.doy"] <- NA
      results.df[result.row,"gam.ksuff.cont_year"] <- NA
      results.df[result.row,"gam.ksuff.doy"] <- NA
      results.df[result.row,"gam.ksuff.ti"] <- NA
      results.df[result.row,"gam.P.cont_year"] <- NA
      results.df[result.row,"gam.P.doy"] <- NA
      results.df[result.row,"gam.P.ti"] <- NA
      results.df[result.row,"gam.ks.P"] <- NA
      results.df[result.row,"gam.sw.P"] <- NA
      results.df[result.row,"gam.Rsq"] <- NA
      # Generate time series plot or null plot
      if( nrow(dat.ij)>0 ){
        # Plot time series (no GAM fit)
        show_dat_ts( dat.ij, station.lab = this.station.lab )
      } else {
        # Generate null plot
        par(family='serif')
        xseq <- year(min.date):(year(max.date)+1)
        plot( x = xseq, y = rep(0,length(xseq)),
              ylim=c(0,1), yaxt = 'n', cex.axis = 1.4,
              xlab = '', ylab = '', col=rgb(0,0,0,0) )
        mtext( paste(this.analyte,"at",this.station.lab), side = 3, line = 0, adj = 0 )
        text( x = median(xseq), y = 0.5, cex = 1.4,
              labels = paste0("No data\n(",lubridate::month(min.date,abbr=TRUE,label=TRUE),
                              " ", year(min.date)," to ",
                              lubridate::month(max.date,abbr=TRUE,label=TRUE),
                              " ", year(max.date), ")") )
      }
    }  # end GAM/time series plot block
    
    # Run mixed effects model
    if( mod$success ){
        # Compute endpoints for 5- and 10-year trend analyses
        year.10 <- year(max.date) 
        year.6  <- year.10 - 4
        year.1  <- year.10 - 9
        # Estimate 10-year trend
        if( min(mod$dat$yr)<=year.1 &  # Require data from 1st and 10th years
            max(mod$dat$yr)>=year.10 ){  
          # Estimate 10y mixmodel and plot it
          trend.10y <- show_mixmod( mod,yrstr=year.1,yrend=year.10 )
          # Write 10y mixmodel results to results.df
          results.df[result.row,"mix10.fun"] <- trend.10y$metfun
          results.df[result.row,"mix10.slope"] <- trend.10y$slope
          results.df[result.row,"mix10.logslope"] <- trend.10y$logslope
          results.df[result.row,"mix10.P.slope"] <- trend.10y$slope.pval
        } else {
          trend.10y <- NULL
          # Write NAs to results.df
          results.df[result.row,"mix10.fun"] <- NA
          results.df[result.row,"mix10.slope"] <- NA
          results.df[result.row,"mix10.logslope"] <- NA
          results.df[result.row,"mix10.P.slope"] <- NA
        }
        # Estimate 5-year trend
        if( min(mod$dat$yr)<=year.6 &  # Require data from 1st and 5th years
            max(mod$dat$yr)>=year.10 ){ 
          # Estimate 5y mixmodel and plot it
          trend.5y <- show_mixmod( mod,yrstr=year.6,yrend=year.10 )
          # Write 5y mixmodel results to results.df
          results.df[result.row,"mix5.fun"] <- trend.5y$metfun
          results.df[result.row,"mix5.slope"] <- trend.5y$slope
          results.df[result.row,"mix5.logslope"] <- trend.5y$logslope
          results.df[result.row,"mix5.P.slope"] <- trend.5y$slope.pval
        } else {
          trend.5y <- NULL
          # Write NAs to results.df
          results.df[result.row,"mix5.fun"] <- NA
          results.df[result.row,"mix5.slope"] <- NA
          results.df[result.row,"mix5.logslope"] <- NA
          results.df[result.row,"mix5.P.slope"] <- NA
        }
      # }
    }  # end mixed effects model block
  
    if( write.png ){
      dev.off()
    }
  
  # Export RData
  setwd(dir.out)
  output <- list( dat.ij = dat.ij, mod = mod,
                  trend.10y = trend.10y, trend.5y = trend.5y,
                  results = results.df[result.row,] )
  save( output, file=paste0(this.station.lab,", ",analytes[j],".RData") )
      
  # Increment result.df row counter  
  result.row <- result.row+1
  }  # end j loop

# Print status to console
cat(paste0( "Completed i = ",i," of ", length(stations), "...\n" ))

}  # end i loop


# Export results.df
setwd(dir.out)
write.csv( results.df, "GAM.results.csv", row.names=FALSE )
save( results.df, file="GAM.results.RData" )
