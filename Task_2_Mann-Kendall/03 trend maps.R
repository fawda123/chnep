
rm(list=ls(all=TRUE)) 



# Set directories
dir.udf <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\User-Defined Functions')
dir.dat <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\Data')
dir.out <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\Output')

# Load data 
setwd( dir.out )
load("MK.trend.10y.results.RData")  # trend results in results.df dataframe
setwd( dir.dat )
strata.df <- read.csv('CHNEPstationstoGrids.csv')  # stratum-to-station associations
coords.df <- read.csv('CHNEP - WQ Trend Data - Stations.csv')  # stations' lat-long
aes.up.red <- read.csv('aes_up-red.csv')
aes.dn.red <- read.csv('aes_dn-red.csv')
aes.neutral <- read.csv('aes_neutral.csv')
aes.analyte <- read.csv('aes_analyte.csv')



# Define mapping function
trend.map <- function( ANALYTE,
                       DAT = results.df,
                       COORDS = coords.df,
                       AES = list( aes.up.red = aes.up.red, 
                                   aes.dn.red = aes.dn.red,
                                   aes.neutral = aes.neutral,
                                   aes.analyte = aes.analyte ),
                       STRATA = strata.df,
                       BBOX = c( left=-82.7, bottom=26.2,
                                 right=-80.5, top=28.2 ),
                       ZOOM = 9,
                       WRITE.PNG = FALSE
                       ){
  
  # Load libraries
  if(!require(plyr)) { install.packages('plyr') }; library(plyr)
  if(!require(tidyr)) { install.packages('tidyr') }; library(tidyr)
  if(!require(ggmap)) { install.packages('ggmap') }; library(ggmap)
  
  # Subset data for specified analyte
  this.dat <- DAT[ which( DAT$Analyte == ANALYTE ), ]
  
  # Order data by MK.trend to determine drawing order of points
  this.dat <- rbind( this.dat[ which( is.na(this.dat$MK.trend) ), ],
                     this.dat[ which( this.dat$MK.trend==0 ), ],
                     this.dat[ which( this.dat$MK.trend==1 ), ],
                     this.dat[ which( this.dat$MK.trend==-1 ), ]
  ) %>% as.data.frame()
  
  # Format mapping dataframe
  # Map strata labels to 'coords' stations
  COORDS$Strata <- mapvalues( x=COORDS$StationID,
                              from=STRATA$STATIONID, to=STRATA$Stratum,
                              warn_missing=FALSE )
  # Map lat-long coordinates to stations
  this.dat$lat  <- mapvalues( x=this.dat$Station,
                              from=COORDS$Strata, to=COORDS$Latitude_DD, 
                              warn_missing=FALSE ) %>% as.numeric()
  this.dat$lon <- mapvalues( x=this.dat$Station,
                             from=COORDS$Strata, to=COORDS$Longitude_DD,
                             warn_missing=FALSE ) %>% as.numeric()
  
  # Label trends according to MK result (-1,0,1) and slope classification (large/small)
  this.dat$Trend <- factor( NA, levels=c( "Up trend, large", "Up trend, small", "No trend",
                                             "Down trend, small", "Down trend, large", "Insufficient data" ) )
  this.dat$Trend[ which( this.dat$MK.trend==1 & this.dat$trend.large==TRUE ) ] <- "Up trend, large"
  this.dat$Trend[ which( this.dat$MK.trend==1 & this.dat$trend.large==FALSE ) ] <- "Up trend, small"
  this.dat$Trend[ which( this.dat$MK.trend==0 & is.na(this.dat$trend.large) ) ] <- "No trend"
  this.dat$Trend[ which( this.dat$MK.trend==-1 & this.dat$trend.large==FALSE ) ] <- "Down trend, small"
  this.dat$Trend[ which( this.dat$MK.trend==-1 & this.dat$trend.large==TRUE ) ] <- "Down trend, large"
  this.dat$Trend[ which( is.na(this.dat$MK.trend) & is.na(this.dat$trend.large) ) ] <- "Insufficient data"
  
  # Choose aesthetic parameters as appropriate for the specified analyte
  analyte.long.name <- AES$aes.analyte$long.name[ which( AES$aes.analyte$analyte == ANALYTE ) ] # name for plot title
  analyte.aes <- AES$aes.analyte$aes[ which( AES$aes.analyte$analyte == ANALYTE ) ]  # aes to use (depending on analyte)
  this.aes <- AES[[ which( names(AES)==analyte.aes ) ]]  # Load the appropriate aes
  
  # Generate date range labels
  # Starting month and year
  month.str <- this.dat$Date.min %>% min(na.rm=TRUE) %>% lubridate::month(label=TRUE,abbr=TRUE)
  year.str  <- this.dat$Date.min %>% min(na.rm=TRUE) %>% lubridate::year()
  date.str  <- paste( month.str, year.str, collapse=' ' )
  # Ending month and year
  month.end <- this.dat$Date.max %>% max(na.rm=TRUE) %>% lubridate::month(label=TRUE,abbr=TRUE)
  year.end  <- this.dat$Date.max %>% max(na.rm=TRUE) %>% lubridate::year()
  date.end  <- paste( month.end, year.end, collapse=' ' )
  
  # Load basemap
  basemap <- get_stamenmap( bbox = BBOX,
                            zoom = ZOOM,
                            maptype = "terrain" )
  # Generate trend map
  ggmap(basemap) +
    geom_point( data = this.dat,
                aes( x = lon, y = lat, size = Trend, color = Trend, fill = Trend, shape = Trend ) ) +
                scale_size_manual( values = this.aes$size, drop=FALSE ) +
                scale_color_manual( values = this.aes$outline, drop=FALSE ) +
                scale_fill_manual( values = apply( as.matrix(this.aes$fill), 1, function(x) eval(parse(text=x)) ), drop=FALSE ) +
                scale_shape_manual( values = this.aes$shape, drop=FALSE ) +
    ggtitle( paste0( analyte.long.name, ' (', date.str, ' - ', date.end, ')'  ) ) +
    labs( x = 'Longitude', y = 'Latitude' ) +
    theme( axis.text = element_text(size=12),
           axis.title = element_text(size=14),
           title = element_text(size=14),
           legend.title = element_text(size=14),
           legend.text = element_text(size=12) )
  if(WRITE.PNG){
    ggsave( paste0(ANALYTE,'.png'), width=12, height=12, units='in', dpi=600 )
    
  }
  
}  # // end trend.map()


# Call the function and write plots to file

analytes <- unique( results.df$Analyte )
setwd( dir.out )
for( i in 1:length(analytes) ){
  trend.map( ANALYTE=analytes[i], WRITE.PNG=TRUE )
}


