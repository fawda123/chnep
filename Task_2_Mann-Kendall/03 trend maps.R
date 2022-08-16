
rm(list=ls(all=TRUE)) 

# Load libraries
if(!require(rgdal)) { install.packages('rgdal') }; library(rgdal)
if(!require(spsurvey)) { install.packages('spsurvey') }; library(spsurvey)
if(!require(ggmap)) { install.packages('ggmap') }; library(ggmap)
if(!require(maptools)) { install.packages('maptools') }; library(maptools)
if(!require(tidyr)) { install.packages('tidyr') }; library(tidyr)
if(!require(plyr)) { install.packages('plyr') }; library(plyr)

# Set directories
dir.udf <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\User-Defined Functions')
dir.dat <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\Data')
dir.out <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\Output')
dir.outmap <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\Output\\Maps')

# Load data and inputs
setwd( dir.out )
  load("MK.trend.10y.results.RData")  # 5y and 10y year trend directions (up/down) and categories (large/small)
setwd( dir.dat )
  aes.up.red <- read.csv('aes_up-red.csv')
  aes.dn.red <- read.csv('aes_dn-red.csv')
  aes.neutral <- read.csv('aes_neutral.csv')
  aes.analyte <- read.csv('aes_analyte.csv')
  coords.df <- read.csv('station-coordinates.csv')  # lat/long coordinates for point stations
  strat.shp <- readOGR( dsn = "Strata", layer = "CCHMN_Strat_Polys" )  # shapefile for strata polygons


# Define mapping function
trend.map <- function( ANALYTE,
                       DAT = results.df,
                       COORDS = coords.df,
                       STRATA.SHP = strat.shp,
                       AES = list( aes.up.red = aes.up.red, 
                                   aes.dn.red = aes.dn.red,
                                   aes.neutral = aes.neutral,
                                   aes.analyte = aes.analyte ),
                       BBOX = c( left=-82.7, bottom=26.2,
                                 right=-80.5, top=28.2 ),
                       ZOOM = 9,
                       WRITE.PNG = TRUE
){
  
  # Subset data by ANALYTE
  this.dat <- DAT[ which( DAT$Analyte==ANALYTE ),
                   c( which( colnames(DAT)=="Station"),
                      which( colnames(DAT)=="Analyte"),
                      which( colnames(DAT)=="Date.max"),
                      which( colnames(DAT)=="Date.min"),
                      which( colnames(DAT)=="MK.trend"),
                      which( colnames(DAT)=="trend.large") ) ]
  
  # Order data by trend direction to set drawing order of points
    # Re-order data by MK.trend column
    this.dat <- rbind( this.dat[ which( is.na(this.dat$MK.trend) ), ],
                       this.dat[ which( this.dat$MK.trend==0 ), ],
                       this.dat[ which( this.dat$MK.trend==1 ), ],
                       this.dat[ which( this.dat$MK.trend==-1 ), ]
    ) %>% as.data.frame()
    
    # Label trends according to trend direction (-1,0,1) and category (large/small)
    this.dat$Trend <- factor( NA, levels=c( "Up trend, large", "Up trend, small", "No trend",
                                            "Down trend, small", "Down trend, large", "Insufficient data" ) )
    this.dat$Trend[ which( this.dat$MK.trend==1 & this.dat$trend.large==TRUE ) ] <- "Up trend, large"
    this.dat$Trend[ which( this.dat$MK.trend==1 & this.dat$trend.large==FALSE ) ] <- "Up trend, small"
    this.dat$Trend[ which( this.dat$MK.trend==0 & is.na(this.dat$trend.large) ) ] <- "No trend"
    this.dat$Trend[ which( this.dat$MK.trend==-1 & this.dat$trend.large==FALSE ) ] <- "Down trend, small"
    this.dat$Trend[ which( this.dat$MK.trend==-1 & this.dat$trend.large==TRUE ) ] <- "Down trend, large"
    this.dat$Trend[ which( is.na(this.dat$MK.trend) & is.na(this.dat$trend.large) ) ] <- "Insufficient data"
  
  # Label point stations in new logical column
  this.dat$pt.station <- FALSE
  this.dat$pt.station[ which( grepl(":",this.dat$Station)) ] <- TRUE
  pt.idx <- which( this.dat$pt.station==TRUE )
  
  # Label point stations' lat/long coordinates
  this.dat$Latitude <- NA
  this.dat$Longitude <- NA
  for( i in pt.idx ){
    this.dat$Latitude[i]  <- coords.df$Latitude[ which(coords.df$Unique.StationID==this.dat$Station[i]) ]
    this.dat$Longitude[i] <- coords.df$Longitude[ which(coords.df$Unique.StationID==this.dat$Station[i]) ]
  }
  
  # Generate date range labels
  year.end <- year(max(this.dat$Date.max,na.rm=TRUE))
  year.str <- year(max(this.dat$Date.min,na.rm=TRUE))
  
  # Choose aesthetic parameters for point stations as appropriate for the specified analyte
  analyte.long.name <- AES$aes.analyte$long.name[ which( AES$aes.analyte$analyte == ANALYTE ) ] # name for plot title
  analyte.aes <- AES$aes.analyte$aes[ which( AES$aes.analyte$analyte == ANALYTE ) ]  # aes to use (depending on analyte)
  this.aes <- AES[[ which( names(AES)==analyte.aes ) ]]  # Load the appropriate aes
  
  # # Define function to extract coordinates from strata shapefile
  # extract.coords <- function( obj ){
  #   return( obj@Polygons[[1]]@coords )
  #   # return( obj@Lines[[1]]@coords )
  # }  # // end extract.coords()
  # # Extract strata polygon coordinates into a list
  # strata <- lapply( STRATA.SHP@polygons, extract.coords )
  # # Name polygon list items (alphabetical order per STRATA.SHP@data$STRATUM)
  # names(strata) <- c("Cape Haze","East Wall Charlotte Harbor","Estero Bay",
  #                    "Lower Charlotte Harbor","Lower Lemon Bay","Matlacha Pass",
  #                    "Pine Island Sound","San Carlos Bay","Tidal Caloosahatchee River",
  #                    "Tidal Myakka River","Tidal Peace River","Upper Lemon Bay",
  #                    "West Wall Charlotte Harbor")
  # # For each strata polygon, convert coordinates to lat/long
  # for( i in 1:length(strata) ){
  #   colnames( strata[[i]] ) <- c('LONGITUDE','LATITUDE')
  #   # For albersgood() arguments, see STRATA.SHP@proj4string
  #   strata[[i]] <- albersgeod( x=strata[[i]][,1]-400000,
  #                              y=strata[[i]][,2], sph='WGS84', clon=-84, clat=24, sp1=24, sp2=31.3 )
  # }
  
  # Create dataframe from shapefile
  strata.df <- fortify(STRATA.SHP)
  # Specify strata polygon names (in alphabetical order per STRATA.SHP@data$STRATUM)
  strata.names <- c("Cape Haze","East Wall Charlotte Harbor","Estero Bay",
                    "Lower Charlotte Harbor","Lower Lemon Bay","Matlacha Pass",
                    "Pine Island Sound","San Carlos Bay","Tidal Caloosahatchee River",
                    "Tidal Myakka River","Tidal Peace River","Upper Lemon Bay",
                    "West Wall Charlotte Harbor")
  # Assign strata names in a new column
  strata.df$stratum <- mapvalues( x=strata.df$group, from=levels(strata.df$group), to=strata.names )
  # Convert original shapefile's coords to lat/long (for albersgood() arguments, see STRATA.SHP@proj4string)
  strata.df[,c('long','lat')] <- albersgeod( x=strata.df$long-400000,
                                             y=strata.df$lat,
                                             sph='WGS84', clon=-84, clat=24, sp1=24, sp2=31.3 )
  # Assign Trend categories from this.dat to strata.df
  strata.df$Trend <- mapvalues( x = strata.df$stratum,
                                from = this.dat$Station[which(this.dat$pt.station==FALSE)],
                                to = as.character(this.dat$Trend[which(this.dat$pt.station==FALSE)]) )
  
  
  # Load basemap
  basemap <- get_stamenmap( bbox = BBOX,
                            zoom = ZOOM,
                            maptype = "toner-lite" )
  
  # Generate trend map
  ggmap(basemap) +
    geom_point( data = this.dat[pt.idx,],
                aes( x = Longitude, y = Latitude, size = Trend, color = Trend, fill = Trend, shape = Trend ) ) +
    geom_polygon( aes(x = long, y = lat, group = stratum, fill = Trend ),
                  data = strata.df, color = "#87878766", alpha = 0.4 ) +
    scale_size_manual( values = this.aes$size, drop=FALSE ) +
    scale_color_manual( values = this.aes$outline, drop=FALSE ) +
    scale_fill_manual( values = apply( as.matrix(this.aes$fill), 1, function(x) eval(parse(text=x)) ), drop=FALSE ) +
    scale_shape_manual( values = this.aes$shape, drop=FALSE ) +
    ggtitle( paste0( analyte.long.name, ' (', year.str, '-', year.end, ')'  ) ) +
    labs( x = 'Longitude', y = 'Latitude' ) +
    theme( axis.text = element_text(size=12),
           axis.title = element_text(size=14),
           title = element_text(size=14),
           legend.title = element_text(size=14),
           legend.text = element_text(size=12) )
  
  # Write map to png file
  if(WRITE.PNG){
    ggsave( paste0(ANALYTE,'.png'), width=12, height=12, units='in', dpi=600 )
  }
  
}  # // end trend.map()


# Call the function and write plots to file
analytes <- unique( results.df$Analyte )
setwd( dir.outmap )
for( i in 1:length(analytes) ){
    trend.map( ANALYTE=analytes[i], WRITE.PNG=TRUE )
}


