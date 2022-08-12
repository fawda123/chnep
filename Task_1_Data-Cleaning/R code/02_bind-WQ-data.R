# This script binds the 2011-2020 water quality data from the three data sources (CHNEP Water Atlas,
# SWFWMD-WIN, SWFWMD-STORET) into one dataset for use in trend analysis.

rm(list=ls(all=TRUE)) 

# Load clean data
setwd('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 1')
load('clean-data_CHNEP-WA-WQ_2011-2020.RData') 
  df.CHNEP  <- df3 ;  rm(df3)
load('clean-data_SWFWMD-WIN-WQ_2011-2020.RData')
  df.WIN    <- df2 ;  rm(df2)
load('clean-data_SWFWMD-STORET-WQ_2011-2020.RData')
  df.STORET <- df2 ;  rm(df2)

# Subset columns in each df
df.sub.CHNEP <- df.CHNEP[, c("Unique.StationID","Analyte","Date","Result_Value",
                             "Result_Unit","Value_Qualifier","Non_detect","Depth","Depth_Unit") ]
df.sub.WIN <- df.WIN[, c("Stratum","Analyte","Date","DEP.Result.Value.Number","DEP.Result.Unit",
                         "Value.Qualifier","Non_detect","Activity.Depth","Activity.Depth.Unit") ]
df.sub.STORET <- df.STORET[, c("Stratum","Analyte","Date","Result.Value","Result.Units",
                               "VQ","Non_detect","Act.Depth","Depth.Units") ]

# Standardize column names
colnames( df.sub.WIN ) <- colnames( df.sub.CHNEP )
colnames( df.sub.STORET ) <- colnames( df.sub.CHNEP )

# Bind dataframes into one
clean.data <- rbind( df.sub.CHNEP, df.sub.WIN, df.sub.STORET )

# Create new logical column to indicate records pertaining to polygons (strata) or points (stations)
clean.data$Polygon <- !grepl( ":", clean.data$Unique.StationID ) # All point stations contain ":"

# Create new columns to label station lat/long
coords <- read.csv('station-coordinates.csv')
# Initialize lat/long columns
  clean.data$Latitude <- NA
  clean.data$Longitude <- NA
# Map coords to clean.data
  # Get unique point station labels
  pt.stations <- unique( clean.data$Unique.StationID[which(!clean.data$Polygon)] )
  # Confirm all pt.stations can be found in coords
  all( (pt.stations %in% coords$Unique.StationID) )
  # Loop over point stations to label coordinates
  for( i in 1:length(pt.stations) ){
    this.station <- pt.stations[i]
    this.lat  <- coords$Latitude[ which( coords$Unique.StationID == this.station ) ]
    this.long <- coords$Longitude[ which( coords$Unique.StationID == this.station ) ]
    this.station.idx <- which( clean.data$Unique.StationID == this.station )
    clean.data$Latitude[ this.station.idx ] <- this.lat
    clean.data$Longitude[ this.station.idx ] <- this.long
  }
  # Confirm all point stations' coordinates are labeled
  all( !is.na( clean.data$Latitude[which(!clean.data$Polygon)] ) )
  all( !is.na( clean.data$Longitude[which(!clean.data$Polygon)] ) )
  # Confirm no strata coordinates are labeled
  all( is.na( clean.data$Latitude[which(clean.data$Polygon)] ) )
  all( is.na( clean.data$Longitude[which(clean.data$Polygon)] ) )
  
# Remove duplicate records
  dupes.idx <- which(duplicated(clean.data)) 
  clean.data <- clean.data[ -dupes.idx, ]
  cat( paste0("Removed ",prettyNum(length(dupes.idx),big.mark=",")," duplicate rows in clean.data.\n") )
  
# Write clean dataset to file
  write.csv( clean.data, "clean-data_2011-2020.csv", row.names = FALSE )
  save( clean.data, file = "clean-data_2011-2020.RData" )
