rm(list=ls(all=TRUE)) 

# Load libraries
  if(!require(plyr)) { install.packages('plyr') } ;  library(plyr)  # mapvalues
  if(!require(dplyr)) { install.packages('dplyr') } ;  library(dplyr)  
  if(!require(tidyr)) { install.packages('tidyr') } ;  library(tidyr) 
  if(!require(data.table)) { install.packages('data.table') } ;  library(data.table)

# Load data provided by USF Water Atlas
  setwd('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\USF docs')
  dat <- read.table('Trend Analysis - CHNEP - 2020 - Export.txt',sep="|",header=TRUE,quote="",fill=TRUE)
  # dat <- read.table('CHNEP - WQ Trend Data - 2020 with Activity Type.txt',sep="|",header=TRUE,quote="")
  # dat <- read.table('CHNEP - WQ Trend Data - 2020 - Pipe.txt',sep="|",header=TRUE,quote="")
  strata <- read.csv('CHNEPstationstoGrids.csv')


# Coerce data classes in new dataframe (df)
  df <- dat
  df$DataSource <- df$DataSource %>% as.factor()
  df$StationID  <- df$StationID %>% as.factor()
  df$Activity_Type <- df$Activity_Type %>% as.factor()
  df$Activity_Start_Date <- df$Activity_Start_Date %>% as.Date(format="%Y-%m-%d")
  df$RelativeDepth       <- df$RelativeDepth %>% as.factor()
  df$Characteristic <- df$Characteristic %>% as.factor()
  df$Result_Unit <- df$Result_Unit %>% as.factor()
  df$Activity_Depth_Unit <- df$Activity_Depth_Unit %>% as.factor()

# Subset by date in new dataframe (df1)
  min.date <- as.Date('2011-01-01')
  max.date <- as.Date('2020-12-31')
  df1 <- df[ which( df$Activity_Start_Date >= min.date & df$Activity_Start_Date <= max.date) ,]
  colnames(df1)[which(colnames(df1)=='Activity_Start_Date')] <- 'Date'
  

# Clean up factor levels and redundancies
  # DataSource
    unique(df1$DataSource)
    length(levels(df1$DataSource)) == length(unique(df1$DataSource))
    df1$DataSource <- droplevels(df1$DataSource)
  # StationID
    unique(df1$StationID)
    length(levels(df1$StationID)) == length(unique(df1$StationID))
    df1$StationID <- droplevels(df1$StationID)
                  # *****   did not thoroughly check StationID *****************************************
  # Activity_Type
    unique(df1$Activity_Type)
    length(levels(df1$Activity_Type)) == length(unique(df1$Activity_Type))
    df1$Activity_Type <- droplevels(df1$Activity_Type)
  # RelativeDepth
    unique(df1$RelativeDepth)
    length(levels(df1$RelativeDepth)) == length(unique(df1$RelativeDepth))
    # Result_Unit
    unique(df1$Result_Unit)
    length(levels(df1$Result_Unit)) == length(unique(df1$Result_Unit))
  # Activity_Depth_Unit
    unique(df1$Activity_Depth_Unit) %>% as.character()
    length(levels(df1$Activity_Depth_Unit)) == length(unique(df1$Activity_Depth_Unit))
    # df1$Activity_Depth_Unit[ which(df1$Activity_Depth_Unit=='m  ') ] <- 'm' # convert all 'm  ' to 'm'
    length(levels(df1$Activity_Depth_Unit)) == length(unique(df1$Activity_Depth_Unit))
    # df1$Activity_Depth_Unit <- df1$Activity_Depth_Unit %>% as.factor()  
    df1$Activity_Depth_Unit <- droplevels(df1$Activity_Depth_Unit)
  # Characteristic
    unique(df1$Characteristic) %>% sort() # Several analyte names are redundant
    df1$Characteristic[ which(df1$Characteristic=='Temperature, Water') ] <- 'Temperature, water'
    df1$Characteristic[ which(df1$Characteristic=='Dissolved Oxygen Saturation') ] <- 'Dissolved oxygen saturation'
    df1$Characteristic[ which(df1$Characteristic=='Fecal Coliform') ] <- 'Fecal coliform'
    df1$Characteristic[ which(df1$Characteristic=='Specific Conductance') ] <- 'Specific conductance'
    df1$Characteristic <- droplevels(df1$Characteristic)
    length(levels(df1$Characteristic)) == length(unique(df1$Characteristic))


# Abbreviate analytes in new column
  analytes.old <- levels(df1$Characteristic)
  analytes.new <- c( 'Color', 'BOD', 'Chl-a', 'DO Conc', 'DO Sat',  'Fecal', 'TN',  'NH4',  'TKN',
                     'NOx',   'pH',   'TP',   'PO4',     'Sp Cond', 'Temp',  'TSS', 'Turbidity'     )
  df1$Analyte <- mapvalues( df1$Characteristic, analytes.old, analytes.new ) 
  analytes <- unique( df1$Analyte ) %>% sort() %>% as.character() # unique analytes
  
  
# Result Units
  # Print unique units for each analyte. Several analytes are missing unit labels.
  unique(df$Result_Unit)
  for( i in 1:length(levels(df1$Analyte)) ){  # Print units associated with each analyte
    this.analyte <- levels(df1$Analyte)[i]
    this.units   <- unique( df1$Result_Unit[which(df1$Analyte==this.analyte)] )
    cat( paste0( this.analyte, ' -- ', this.units, '\n' ) )
  }  # // end i loop

  # Explore distributions of reported values among records with units labeled vs. not labeled
    analytes.unlabeled <- unique( df1$Analyte[which(df1$Result_Unit=='')] )
    analytes.unlabeled.idx <- which( analytes %in% analytes.unlabeled )
  # Histograms
    par(mfrow=c(4,2))
    # Records WITHOUT labeled units (blue histograms)
    for( i in analytes.unlabeled.idx ){
      this.analyte <- analytes[i]
      this.unit    <- '?????'
      # Print each analyte's units to console
      cat(paste0(i,'. ',this.analyte,": ",this.unit,'\n'))
      if(this.unit[1]=="None"){ this.unit<-'' }
      # Draw a histogram for each analyte
      hist.data <- df1$Result_Value[ which(df1$Analyte==this.analyte & df1$Result_Unit=='') ]
      if(length(hist.data)==0){ next }
      hist( hist.data, breaks=100,
            border=rgb(1,1,1,1), col=rgb(0,0.2,0.9,1),
            main=this.analyte, xlab=this.unit
      )
      legend('topright',bty='n',legend=( c(paste('min:',min(hist.data)),
                                           paste('max:',max(hist.data)),
                                           paste('n:',length(hist.data))
      ))
      )
    }  # end i loop (blue histograms)
    # Records WITH labeled units (red histograms)
    par(mfrow=c(4,2))
    for( i in analytes.unlabeled.idx ){
      this.analyte <- analytes[i]
      this.unit    <- unique( df1$Result_Unit[ which(df1$Analyte==this.analyte) ] )
      # Print this analyte's units to console
      cat(paste0(i,'. ',this.analyte,": ",this.unit,'\n'))
      if(this.unit[1]=="None"){ this.unit<-'' }
      # Draw a histogram for each analyte
      this.unit <- this.unit[ which( !(this.unit %in% '' ) ) ] %>% as.character()
      hist.data <- df1$Result_Value[ which(df1$Analyte==this.analyte & df1$Result_Unit==this.unit) ]
      hist( hist.data, breaks=100,
            border=rgb(1,1,1,1), col=rgb(0.9,0.2,0,1),
            main=this.analyte, xlab=this.unit
      )
      legend('topright',bty='n',legend=( c(paste('min:',min(hist.data)),
                                           paste('max:',max(hist.data)),
                                           paste('n:',length(hist.data))
      ))
      )
    }  # end i loop (red histograms)
  # Explore data sources WITHOUT units labels: exclusively from LEE_PONDWATCH_WQ
    for( i in analytes.unlabeled.idx){
      this.analyte <- analytes[i]
      this.sources <- unique( df1$DataSource[ which(df1$Analyte==this.analyte &
                                                    df1$Result_Unit=='')] )
      cat(paste0(i,'. ',this.analyte,": ",this.sources,'\n'))
    }  # // end i loop
  # Add unit labels where they are missing. Per USF, data are reported in consistent units. Histograms are
  # consistent with this assertion; distributions of unlabeled records are well within ranges of data with labeled units.
    for( i in analytes.unlabeled.idx ){
      this.analyte <- analytes[i]
      this.unlabeled.idx <- which( df1$Analyte==this.analyte & df1$Result_Unit=='' ) # unlabeled unit indices
      this.units <- unique(df1$Result_Unit[which(df1$Analyte==analytes[i])])  # unit labels
      replacement.unit <- this.units[ which( !(this.units %in% '' ) ) ] %>% as.character() # extract unit string
      if( length(replacement.unit)>1 ){ stop('More than one replacement unit found.') }  # check if >1 strings
      df1$Result_Unit[this.unlabeled.idx] <- replacement.unit  # replace blank unit with string
    }  # // end i loop
  # Convert PO4 from mg/l to ug/l
    df1$Result_Value[ which(df1$Analyte=='PO4') ] <- df1$Result_Value[ which(df1$Analyte=='PO4') ]*1000
    df1$Result_Unit[ which(df1$Analyte=='PO4') ]  <- 'ug/l'
  # Confirm that all units are labeled and drop unused levels
    which(df1$Result_Unit=='')
    df1$Result_Unit <- droplevels( df1$Result_Unit )
  
  
# Remove field and equipment blanks (and retain 'Field Replicate' samples)
  df1 <- df1[ -which( df1$Activity_Type %in% c('Equipment Blank','Field Blank')), ]  # Remove blanks
  df1$Activity_Type <- df1$Activity_Type %>% droplevels()  # Drop associated factor levels
  
  
# Sample depth units
# Issues: Depths reported without units and depths reported in feet rather than meters
  df1$Depth <- df1$Activity_Depth  # new depth column
  df1$Depth_Unit <- df1$Activity_Depth_Unit  # new depth unit column
  unique(df1$Depth_Unit)  # depths are expressed in both feet and meters, and some depths have no units
  # Print depth units used by each data source that reported depths without units:
    # SARASOTA_COASTALCREEK_WQ and STORET_21FLCHAR records include depths without units, and elsewhere in both ft and m.
    # SWFWMD_HYDRO and USGS_NWIS records do not indicate depth units.
    # Other data sources with missing depth units elsewhere report depths in m.
    no.depth.stations <- unique( df1$DataSource[ which(df1$Depth_Unit=='') ] )  # Stations with missing depth units
    for( i in 1:length(no.depth.stations) ){  
      this.units <- unique( df1$Depth_Unit[ which(df1$DataSource==no.depth.stations[i]) ] )
      cat( paste0( no.depth.stations[i], ': ', this.units, '\n' ) )
    }  # // end i loop
  # Standardize depth units (convert feet to meters)
    df1$Depth[ which(df1$Depth_Unit=='ft') ] <- df1$Depth[ which(df1$Depth_Unit=='ft') ] * 0.3048  # convert feet to meters
    df1$Depth_Unit[ which(df1$Depth_Unit=='ft') ] <- 'm'  # correct units to meters
    unique(df1$Depth_Unit) %>% as.character()  # depths are expressed in meters or missing units
  # Explore depths that are missing unit labels
    df1$DataSource[which(df1$Depth_Unit=='')] %>% unique()  # unitless depths come from 12 data sources
    df1$StationID[which(df1$Depth_Unit=='')] %>% unique()  # unitless depths come from 95 stations
    which(df1$Depth_Unit=='') %>% length()  # In total, almost 30k records have no depth units specified
    # Plot depths with and without unit labels
    par(mfrow=c(3,2))
      # Without units
      df1$Depth[which(df1$Depth_Unit=='')] %>%
        hist(breaks=30,freq=F,col=rgb(0,0.4,0.9,0.4),border=rgb(1,1,1,1),
             main='without units',xlab='depth (???)')
      df1$Depth[which(df1$Depth_Unit=='')] %>% sort() %>%
        plot(pch=16,col=rgb(0,0.4,0.9,0.4),cex=0.5,main='without units',xlab='No. of samples',ylab='depth (???)')
        abline( h=seq(0,6,0.5), col=rgb(0,0,0,0.3) )
      # With units (0-6 m)
      df1$Depth[which(df1$Depth_Unit!='' & df1$Depth<=6)] %>%
        hist(breaks=30,freq=F,col=rgb(0.9,0.2,0.1,0.4),border=rgb(1,1,1,1),main='with units',xlab='depth (m)')
      df1$Depth[which(df1$Depth_Unit!='' & df1$Depth<=6)] %>% sort() %>%
        plot(pch=16,col=rgb(0.9,0.2,0.1,0.4),cex=0.5,main='with units',xlab='No. of samples',ylab='depth (m)')
        abline( h=seq(0,6,0.5), col=rgb(0,0,0,0.3) )
      # With units (all)
      df1$Depth[which(df1$Depth_Unit!='')] %>%
          hist(breaks=50,freq=F,col=rgb(0.9,0.2,0.1,0.4),border=rgb(1,1,1,1),main='with units',xlab='depth (m)')
      df1$Depth[which(df1$Depth_Unit!='')] %>% sort() %>%
          plot(pch=16,col=rgb(0.9,0.2,0.1,0.4),cex=0.5,main='with units',xlab='No. of samples',ylab='depth (m)')
      abline( h=seq(0,30,1), col=rgb(0,0,0,0.3) )
    # Assume the unlabeled depths are expressed in feet, and convert the assumed feet values to meters
      missing.depth.idx <- which( !is.na(df1$Activity_Depth) & df1$Depth_Unit=='' )
      df1$Depth[ missing.depth.idx ] <- df1$Depth[ missing.depth.idx ] * 0.3048  # convert (assumed) feet to meters
      df1$Depth_Unit[ missing.depth.idx ] <- as.factor('m')  # Assign 'm' to entire Depth_Unit column
      df1$Depth_Unit <- droplevels( df1$Depth_Unit )  # drop unused levels
  
    
# Sample depth
  # Retain surface samples only, assuming surface if not reported (according to conditionals below)
  surface.idx <- which( df1$Depth<=1 |   # reported depth no more than one meter, OR 
                          ( df1$RelativeDepth=='Surface' & is.na(df1$Depth)) | # labeled 'Surface' with no depth reported, OR
                          ( df1$RelativeDepth=='' & is.na(df1$Depth) )  )  # no depth info reported
  df1.nonsurface <- df1[ -surface.idx, ]
  df1 <- df1[ surface.idx, ]
  # Explore depth information in df1.nonsurface to ensure no surface records were unnecessarily discarded
  df1.nonsurface %>% nrow()  # 81k records
  df1.nonsurface[ which(df1.nonsurface$Depth<=1), ] %>% nrow()  # no records with Depth<=1
  df1.nonsurface$RelativeDepth %>% unique() %>% as.character()  # "", "Surface", "Midwater"
  df1.nonsurface[ which( df1.nonsurface$RelativeDepth=="Surface"), ] %>% nrow()  # 14.5k "Surface" records
  df1.nonsurface$Depth[ which( df1.nonsurface$RelativeDepth=="Surface") ] %>% range()  # min is >1 m
  df1.nonsurface$Depth[ which( df1.nonsurface$RelativeDepth=="") ] %>% range()  # min is >1 m
 
  
# Remove duplicate records
  dupes.1 <- which(duplicated(df1))  # Find duplicate entries 
  dupes.2 <- which(duplicated(df1,fromLast=TRUE))  # Find the corresponding first entries
  dupes <- cbind(dupes.1,dupes.2)  # Table of duplicate rows
  df2 <- df1[-dupes.1,]  # Remove duplicates and store result in a new dataframe
  which(duplicated(df2))  # Confirm no duplicates in new dataframe
  
  
# Stations and strata (some sample locations are one-offs, randomly sampled within a wider area, or 'stratum')
  # Create new column grouping stations by stratum (not all 'strata' stations appear in 'df2')
  df2$Stratum <- mapvalues( x=df2$StationID, from=strata$STATIONID, to=strata$Stratum, warn_missing=FALSE )
  # Check that StationID-to-stratum associations are one-to-one or many-to-one
  unique.strata <- unique( df2$Stratum )
  n.stations.in.strata <- c()
  for( i in 1:length(unique.strata) ){
    this.stations <- unique( df2$StationID[which(df2$Stratum==unique.strata[i])] )
    if(length(this.stations)>1){ 
      cat( paste0( unique.strata[i],'::: ',paste(this.stations,collapse='; ',sep=' '),'\n') )
    }  # // end if()
    n.stations.in.strata <- c( n.stations.in.strata, length(this.stations) )  # Confirm one or more stations per stratum
  }  # // end i loop
  unique(n.stations.in.strata)
  # Check that there is no more than one stratum per station
  unique.stations <- unique( df2$StationID )
  n.strata.in.stations <- c()
  for( i in 1:length(unique.stations) ){
    this.stratum <- unique( df2$Stratum[which(df2$StationID==unique.stations[i])] )
    if(length(this.stratum)>1){
      cat( paste0( unique.stations[i],'::: ',paste(this.stratum,collapse='; ',sep=' '),'\n') )
    }  # // end if()
    n.strata.in.stations <- c( n.strata.in.stations, length(this.stratum) )
  }  # // end i loop
  unique(n.strata.in.stations)  # Confirm no more than one stratum per station
  
  
  # Fatal qualifier codes
  # First, check whether any qualifier codes in the dataset stand out as different from FDEP codes.
  # Explore qualifier codes associated with each DataSource
    datasources <- unique(df2$DataSource)
    for( i in 1:length(datasources) ){
      this.codes <- unique( df2$Value_Qualifier[which(df2$DataSource==datasources[i])] )
      cat( paste0( datasources[i],': ',paste(this.codes,collapse=' ',sep=' '),'\n') )
    }  # // end i loop
  # Load qualifier code definitions provided by USF Water Atlas and group codes by DataSource
    code.defs <- read.csv('WA-Quality-Assurance-Code-Definitions_3_csv.csv')  # Load code definitions from file
    colnames(code.defs) <- c('DataSource','Code','Meaning')  # Rename columns
    code.defs$DataSource[ which(code.defs=='') ] <- 'FDEP'  # Label blank DataSources as 'FDEP', per Water Atlas documentation
    codes.by.source <- aggregate( code.defs$Code, by=list(code.defs$DataSource), FUN=unlist, simplify=FALSE ) # Aggregate Codes by DataSource
    colnames(codes.by.source) <- c('DataSource','Code')  # Rename columns
  # Compare codes present in the dataset with code definitions provided by USF Water Atlas
    unique(df2$DataSource)[ which( unique(df2$DataSource) %in% codes.by.source$DataSource ) ]  # Two DataSources in the dataset appear in the USFWA list
    df2$Value_Qualifier[ which( df2$DataSource=='USGS_NWIS') ] %>% unique()  # According to USGS, 'P' indicates 'preferred'. No action needed.
    df2$Value_Qualifier[ which( df2$DataSource=='SWFWMD_HYDRO') ] %>% unique() # No qualifiers in SWFWMD_HYDRO data. No action needed.
  
  
  # Specify fatal codes
  invalid.codes <- c("*","?","A","B","G","H","J","K","L","N","O","Q","T","V","Y","Z")
  # invalid.codes <- c( "A", "F", "G", "H", "K", "L", "N", "O", "T", "V", "Y", "?" ) # add "B", "J", "Q", "Z" ?
  # Define function to identify rows containing fatal codes.
  # Some observations are flagged with multiple qualifer codes, so each character in the code has to be evaluated separately.
  # For example, if "V" is invalid, we want any rows containing "V" to be identified:  "V", "I,V", "I, V".
  # This is accomplished by parsing the qualifier code string into single characters, and checking if any of the characters
  # match the codes in 'invalid.codes'.
  find.invalid <- function( QUALIFIER, INVALID=invalid.codes ){  # QUALIFER argument is a character string
    input.code <- QUALIFIER %>% strsplit(split='') %>% unlist()  # parse the qualifier string into single characters
    invalid <- input.code %in% INVALID %>% any()  # check if any input characters match the invalid codes
    return( invalid )  # return TRUE or FALSE
  }  #  // end find.invalid() function
  # Identify rows containing invalid values to be removed
  invalid.rows <- apply( matrix(df2$Value_Qualifier,ncol=1), 1, find.invalid ) %>% which()
  # If any invalid rows identified, remove them and save new dataframe as 'df3'
  if( length(invalid.rows)>0 ){
    df3 <- df2[ -invalid.rows, ] 
  } else {  # else, save 'df2' as 'df3'
    df3 <- df2
  }  # // end if()
  # Drop unused factor levels from Qualifier column
  df3$Value_Qualifier <- df3$Value_Qualifier %>% as.factor()
  # Print result to console
  codes.string <- do.call( paste, c( as.list(invalid.codes), sep=', ') )
  cat( paste( length(invalid.rows),'invalid values removed.  Invalid qualifiers: ',codes.string, '\n' ) )
  
  # Check remaining codes
  unique(df3$Value_Qualifier)  # Print codes to console
  df3[ which(df3$Value_Qualifier=='R1'), ]  # LEE_WQ  ********** look up 'R1'; emailed Keith Kibbey 3/16 *********
  df3[ which(df3$Value_Qualifier=='IC'), ]  # SARASOTA_COASTALCREEK_WQ  ************* look up 'IC' and 'C'; emailed John Ryan 3/16 
  df3[ which(df3$Value_Qualifier=='UC'), ]  # SARASOTA_COASTALCREEK_WQ  ************* look up 'UC' and 'C'   ***********


# Non-detects and MDLs (use MDL for non-detects)
  # Print all MDL units
    df3$MDL_Unit %>% unique() %>% sort()
  # Replace "None" with "" for all missing MDL units
    df3$MDL_Unit[ which( df3$MDL_Unit=='None') ] <- ""
    
  # Label non-detects in a new column
    # Specify function to find records flagged with 'U'
    find.U <- function( QUALIFIER, CODE='U' ){  # QUALIFER argument is a character string
      QUALIFIER <- toupper(QUALIFIER)  # capitalize the input
      input.code <- QUALIFIER %>% strsplit(split='') %>% unlist()  # parse the qualifier string into single characters
      u.code <- input.code %in% CODE %>% any()  # check if any input characters contain U
      return( u.code )  # return TRUE or FALSE
    }  #  // end find.U() function
    # Apply find.U() and store results in new column
    df3$Non_detect <- apply( matrix(df3$Value_Qualifier,ncol=1), 1, find.U )
    # Print qualifier codes labeled Non_detect==TRUE
    df3$Value_Qualifier[ which( df3$Non_detect==TRUE ) ] %>% unique() %>% as.character()
    # Print qualifier codes labeled Non_detect==FALSE
    df3$Value_Qualifier[ which( df3$Non_detect==FALSE ) ] %>% unique() %>% as.character()
    
  # Apply Rule 1: Non-detects with MDL_Unit != Result_Unit
    # Find analytes with MDL_Unit != Result_Unit
    for( i in 1:length(analytes) ){
      this.analyte <- levels(df3$Analyte)[i]
      MDL.units <- df3$MDL_Unit[ which( df3$Analyte==this.analyte &
                                          !is.na(df3$MDL) &
                                          df3$Non_detect==TRUE ) ] %>% unique()
      if( length(MDL.units)==0 ){ MDL.units <- "(No non-detects found)" }
      cat( paste0( this.analyte, ' -- ', MDL.units, '\n' ) )
    }  # // end i loop
    # Result: MDLs for TN, NH4, TKN, NOx, TP, and PO4 are in mg/l or ppm (results are in ug/l).
    # Confirm all nitrogen MDL values are mg/l
    df3$MDL[ which( df3$Analyte %in% c('TN', 'NH4', 'TKN', 'NOx') &
                      !is.na(df3$MDL) & df3$Non_detect==TRUE) ] %>% unique() %>% sort()
    # Confirm all phosphorus MDL values are mg/l
    df3$MDL[ which( df3$Analyte %in% c('PO4','TP') &
                      !is.na(df3$MDL) & df3$Non_detect==TRUE) ] %>% unique() %>% sort()
    # Convert nitrogen and phosphorus MDLs from mg/l to ug/l
    MDL.idx <- which( df3$Analyte %in% c('TN', 'NH4', 'TKN', 'NOx','PO4','TP') &
                        !is.na(df3$MDL) & df3$Non_detect==TRUE)
    df3$MDL[ MDL.idx ] <- df3$MDL[ MDL.idx ] * 1000  # convert mg/l to ug/l
    df3$MDL_Unit[ MDL.idx ] <- "ug/l"
    
  # Apply Rule 2: Non-detects reporting MDLs without units
    # For each analyte, explore 'target' MDL values (missing units) and compare with all MDL values
    for( i in 1:length(analytes) ){
      this.analyte <- analytes[i]
      targets.idx <- which( df3$Analyte==this.analyte &
                              !is.na(df3$MDL) &
                              df3$MDL_Unit=='' &
                              df3$Non_detect==TRUE )
      if( length(targets.idx)==0 ){
        targets.MDL <- "(No values found)"
        all.MDL <- "(skip)"
      } else {
        targets.MDL <- df3$MDL[ targets.idx ] %>% unique() %>% sort()
        all.MDL <- df3$MDL[ which( df3$Analyte==this.analyte &
                                     !is.na(df3$MDL) ) ] %>% unique() %>% sort()
      }  # end if()
      cat( paste('===============', this.analyte, '=========================\n') )
      cat( paste('Target MDL values:',  paste(targets.MDL,collapse=', '), '\n') )
      cat( paste('All MDL values:', paste(    all.MDL,collapse=', '), '\n\n') )
    }  # // end i loop
    # Result: All target MDLs appear to use the same MDL units used elsewhere
    # Assign labels where they are missing
    analytes.rule2 <- c('BOD','Chl-a','Fecal','TSS')
    units.rule2 <- c('mg/l','ug/l','cfu/100ml','mg/l')
    for( i in 1:length(analytes.rule2) ){
      this.analyte <- analytes.rule2[i]
      targets.idx <- which( df3$Analyte==this.analyte &
                              !is.na(df3$MDL) &
                              df3$MDL_Unit=='' &
                              df3$Non_detect==TRUE )
      df3$MDL_Unit[ targets.idx ] <- units.rule2[i] 
    }  # // end i loop
    
  # Apply Rule 3: Non-detects without MDL information
    # For each analyte, check whether Result_Values can be assumed to reflect MDL values
    for( i in 1:length(analytes)){
      this.analyte <- analytes[i]
      targets.idx <- which( df3$Analyte==this.analyte &
                              is.na(df3$MDL) &
                              df3$Non_detect==TRUE )
      if( length(targets.idx)==0 ){
        targets.results <- "(No Values found)"
        targets.range   <- "(skip)"
        all.nondetect.MDL <- "(skip)"
        all.MDL <- "(skip)"
      } else {
        targets.results <- df3$Result_Value[ targets.idx ] %>% unique() %>% sort()
        targets.range   <- range( targets.results )
        all.nondetect.MDL <- df3$MDL[ which( df3$Analyte==this.analyte & df3$Non_detect==TRUE &
                                               !is.na(df3$MDL) ) ] %>% unique() %>% sort()
        all.MDL <- df3$MDL[ which( df3$Analyte==this.analyte & 
                                     !is.na(df3$MDL) ) ] %>% unique() %>% sort()
      }  # // end if()
      cat( paste('===============', this.analyte, '=========================\n') )
      cat( paste('Target Result values:', paste(targets.results,collapse=', '), '\n') )
      cat( paste('Target Result range: ', paste(  targets.range,collapse=' - '), '\n\n') )
      cat( paste('All non-detect MDL values:', paste(all.nondetect.MDL,collapse=', '), '\n\n') )
      cat( paste('All MDL values:', paste(all.MDL,collapse=', '), '\n\n') )
    }  # // end i loop
    
    # Explore large (>8) Result values among non-detect Color samples
      df3[ which( df3$Analyte=='Color' &
                    df3$Result_Value>8 & df3$Non_detect==TRUE), ]
      # Each of these 13 records cite elevated MDL due to sample matrix interference (see **Result_Comment**)
      # Discard these 13 records.
      df3 <- df3[ -which( df3$Analyte=='Color' &
                            df3$Result_Value>8 & df3$Non_detect==TRUE), ]
    
    # Explore low Result values (0.008) among non-detect NH4 samples
      NH4.008.idx <- which( df3$Analyte=='NH4' &
                              df3$Result_Value==0.008 & df3$Non_detect==TRUE)
      df3[ NH4.008.idx, ] %>% nrow()
      df3$Result_Unit[ NH4.008.idx ] %>% unique()  # units say 'ug/l'
      # Convert convert these values from mg/l to ug/l
      df3$Result_Value[ NH4.008.idx ] <- df3$Result_Value[ NH4.008.idx ] * 1000  # mg/l to ug/l
    
    # Explore large Result values (>100) among non-detect TKN samples.
      df3[ which( df3$Analyte=='TKN' &
                    df3$Result_Value>100 & df3$Non_detect==TRUE), ]
      # MDLs of 160-640 ug/l might be possible, but an MDL of 1100 ug/l seems highly unlikely. 
      # Discard the two records with non-detect values of 1100 ug/l
      df3 <- df3[ -which( df3$Analyte=='TKN' &
                            df3$Result_Value==1100 & df3$Non_detect==TRUE), ]
      
  # Apply Rule 4: For all non-detects with MDL information, assign MDLs as Result_Values
    mismatch.idx <- which( df3$Non_detect==TRUE &
                             !is.na(df3$MDL) &
                             df3$MDL!=df3$Result_Value )
    df3[ mismatch.idx, ]  # 3 records found
    # Correct mismatched values
    df3$Result_Value[ mismatch.idx ] <- df3$MDL[ mismatch.idx ]

  
# Values and thresholds
  # Explore distributions of each analyte
    par(mfrow=c(5,4))
    for( i in 1:length(analytes) ){
      this.analyte <- analytes[i]
      this.unit    <- unique( df3$Result_Unit[ which(df3$Analyte==this.analyte) ] )
      # Print each analyte's units to console
      cat(paste0(i,'. ',this.analyte,": ",this.unit,'\n'))
      if(this.unit[1]=="None"){ this.unit<-'' }
      # Draw a histogram for each analyte
      hist( df3$Result_Value[ which(df3$Analyte==this.analyte) ], breaks=50,
            border=rgb(1,1,1,1), col=rgb(0,0.2,0.9,1),
            main=this.analyte, xlab=this.unit
            )
      legend('topright',bty='n',legend=range(df3$Result_Value[which(df3$Analyte==this.analyte)]))
    }  # end i loop
  # Screen data outside specified thresholds
    # Load thresholds from file
    thresholds <- read.csv("thresholds.csv")
    # Initialize dataframe to store summary
    threshold.screening <- data.frame( analyte=analytes, n.removals=NA )
    # Loop over analytes and screen out data outside of specified thresholds
    for( i in 1:length(analytes) ){
      # Verify thresholds units match data units
      this.unit <- unique(df3$Result_Unit[which(df3$Analyte==analytes[i])])
      if( thresholds$unit[i] != this.unit ){
        stop(paste("Threshold unit doesn't match data (",analytes[i],")."))
        } # // end if()
      # Remove data outside of specified thresholds
      outside.idx <- which( df3$Analyte==analytes[i] &
                            (df3$Result_Value<thresholds$low[i] | df3$Result_Value>thresholds$high[i]) )
      if( length(outside.idx)>0 ){
        df3 <- df3[-outside.idx,]
      }  # // end if()
      # Add summary entry 
      threshold.screening$n.removals[i] <- length(outside.idx)
    }  # // end i loop
    # Write summary to file
    write.csv(threshold.screening,"threshold.results.csv",row.names=FALSE)
  
# Export clean data
    write.csv( df3, "CHNEP-WQ_clean-data_2011-2020.csv", row.names=FALSE )
