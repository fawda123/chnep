rm(list=ls(all=TRUE)) 

# Load libraries
if(!require(tidyr)) { install.packages('tidyr') }; library(tidyr)
if(!require(plyr)) { install.packages('plyr') }; library(plyr)
if(!require(dplyr)) { install.packages('dplyr') }; library(dplyr)

# Set directories
dir.udf <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\User-Defined Functions')
dir.dat <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\Data')
dir.out <- c('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\Output')

# Load data 
setwd( dir.out )
load("MK.trend.10y.results.RData")  # trend results in results.df dataframe
setwd( dir.dat )
USF.results <- read.csv('Trend Analysis - CHNEP - 2020 - Results.csv')


# Format USF's trend analysis results
  # Subset 10-year trend results for 2011-2020 period
  USF.results <- USF.results[ which( USF.results$AnalysisTimeSpan=='10 year' ), ]
  USF.results <- USF.results[ which( USF.results$MinYear==2011 ), ]
  USF.results <- USF.results[ which( USF.results$MaxYear==2020 ), ]
  # Rename analytes ('Parameters') to match analyte names in CCS results
  analytes.old <- USF.results$Parameter %>% unique() %>% sort()
  analytes.new <- c( 'BOD', 'Chl-a', 'Color', 'Sp Cond', 'DO Conc', 'DO Sat', 'Fecal', 'NH4', 'NOx', 'PO4',
                     'pH', 'Temp', 'TKN', 'TN', 'TP', 'TSS', 'Turbidity' )
  USF.results$Analyte <- mapvalues( USF.results$Parameter, analytes.old, analytes.new ) 
    # QC check: Confirm analytes are matched correctly
    for( i in 1:length(analytes.old) ){
      cat( paste( analytes.old[i], "::", unique(USF.results$Analyte[which(USF.results$Parameter==analytes.old[i])]),"\n" ) )
    }  # // end i loop
  # Subset columns
  USF.results <- USF.results[ , which( colnames(USF.results) %in% c('StationID','Analyte','Trend') ) ]
  colnames( USF.results )[ which( colnames(USF.results)=='StationID' ) ] <- 'Station'
  # Coerce column classes
  USF.results$Station <- USF.results$Station %>% as.factor()
  USF.results$Trend <- USF.results$Trend %>% as.numeric()
  USF.results$Analyte <- USF.results$Analyte %>% as.factor()
  # Create new columns for MK trend (Trend negative, zero, positive) and for large trends (Trend of -2 or 2)
    # MK.trend (Positive Trends get labeled 1; negative Trends get labeled -1; non-sig Trends get labeled 0; others NA)
    USF.results$USF.MK.trend <- NA
    USF.results$USF.MK.trend[ which( USF.results$Trend>0 ) ] <- 1
    USF.results$USF.MK.trend[ which( USF.results$Trend==0 ) ] <- 0
    USF.results$USF.MK.trend[ which( USF.results$Trend<0 ) ] <- -1
    # trend.large (Trends of -1 or 1 get labeled FALSE; Trends of -2 or 2 get labeled TRUE; others NA)
    USF.results$USF.trend.large <- NA
    USF.results$USF.trend.large[ which( abs(USF.results$Trend)==1 ) ] <- FALSE
    USF.results$USF.trend.large[ which( abs(USF.results$Trend)==2 ) ] <- TRUE
  # Remove USF's original Trend column
    USF.results <- USF.results[, -which(colnames(USF.results)=='Trend') ]
    

    
# Compare USF and CCS trend results
    # Join CCS results (results.df) and USF results (USF.results)
    all.results <- full_join( x = results.df, y = USF.results, by = c('Station','Analyte') )
    # In a new column, label comparison results
    all.results$Compare <- NA
    all.results$Compare[ which( all.results$MK.trend != all.results$USF.MK.trend ) ] <- "MK.trend"
    all.results$Compare[ which( all.results$MK.trend == all.results$USF.MK.trend &
                                all.results$trend.large != all.results$USF.trend.large ) ] <- "trend.large"
    all.results$Compare[ which( ( all.results$MK.trend == all.results$USF.MK.trend |
                                 (is.na(all.results$MK.trend) & is.na(all.results$USF.MK.trend)) ) &
                                ( all.results$trend.large == all.results$USF.trend.large | 
                                 (is.na(all.results$trend.large) & is.na(all.results$USF.trend.large)) ) ) ] <- 'Match'
    all.results$Compare[ which( !is.na(all.results$MK.trend) & is.na(all.results$USF.MK.trend) ) ] <- 'CCSrun.USFnotrun'
    all.results$Compare[ which( is.na(all.results$MK.trend) & !is.na(all.results$USF.MK.trend) ) ] <- 'CCSnotrun.USFrun'
    # Summary of results
    table( all.results$Compare, useNA='always' )
    
    
# Investigate disagreements in trend direction
    # USF trend up vs. CCS no trend
    all.results[ which( all.results$USF.MK.trend==1 &
                        all.results$MK.trend==0 ), ]
    # USF trend up vs. CCS trend down  **** 
      # Notes: The USF data on Station 212 Turbidity look somewhat different (less data), see:
      # https://chnep.wateratlas.usf.edu/water-quality-trends/station-details?param=turb_ntu&ds=WIN%2FSTORET+-+CHNEPMP&stn=212&por=0
    all.results[ which( all.results$USF.MK.trend==1 &
                        all.results$MK.trend==-1 ), ]
    # USF trend down vs. CCS no trend
    all.results[ which( all.results$USF.MK.trend==-1 &
                        all.results$MK.trend==0 ), ]
    # USF trend down vs. CCS trend up  *****
      # Notes: The USF data on Station 268 TKN look completely different (much less data), see:
     # https://chnep.wateratlas.usf.edu/water-quality-trends/station-details?param=tkn_ugl&ds=WIN%2FSTORET+-+CHNEPMP&stn=268&por=0
    all.results[ which( all.results$USF.MK.trend==-1 &
                        all.results$MK.trend==1 ), ]
    # USF no trend vs. CCS trend up
    all.results[ which( all.results$USF.MK.trend==0 &
                        all.results$MK.trend==1 ), ]
    # USF no trend vs. CCS trend down
    all.results[ which( all.results$USF.MK.trend==0 &
                        all.results$MK.trend==-1 ), ]
    # USF ran analysis vs. CCS did not run
      # Notes: Station 74C data have almost 3yr gap)
      # USF does not exclude data w/ large gaps?
    all.results[ which( !is.na(all.results$USF.MK.trend) &
                        is.na(all.results$MK.trend) ), ]
    # USF did not run vs. CCS ran analysis
      # Notes: Station 105 data are less than 10 years, start in 2014)
    all.results[ which( is.na(all.results$USF.MK.trend) &
                        !is.na(all.results$MK.trend) ), ]

    