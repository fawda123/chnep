rm(list=ls(all=TRUE)) 

# Specify directories
dir.udf <- ('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\User-Defined Functions')
dir.dat <- ('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\Data')
dir.out <- ('C:\\Users\\miles.medina\\Documents\\UF Postdoc\\CHNEP\\Task 2\\R files\\Output')

# Load libraries
if(!require(tidyr)) { install.packages('tidyr') }; library(tidyr)

# Load user-defined functions
setwd( dir.udf )
source('udf_annual.means.R')
source('udf_plot.means.R')
source('udf_plot.time.R')

# Load clean Water Atlas data
setwd( dir.dat )
dat <- read.csv('CHNEP-WQ_clean-data_2011-2020.csv' )

# Format dataframe
df1 <- dat[,c('Stratum','Analyte','Date','Result_Value','Result_Unit')]
colnames( df1 ) <- c('Station','Analyte','Date','Value','Unit')
df1$Date <- df1$Date %>% as.Date('%Y-%m-%d')

# Compute means
dat.means <- annual.means( df1, MIN.OBS=0 )

# Generate annual means plots
setwd( dir.out )
plot.means( dat.means, write.png=TRUE )

# Write dat.means to file
setwd( dir.out )
write.csv( dat.means, "annual.means.csv", row.names=FALSE )
save( dat.means, file="annual.means.RData" )

# Generate time plots
setwd( dir.out )
plot.time( df1, write.png=TRUE )
