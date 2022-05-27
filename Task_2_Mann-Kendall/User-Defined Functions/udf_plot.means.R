# Define function that plots annual means for all stations and analytes.
# Input dataframe 'MEANS' is output from annual.means() function.
# The function generates a separate graphics frame (or PNG file) for each station.

plot.means <- function( MEANS, par.mfrow=c(6,3), write.png=FALSE ){
  
  # Load libraries
  if(!require(tidyr)) { install.packages('tidyr') }; library(tidyr)
  
  # Get station and analyte names
  stations <- unique( MEANS$Station ) %>% sort()
  analytes <- unique( MEANS$Analyte ) %>% sort()
  
  # Loop over stations and analytes to generate plots
  for( i in 1:length(stations) ){
    # Subset data for i'th station
    this.station <- stations[i]
    this.dat.i <- MEANS[ which( MEANS$Station==this.station ), ]
    # Each station's plots appear in a separate graphics frame or png file
    if( write.png ){
      filename <- paste0("annual.means__",gsub(' : ','__',this.station),".png")
      png( filename, width=8, height=12, units='in', res=600 )
    }
    par(mfrow=par.mfrow)
    # Loop over analytes to generate plots
    for( j in 1:length(analytes) ){
      # Subset data for j'th analyte
      this.analyte <- analytes[j]
      this.dat.ij <- this.dat.i[ which( this.dat.i$Analyte==this.analyte ), ]
      if( nrow(this.dat.ij)==0 ){ # If no data, generate blank plot
        this.values <- rep( NA, ncol(this.dat.ij)-4 )
        this.plot <- barplot( height = this.values,
                              names = colnames(this.dat.ij)[5:ncol(this.dat.ij)],
                              ylim = c(0,1), yaxt='n', axis.lty = 1,
                              main = paste0(this.analyte,'\n',gsub(' : ','\n',this.station)),
                              cex.main = 0.9 )
        text( x=mean(this.plot), y=0.5, labels='No data' )
      } else {  # If there are data, generate bar plot of annual means
        this.values <- this.dat.ij[ 5:ncol(this.dat.ij) ]
        this.unit <- unique( this.dat.ij$Unit )
        # Generate bar plot of annual means
        this.plot <- barplot( height = as.matrix(this.values),
                              names = colnames(this.dat.ij)[5:ncol(this.dat.ij)],
                              main = paste0(this.analyte,'\n',gsub(' : ','\n',this.station)),
                              cex.main = 0.9,
                              ylab = this.unit, ylim = c(0, max(this.values,na.rm=TRUE)*1.1),
                              col = rgb(0,0.3,0.9,0.7), border = NA, las = 1, axis.lty = 1 )
        # Draw horizontal lines
        abline(h=0)
        yticks <- par()$yaxp  
        abline( h = seq(yticks[1],yticks[2],length.out=yticks[3]+1), col=rgb(0,0,0,0.2) )
        # Add 'NA' text to label missing annual means on plot
        text( x = this.plot[ which(is.na(this.values)) ],
              y = 0, labels = 'NA', pos = 3, cex=0.75 )
      }  # // end if(this.dat.ij)
    }  # // end j loop (analytes)
    if( write.png ){ dev.off() }
  }  # // end i loop (stations)
  
}  # // end plot.means()