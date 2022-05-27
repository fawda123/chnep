plot.time <- function( DF, DATES=range(DF$Date), par.mfrow=c(6,3), write.png=FALSE ){
  
  # Load libraries
  if(!require(tidyr)) { install.packages('tidyr') }; library(tidyr)
  
  # Get station and analyte names
  stations <- unique( DF$Station ) %>% sort()
  analytes <- unique( DF$Analyte ) %>% sort()
  
  # Loop over stations and analytes to generate plots
  for( i in 1:length(stations) ){
    # Subset data for i'th station
    this.station <- stations[i]
    this.dat.i <- DF[ which( DF$Station==this.station ), ]
    # Each station's plots appear in a separate graphics frame or png file
    if( write.png ){
      filename <- paste0("time.plot__",gsub(' : ','__',this.station),".png")
      png( filename, width=8, height=12, units='in', res=600 )
    }
    par(mfrow=par.mfrow)
    # Loop over analytes to generate plots
    for( j in 1:length(analytes) ){
      # Subset data for j'th analyte
      this.analyte <- analytes[j]
      this.dat.ij <- this.dat.i[ which( this.dat.i$Analyte==this.analyte ), ]
      this.dat.ij <- this.dat.ij[ order( this.dat.ij$Date ), ]
      if( nrow(this.dat.ij)==0 ){ # If no data, generate blank plot
        this.plot <- plot( x = DATES[1], y = 0, col=rgb(0,0,0,0), bty='L',
                           xlim = DATES, ylim = c(0,1), yaxt='n',
                           main = paste0(this.analyte,'\n',gsub(' : ','\n',this.station)),
                           cex.main = 0.9,
                           xlab = '', ylab = '' )
        text( x=mean(DATES), y=0.5, labels='No data' )
      } else {  # If there are data, generate bar plot of annual means
        this.unit <- unique( this.dat.ij$Unit )
        this.plot <- plot( Value ~ Date, data = this.dat.ij, bty = 'L',
                           col = rgb(0,0.3,0.9,0.7), pch = 16, cex = 0.6,
                           xlim = DATES, ylim = c(0,max(this.dat.ij$Value)*1.1),
                           main = paste0(this.analyte,'\n',gsub(' : ','\n',this.station)),
                           cex.main = 0.9,
                           xlab = '', ylab = this.unit, las = 1 )
        lines( Value ~ Date, data = this.dat.ij, col = rgb(0,0.3,0.9,0.5) )
        # Draw vertical lines
        abline( v = seq.Date(DATES[1],DATES[2]+1,by='year'), col=rgb(0,0,0,0.2) )
        # Draw horizontal lines
        yticks <- par()$yaxp  
        abline( h = seq(yticks[1],yticks[2],length.out=yticks[3]+1), col=rgb(0,0,0,0.2) )
      }  # // end if(this.dat.ij)
    }  # // end j loop (analytes)
    if( write.png ){ dev.off() }
  }  # // end i loop (stations)
}  # // end plot.time()