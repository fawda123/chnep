show_dat_ts <- function( dat,
                         station.lab,
                         ylab = paste0( unique(dat$param), " (", unique(dat$unit), ")" ),
                         xlim = c(2011,2021),
                         font.family = 'serif'  # font face for plot (serif=times new roman)
){
  
  # Check dates
  if( min(dat$yr)<min(xlim) | max(dat$yr)>max(xlim) ){
    warning("Input data contain dates outside the specified xlim.")
  }
  
  # Generate time series plot
  par(family=font.family)
  # Compute axis parameters
  xmin <- min( xlim ) # min( mod$var.summary$cont_year )
  xmax <- max( xlim ) # max( mod$var.summary$cont_year )
  ymin <- max( 0, min( dat$value ))
  ymax <- max( dat$value )*1.1
  # Plot data   
  plot( x = dat$cont_year, y = dat$value,
        pch = 16, cex = 1, col = rgb(0,0,0.3,0.4),
        xlim = c(xmin,xmax), ylim = c(ymin,ymax), las=1,
        ylab = '', xlab = '', cex.lab = 1.4, cex.axis = 1.4 )
  # Add plot titles
  mtext( paste("Observed",this.analyte,"at",station.lab),
         side=3, line=1, adj=0 )
  mtext( paste0(dat$mo[1]," ",dat$yr[1]," to ",
                dat$mo[nrow(dat)]," ",dat$yr[nrow(dat)],
                " (n=",nrow(dat),")"),
         side=3, line=0, adj=0 )
  mtext( ylab, side = 2, line = 3.75 )
  # Draw gridlines
  abline( h = axTicks(2), col = rgb(0,0,0,0.1) )
  abline( v = floor(xmin):ceiling(xmax), col = rgb(0,0,0,0.1) )
  # Plot GAM curve
  lines( x = dat$cont_year, y = dat$value,
         lwd = 4, col = rgb(0,0,0.5,0.05) )
  # Add legend
  legend('topleft', legend = c("Data"),
         cex = 1.2, inset = 0.015,
         bg = rgb(0.98,0.99,0.99,0.7), box.col = rgb(0.3,0.6,0.6,0.1),
         pch = 16, pt.cex = 1, 
         col = rgb(0,0,0.3,0.4) )
  
}  # // end show_dat_ts()