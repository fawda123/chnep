show_gam_ts <- function( mod,
                         station.lab,
                         ylab = paste0( unique(mod$data$param), " (", unique(mod$data$unit), ")" ),
                         xlim = c(2011,2021),
                         font.family = 'serif'  # font face for plot (serif=times new roman)
                         ){
  
  # Check dates
  if( min(mod$data$yr)<min(xlim) | max(mod$data$yr)>max(xlim) ){
    warning("Input data contain dates outside the specified xlim.")
  }
  
  # Model prediction over data POR; pred.seq is a temporal sequence with 50 obs per year
  pred.seq <- c( seq( min( mod$var.summary$cont_year),
                      max( mod$var.summary$cont_year), 0.02 ),
                 max( mod$var.summary$cont_year ) )
  pred <- predict.gam( mod,
                       newdata = data.frame( cont_year = pred.seq,
                                             doy = yday(date_decimal(pred.seq)) ),
                       se.fit=TRUE )
  
  # Transform log10 values back to measurement units
  if( mod$trans=='log10' ){
    mod.y <- 10^mod$y
    pred.fit <- 10^(pred$fit)
  } else {
    mod.y <- mod$y
    pred.fit <- pred$fit
  }
  
  # Generate time series plot
    par(family=font.family)
    # Compute axis parameters
    xmin <- min( xlim ) # min( mod$var.summary$cont_year )
    xmax <- max( xlim ) # max( mod$var.summary$cont_year )
    ymin <- max( 0, min( pred.fit, mod.y ))
    ymax <- max( pred.fit, mod.y )*1.1
    # Plot data   
    plot( x = mod$data$cont_year, y = mod.y,
          pch = 16, cex = 0.8, col = rgb(0,0,0,0.4),
          xlim = c(xmin,xmax), ylim = c(ymin,ymax), las=1,
          ylab = '', xlab = '', cex.lab = 1.4, cex.axis = 1.4 )
    # Add plot titles
    mtext( paste("GAM for",this.analyte,"at",station.lab),
           side=3, line=1, adj=0 )
    mtext( paste0(mod$data$mo[1]," ",mod$data$yr[1]," to ",
                 mod$data$mo[nrow(mod$data)]," ",mod$data$yr[nrow(mod$data)],
                 " (n=",nrow(mod$data),")"),
           side=3, line=0, adj=0 )
    mtext( ylab, side = 2, line = 3.75 )
    # Draw gridlines
    abline( h = axTicks(2), col = rgb(0,0,0,0.2) )
    abline( v = floor(xmin):ceiling(xmax), col = rgb(0,0,0,0.2) )
    # Plot GAM curve
    lines( pred.seq, pred.fit, lwd = 4, col = rgb(0.21,0.47,1,0.7) )
    # Add legend
    legend('topleft', legend = c("GAM","Data"),
           cex = 1.2, inset = 0.015,
           bg = rgb(0.98,0.99,0.99,0.7), box.col = rgb(0.3,0.6,0.6,0.1),
           pch = c(NA, 16), pt.cex = c(NA, 0.8), 
           lwd = c(3, NA), seg.len = 0.75,
           col = c(rgb(0.21,0.47,1,0.7),rgb(0,0,0,0.4)) )
  
}  # // end show_gam_ts()