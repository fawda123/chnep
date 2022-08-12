show_mixmod <- function ( mod,
                          metfun = mean,
                          doystr = 1, doyend = 365,
                          yrstr = 2011, yrend = 2020,
                          xlims = c(2011,2020),
                          ylab = paste0( unique(mod$data$param), " (", unique(mod$data$unit), ")" ),
                          nsim = 10000,
                          useave = FALSE,
                          font.family = 'serif',
                          ...){

  chk <- identical(deparse(metfun), deparse(mean))
  if (!chk & useave) 
    stop("Specify metfun = mean if useave = T")
  if (useave) 
    metseason <- anlz_avgseason(mod, doystr = doystr, doyend = doyend)
  if (!useave) 
    metseason <- anlz_metseason(mod, metfun, doystr = doystr, 
                                doyend = doyend, nsim = nsim, ... )
  trans <- mod$trans
  dts <- as.Date(c(doystr, doyend), origin = as.Date("2000-12-31"))
  strt <- paste(lubridate::month(dts[1], label = T, abbr = T), 
                lubridate::day(dts[1]))
  ends <- paste(lubridate::month(dts[2], label = T, abbr = T), 
                lubridate::day(dts[2]))
  func <- as.character(substitute(metfun))
  toplo1 <- metseason %>% as.data.frame()
  
  if (!any(is.null(yrstr) | is.null(yrend))) {
    mixmet <- anlz_mixmeta(metseason, yrstr = yrstr, yrend = yrend)
    toplo2 <- data.frame(yr = seq(yrstr, yrend, length = 50)) %>% 
      dplyr::mutate(met = predict(mixmet, newdata = data.frame(yr = yr)), 
                    se = predict(mixmet, newdata = data.frame(yr = yr), 
                                 se = T)[, 2],
                    bt_lwr = met - 1.96 * se,
                    bt_upr = met + 1.96 * se,
                    bt_met = met )
    pval <- coefficients(summary(mixmet)) %>% data.frame %>% .[2, 4]
    pval.txt <- pval %>% anlz_pval()
    if (mod$trans == "log10") {
      dispersion <- summary(mod)$dispersion
      toplo2 <- data.frame(yr = seq(yrstr, yrend, length = 50)) %>% 
        dplyr::mutate(met = predict(mixmet, newdata = data.frame(yr = yr)), 
                      se = predict(mixmet, newdata = data.frame(yr = yr), 
                                   se = T)[, 2],
                      bt_lwr = 10^((met - 1.96 * se) + log(10) * dispersion/2),
                      bt_upr = 10^((met + 1.96 * se) + log(10) * dispersion/2),
                      bt_met = 10^(met + log(10) * dispersion/2)) 
      slope <- lm(bt_met ~ yr, toplo2) %>% summary %>% coefficients %>% .[2, 1]
      slope <- round(slope, 2)
      logslope <- summary(mixmet)$coefficients[2, c(1,5,6)]
      logslope <- round(logslope, 2)
      logslope.txt <- paste0(logslope[1], " [", logslope[2], 
                         ", ", logslope[3], "]")
      subtitle <- paste0("     ", yrstr, " \u2013 ", yrend, " Trend: ",
                         "Approx. slope ", slope, ", ",
                         " log-slope ", logslope.txt, ", ", pval.txt)
    }
    if (mod$trans == "ident") {
      slope <- summary(mixmet)$coefficients[2, c(1,5,6)]
      slope <- round(slope, 2)
      slope.txt <- paste0(slope[1], " [", slope[2], ", ", 
                      slope[3], "]")
      logslope <- NA
      subtitle <- paste0("     ", yrstr, " \u2013 ", yrend, " Trend: ",
                         "Approx. slope ", slope.txt, ", ", pval.txt)
    }
    
    # Generate plot
    par(mar=c(3,5,2,1))
      # Compute axis parameters
      ybuf <- (toplo1$bt_upr-toplo1$bt_met) %>% mean()  # y-axis white space
      yrng <- range( toplo1$bt_lwr, toplo1$bt_upr, toplo2$bt_lwr, toplo2$bt_upr )
      ymin <- min( yrng ) - ybuf
      ymax <- max( yrng ) + ybuf
      xmin <- min(xlims)
      xmax <- max(xlims)+1
      # Plot metric estimates (e.g. means)
      plot( x = toplo1$yr, y = toplo1$bt_met,
            pch = 16, col = rgb(0,0.6,0.8,1), cex = 0.8,
            ylim = c(ymin,ymax), ylab = '', xlim=c(xmin,xmax), xlab = '',
            las = 1, family = font.family, cex.lab = 1.4, cex.axis = 1.4 )
      mtext( ylab, side = 2, line = 3.75 )
      # Draw metrics' error bars
      arrows( x0 = toplo1$yr,
              y0 = toplo1$bt_lwr,
              y1 = toplo1$bt_upr,
              code = 3, angle = 90, length = 0.15,
              col = rgb(0,0.6,0.8,1), lwd = 2 )
      # Draw gridlines
      abline( h = axTicks(2), col = rgb(0,0,0,0.2) )
      # Add trend line and bounds
      if( pval < 0.05 ){
        col.line <- rgb(1,0.45,0.50,0.6)
        col.poly <- rgb(1,0.45,0.50,0.2)
        col.lgnd <- rgb(1,0.45,0.50,0.8)
      } else {
        col.line <- rgb(0,0,0.4,0.3)
        col.poly <- rgb(0,0,0.3,0.1)
        col.lgnd <- rgb(0,0,0.4,0.4)
      }
      polygon( x = c( toplo2$yr, rev(toplo2$yr) ),
               y = c( toplo2$bt_upr, rev(toplo2$bt_lwr) ),
               col = col.poly, border = NA)
      lines( x = toplo2$yr, y = toplo2$bt_met,
             lwd = 2.5, col = col.line )
      # Add hacky plot title legend
      mtext("I",side=3,line=0.9,adj=0,family='mono',  # blue error bar
            col=rgb(0,0.6,0.8,1), cex=1.5)
      mtext( paste0("     Est. ", func, " with 95% CI (", # error bar text
                    strt, " \u2013 ", ends, ")" ),
             side=3, line=1, adj=0, family=font.family )
      mtext("\u2013",side=3,line=0,adj=0,family='mono',  # trend line
            col=col.lgnd, cex=1.5,font=2)
      mtext( subtitle,    # trend line text
             side=3, line=0, adj=0, family=font.family )
  }
  
  out <- list( metfun = func,
               slope = slope[1],
               logslope = logslope[1],
               slope.pval = pval,
               toplo1 = toplo1,
               toplo2 = toplo2 )
  return( out )
  
}  # // end show_mixmod()