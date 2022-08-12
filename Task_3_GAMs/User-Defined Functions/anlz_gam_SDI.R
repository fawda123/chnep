# This function is an alternative to wqtrends::anlz_gam()'s original implementation.
# The GAM specification includes a tp spline for the decimal year (long-term trend),
# a cc spline for the day of year (seasonality), and a year-season interaction,
# with select=TRUE. The default smoothing parameter estimation method (gam.method)
# is "REML", in contrast to the original implementation's use of the mgcv default,
# "GCV.Cp". The function first tries to fit a GAM with specified k values 
# (k.doy and k.cont_year). If it fails, the k.cont_year value is reduced by one
# until the GAM is successfully fit.

anlz_gam_SDI <- function ( moddat, k.doy = 12, k.cont_year = 10,
                           trans = 'auto', gam.method='REML',
                           logvars = c( 'TN','TKN','NOx','TP','Color',
                                        'Chl-a','Fecal','Turbidity','Sp Cond',
                                        'PO4','NH4','TSS','BOD' ) ) {
  # Throw error if >1 analyte or station
    if (length(unique(moddat$param)) > 1) 
        stop("More than one parameter found in input data")
    if (length(unique(moddat$station)) > 1) 
        stop("More than one station found in input data")
  
  # Transform input data
    if( trans == 'ident'){
      this.trans <- trans
    } else if( trans == 'log10' ){
      this.trans <- trans
    } else if( trans == 'auto' ){
      this.trans <- if( unique(moddat$param) %in% logvars ){
                           'log10'
                         } else {
                           'ident'
                         }
    } else {
      stop("Trans must be 'ident', 'log10', or 'auto'")
    }  # // end if()
    moddat <- anlz_trans(moddat, trans = this.trans )
    
  # Fit GAM
    # Try to fit GAM with specified k's
    out <- try( gam( value ~ s( cont_year, k = k.cont_year ) +
                     s( doy, bs='cc', k = k.doy ) +
                     ti( cont_year, doy, bs = c('tp','cc'), k = c(k.cont_year,k.doy) ),
                     data = moddat,
                     select = TRUE, method = gam.method,
                     knots = list(doy = c(0.5,366.5)),
                     control = list( keepData = TRUE ) ),
                silent = TRUE )
    # If unsuccessful, reduce k.cont_year by one until it works
    while( inherits(out, "try-error") ) {
        k.cont_year <- k.cont_year - 1
        if( k.cont_year==0 ){ break }  # Prevent k from going negative
        out <- try( gam( value ~ s( cont_year, k = k.cont_year ) +
                         s( doy, bs='cc', k = k.doy ) +
                         ti( cont_year, doy, bs = c('tp','cc'), k = c(k.cont_year,k.doy) ),
                         data = moddat,
                         select = TRUE, method = gam.method,
                         knots = list(doy = c(0.5,366.5)),
                         control = list( keepData = TRUE ) ),
                    silent = TRUE )
    }  # // end while()
    
  # Generate output
    if( inherits(out, "try-error") ){  # if GAM was not fit...
      out <- list()
      out$trans <- this.trans
      out$success <- FALSE
    } else {  # if GAM was successfully fit..
      out$success <- TRUE
      out$trans <- this.trans
      out$knots <- c( doy = k.doy, cont_year = k.cont_year )
      # Check knots are sufficient (TRUE if pval>0.05)
      set.seed(98403)
      k.chk <- k.check( out )
      out$k.suff <- list( cont_year = k.chk[1,4]>0.05,
                          doy = k.chk[2,4]>0.05,
                          ti = k.chk[3,4]>0.05 )
      # Check residuals using Kolmogorov-Smirnov and Shapiro-Wilk
      r.chk <- gam.residuals.check( out )
      out$ks <- r.chk$ks
      out$sw <- r.chk$sw
    }  # // end if()
    
  return(out)
      
}  # // end anlz_gam_SDI()

