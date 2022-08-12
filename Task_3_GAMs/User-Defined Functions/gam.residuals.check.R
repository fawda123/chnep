gam.residuals.check <- function( mod, plot.hist=FALSE, n=1e6 ){
  
  set.seed(931048)
  normdat <- rnorm(n,mean=mean(mod$residuals),sd=sd(mod$residuals))
  ks <- ks.test( x = normdat,  # Kolmogorov-Smirnov test
                 y = mod$residuals )
  sw <- shapiro.test( x= mod$residuals )  # Shapiro-Wilk test
  
  if(plot.hist){
    h1 <- hist( normdat, breaks = 100, plot = FALSE )$density
    h2 <- hist( mod$residuals, breaks = 100, plot = FALSE )$density
    hist( normdat,
          border = rgb(1,1,1,1), breaks = 100, freq = FALSE,
          main = 'Residuals histogram', xlab = '',
          xlim = range(c(normdat,mod$residuals)),
          ylim = c(0,max(h1,h2)) )
    hist( mod$residuals, col = rgb(1,0,0,0.15), border = rgb(1,1,1,1),
          breaks = 100, freq = FALSE, add = TRUE )
    lines( density(normdat), lwd = 2, col = rgb(0,0,0,0.8) )  # gaussian density
    lines( density(mod$residuals), lwd = 2, col = rgb(1,0.2,0.2,0.6) )  # data density
    legend( 'topleft', legend=c(paste0('rnorm(',n,')'),'Model residuals'),
            text.col=c(rgb(0,0,0,0.5),rgb(1,0,0,0.5)), text.font = 2, bty='n' )
    text( x = par('usr')[2], y = par('usr')[4]*0.95, pos = 2,
          labels = paste0("Kolmogorov-Smirnov P = ",round(ks$p.value,3),
                          "\nShapiro-Wilk P = ",round(sw$p.value,3)) )
  }  # end histogram block
  
  return( list( ks=ks, sw=sw, ks.p=ks$p.value, sw.p=sw$p.value ) )
  
}  # end gam.residuals.check()