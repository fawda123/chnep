anlz_pval <- function (x){
  sig_cats <- c("P<0.001", "P<0.01", "P<0.05", 
                "P>0.05 (ns)")
  sig_vals <- c(-Inf, 0.001, 0.01, 0.05, Inf)
  out <- cut(x, breaks = sig_vals, labels = sig_cats, right = FALSE)
  out <- as.character(out)
  return(out)
}