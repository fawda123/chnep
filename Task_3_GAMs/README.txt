8/12/2022

These scripts perform trend analysis based on the GAMs/mixed models framework proposed by Beck et al. (2022) with functions from the accompanying package `wqtrends`. If running the scripts, be sure to update any setwd() calls to your local directories.
* "01 GAM trends v3.R" estimates and visualizes trends for each station/analyte or stratum/analyte pair. First, we fit a GAM using mgcv::gam(), with the "tp" smooth for continuous year (long-term trend), the "cc" smooth for the day of year (seasonality), and the "ti" interaction smooth for the first two smooths. This specification differs from the original Beck et al. implementation that used a single smooth for continuous year with a large number of knots. In addition, we set the gam() arguments select=TRUE and method="REML" in contrast to the original Beck et al. implementation (see the user-defined function anlz_gam_SDI() for the gam() call). Following the GAM fit, the mixmeta::mixmeta() fits a mixed model to estimate trends in the annual mean values over 10-year and 5-year periods (2011-2020 and 2016-2020).
* "02 medians.R" estimates median values for each station/analyte or stratum/analyte pair.
* "03 categorize trends.R" categorizes 10-year and 5-year significant trends as large or small, following the USF Water Atlas team's criteria: A trends is "large" if the slope exceeds 10% of the median value over the same period.
* "04 trend maps.R" maps trends for each analyte and trend analysis period (5-year or 10-year) using ggmap.

Output data and visualizations are stored elsewhere (not on github).