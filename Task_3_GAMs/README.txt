8/12/2022

These scripts perform trend analysis based on the GAMs/mixed models framework proposed by Beck et al. (2022) with functions from the accompanying package `wqtrends`.
* "01 GAM trends v3.R" estimates and visualizes trends for each station/analyte or stratum/analyte pair using mgcv::gam(), with the "tp" smooth for continuous year (long-term trend), the "cc" smooth for the day of year (seasonality), and the "ti" interaction smooth for the first two smooths. In contrast to the original implementation by Beck et al., we specify select=TRUE and method="REML". Then, the mixmeta::mixmeta() fits a mixed model estimate trends in the annual mean values over 10-year and 5-year periods (2011-2020 and 2016-2020).
* "02 medians.R" estimates median values for each station/analyte or stratum/analyte pair.
* "03 categorize trends.R" categorizes 10-year and 5-year significant trends as large or small, following the USF Water Atlas team's criteria: A trends is "large" if the slope exceeds 10% of the median value over the same period.
* "04 trend maps.R" maps trends for each analyte and trend analysis period (5-year or 10-year) using ggmap.