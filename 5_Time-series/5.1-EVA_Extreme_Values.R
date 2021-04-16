#'---
#'title: "Statistics Course - UWI"
#'author: "AlbyDR"
#'date: "`r format(Sys.Date())`"
#'always_allow_html: true
#'output: github_document
#'---
#'
#'## Example 5.2:EVA - Extreme Values Events 
#'
packages_list5.2 <- c("extRemes", "tidyverse", "lubridate")
#'
#' new.packages <- packages_list5.2[!(packages_list5.2 %in% installed.packages()[,"Package"])]
#' if(length(new.packages)) install.packages(new.packages)
#' #'
#' update.packages <- packages_list5.2[(packages_list5.2 %in% old.packages()[,"Package"])]
#' if(length(update.packages)) install.packages(update.packages)
#'
invisible(lapply(packages_list5.2, library, character.only = T, quietly = TRUE, warn.conflicts = F))
#'
DWD_temperature <- read_rds("DWD_temperature.rds")
DWD_precipitation <- read_rds("DWD_precipitation.rds")
#'
#'### Precipitation
#'
#' **Block Maxima Approach**
#' 
bmPrec <- blockmaxxer(filter(DWD_precipitation[,1:2], !is.na(precip_mm)), which="precip_mm", 
                      blocks = year(filter(DWD_precipitation[,1:2], !is.na(precip_mm))$timestamp))
#'
summary(bmPrec)
#'
plot(year(bmPrec$timestamp), bmPrec$precip_mm, col = "white", pch = 21, bg = "darkred", cex=1.5)
#'
GEV_mm <- fevd(bmPrec$precip_mm, type='GEV')
GEV_mm
#' 
ci(GEV_mm, alpha=0.05, type="parameter")
#' 
plot(GEV_mm)
plot(GEV_mm, "trace")
#' 
threshrange.plot(bmPrec$precip_mm, r = c(3, 10), type = "GP", nint = 20)
#' 
#' **Gumbel**
Gumbel_mm <- fevd(bmPrec$precip_mm, type="Gumbel", units="mm")
Gumbel_mm
#'
ci(Gumbel_mm, alpha=0.05, type="parameter")
#'
plot(Gumbel_mm)
#plot(Gumbel_temp, "trace")
#'
return.level(Gumbel_mm, return.period=c(2,5,10,20,30,50,100), do.ci=TRUE)
#'
lr.test(Gumbel_mm, GEV_mm, alpha = 0.05)
#'
#' **Bayesian**
Bayesian_mm <- fevd(bmPrec$precip_mm, method = "Bayesian")
Bayesian_mm
#'
postmode(Bayesian_mm)
#'
plot(Bayesian_mm)
plot(Bayesian_mm, "trace")
#' The result is a distribution of the parameters locations, scale and shape
#'
ci(Bayesian_mm, alpha=0.05, type="parameter")
return.level(Bayesian_mm, return.period=c(2,5,10,20,30,50,100), do.ci=TRUE)
#'
pextRemes(Bayesian_mm, c(32, 34, 36, 38, 40), lower.tail = FALSE)
#'
mrlplot(bmPrec$precip_mm)
#'
# PEACKS OVER THRESHOLD APPROACH
# Data set of daily rainfall data
#'
bm_mm <- blockmaxxer(DWD_precipitation, which = "precip_mm", blocks=NULL, blen = 24, span = 24*365)
bm_mm <- filter(bm_mm, !is.na(precip_mm))
#'
summary(bm_mm)
#'
mrlplot(bm_mm$precip_mm)
threshrange.plot(bm_mm$precip_mm)
#'
#' **Generalized Pareto (GP)**
GP_mm <- fevd(bm_mm$precip_mm, threshold = 3, type = "GP", units = "mm")
GP_mm
#'
plot(GP_mm)
#'
ci(GP_mm, alpha = 0.05, type = "parameter")
ci(GP_mm, alpha = 0.05, type = "return.level")
#'
#'
return.level(GP_mm, return.period = c(10,20,30,50,100), do.ci = TRUE)
#'
#' Number of excesses
count = 0
for (i in c(1:length(bm_mm$precip_mm))){
  if (bm_mm$precip_mm[i] >= 10){
    count = count + 1
  }
}
#'
plot(y=bm_mm$precip_mm, x=bm_mm$timestamp)
points(y=filter(bm_mm, precip_mm >= 20)$precip_mm,
       x=filter(bm_mm, precip_mm >= 20)$timestamp, 
       pch = 16, col = 'red')
#'
pextRemes(GP_mm, c(10, 20, 30, 40), lower.tail = FALSE)
#'
profliker (GP_mm, type ="parameter", which.par = 2,
           main = "Profile Log - Likelihood for ShapeParameter")
#'
#'  100-year return level
ci(GP_mm, method="proflik", xrange=c(18,38), verbose=TRUE)
#'
#' Fit the Poisson Process (PP) model to the daily data from above.
PP_mm <- fevd(bm_mm$precip_mm, threshold = 3,
              type = "PP", units = "mm")
#'
PP_mm
#'
plot(PP_mm)
#'
#' Fit the Exponential model to the daily data from above.
Exp_mm <- fevd(bm_mm$precip_mm, threshold = 3,
              type = "Exponential", units = "mm")
Exp_mm
#'
plot(Exp_mm)
#'
#'
#'
#'### Temperature
#' 
DWD_temperature[,1:2] %>%
  filter(year(timestamp) >= 1900) %>%
  group_by(Year = year(timestamp), doy = yday(timestamp)) %>%
  summarise(Tmax = max(air_temp, na.rm = T)) %>%
  print(n=10) -> Max_tempday
#' 
#' 
bmTempday <- blockmaxxer(Max_tempday, blocks = Max_tempday$doy, which = "Tmax")
summary(bmTempday)
#' 
plot(bmTempday$doy, bmTempday$Tmax, col = "white", pch = 21, bg = "darkred", cex=1.5)
#' 
GEV_temp_day <- fevd(bmTempday$Tmax, type='GEV')
GEV_temp_day
#' 
ci(GEV_temp_day, alpha=0.05, type="parameter")
#' 
plot(GEV_temp_day)
# plot(GEV_temp_day, "trace")
#' 
threshrange.plot(bmTempday$Tmax, r = c(28, 36), type = "GP", nint = 20)
#' 
GP_temp_day <- fevd(x = bmTempday$Tmax, threshold = 33.5, type = "GP", span = 62,
                    units = "deg C", time.units = "5/year")
#' 
ci(GP_temp_day, alpha=0.05, type="parameter")
#' 
plot(GP_temp_day)
#' 
#'  estimates of the auto-tail dependence function(s) (atdf) based on either chi (rho) 
#'  or chibar (rhobar), or both.
atdf(bmTempday$Tmax, 0.90)
#' 
#'  The extremal index is a useful indicator of how much clustering of exceedances 
#'  of a threshold occurs in the limit of the distribution. For independent data, 
#'  theta = 1, (though the converse is does not hold) and if theta < 1, 
#'  then there is some dependency (clustering) in the limit.
extremalindex(bmTempday$Tmax, threshold = 20, method = "runs", run.length = 10, blocks=Max_temp$Year)
#'
#' Decluster data above a given threshold to try to make them independent.
dcTemp_day <- decluster(bmTempday$Tmax, 20, r = 10)
plot(dcTemp_day)
#'
#'
#' The Block Maxima option takes the maxima of one variable over blocks
#' defined by another variable where both are columns of the same data frame 
#' usually, a column containing the years is used for the blocks so that annual maxima are taken
#'
DWD_temperature[,1:2] %>%
  filter(year(timestamp) >= 1900) %>%
  group_by(Year = year(timestamp)) %>%
  summarise(Tmax = max(air_temp, na.rm = T)) -> Max_temp
#'
#' estimates of the auto-tail dependence function(s) (atdf) based on either chi (rho) 
#' or chibar (rhobar), or both.
atdf(Max_temp$Tmax, 0.90)
#'
#' The extremal index is a useful indicator of how much clustering of exceedances 
#' of a threshold occurs in the limit of the distribution. For independent data, 
#' theta = 1, (though the converse is does not hold) and if theta < 1, 
#' then there is some dependency (clustering) in the limit.
extremalindex(Max_temp$Tmax, threshold = 30, method = "runs", run.length = 9, blocks=Max_temp$Year)
#'
#' Decluster data above a given threshold to try to make them independent.
dcTemp_year <- decluster(Max_temp$Tmax, 30, r = 9)
plot(dcTemp_year)
#'
DWD_temperature <- filter(DWD_temperature, year(timestamp) >= 1900)
#'
bmTemp <- blockmaxxer(DWD_temperature[,1:2], blocks = year(DWD_temperature$timestamp), which = "air_temp")
bmTemp <- filter(bmTemp, !is.na(air_temp))
#'
#' estimates of the auto-tail dependence function(s) (atdf) based on either chi (rho) 
#' or chibar (rhobar), or both.
atdf(bmTemp$air_temp, 0.90)
#'
#' The extremal index is a useful indicator of how much clustering of exceedances 
#' of a threshold occurs in the limit of the distribution. For independent data, 
#' theta = 1, (though the converse is does not hold) and if theta < 1, 
#' then there is some dependency (clustering) in the limit.
extremalindex(bmTemp$air_temp, threshold = 20, method = "runs", run.length = 10, blocks=Max_temp$Year)
#'
#' Decluster data above a given threshold to try to make them independent.
dcTemp_year <- decluster(Max_temp$Tmax, 35, r = 9)
plot(dcTemp_year)
#'
summary(bmTemp)
summary(Max_temp)
#'
plot(Max_temp$Year, Max_temp$Tmax, xlab = "Year",
     ylab = "precip_mm", cex = 1.25,
     cex.lab = 1.25, col = "darkblue", bg = "lightblue", pch = 21)
#'
qqnorm(Max_temp$Tmax, cex.lab = 1.25)
#'
plot(year(bmTemp$timestamp), bmTemp$air_temp, xlab = "Year",
     ylab = "precip_mm", cex = 1.25,
     cex.lab = 1.25, col = "darkblue", bg = "lightblue", pch = 21)
#'
qqnorm(bmTemp$air_temp, cex.lab = 1.25)
#'
plot(Max_temp$Year, log(Max_temp$Tmax), xlab = "Year",
     ylab = "log(precip_mm)", cex = 1.25,
     cex.lab = 1.25, col = "darkblue", bg = "lightblue", pch = 21)
#'
qqnorm(log(Max_temp$Tmax), cex.lab = 1.25)
#'
#' Select a threshold for daily data from above.
threshrange.plot(Max_temp$Tmax, r = c(28,38), nint=10)
#'
mrlplot(Max_temp$Tmax)
#'
#'
plot(year(bmTemp$timestamp), bmTemp$air_temp, col = "white", pch = 21, bg = "darkred", cex=1.5)
#'
#' estimates of the auto-tail dependence function(s) (atdf) based on either chi (rho) 
#' or chibar (rhobar), or both.
atdf(Max_temp$Tmax, 0.90)
#'
#' The extremal index is a useful indicator of how much clustering of exceedances 
#' of a threshold occurs in the limit of the distribution. For independent data, 
#' theta = 1, (though the converse is does not hold) and if theta < 1, 
#' then there is some dependency (clustering) in the limit.
extremalindex(Max_temp$Tmax, threshold = 30, method = "runs", run.length = 9, blocks=Max_temp$Year)
#'
#' Decluster data above a given threshold to try to make them independent.
dcTemp_year <- decluster(Max_temp$Tmax, 30, r = 9)
plot(dcTemp_year)
#'
#' Fit a GEV distribution to annual maximum temperature
GEV_temp_year <- fevd(bmTemp$air_temp, type='GEV', units = "deg C")
#'
GEV_temp_year
#'
ci(GEV_temp_year, alpha=0.05, type="parameter")
#'
plot(GEV_temp_year)
#plot(GEV_temp_year, "trace")
#'
return.level(GEV_temp_year, return.period=c(2,5,10,20,30,50,100), do.ci=TRUE)
#'
#' **Gumbel**
Gumbel_temp <- fevd(bmTemp$air_temp, type="Gumbel", units="deg C")
Gumbel_temp
#'
ci(Gumbel_temp, alpha=0.05, type="parameter")
#'
plot(Gumbel_temp)
#plot(Gumbel_temp, "trace")
#'
return.level(Gumbel_temp, return.period=c(2,5,10,20,30,50,100), do.ci=TRUE)
#'
lr.test(Gumbel_temp, GEV_temp_year, alpha = 0.05)
#'
#' **Bayesian**
Bayesian_temp <- fevd(bmTemp$air_temp, method = "Bayesian")
Bayesian_temp
#'
postmode(Bayesian_temp)
#'
plot(Bayesian_temp)
plot(Bayesian_temp, "trace")
#' The result is a distribution of the parameters locations, scale and shape
#'
ci(Bayesian_temp, alpha=0.05, type="parameter")
return.level(Bayesian_temp, return.period=c(2,5,10,20,30,50,100), do.ci=TRUE)
#'
pextRemes(Bayesian_temp, c(32, 34, 36, 38, 40), lower.tail = FALSE)
#'
#'