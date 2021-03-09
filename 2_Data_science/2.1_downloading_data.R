#'---
#' title: "Statitics Course - UWI"
#' author: "AlbyDR"
#' date: "`r format(Sys.Date())`"
#' always_allow_html: yes
#' output: 
#'  github_document
#'---
#' 
library(tidyverse)
suppressPackageStartupMessages({
library(readr)      #' read_table2,read_delim
library(sp)         #' spDistsN1 function
library(mapview)    #' mapview
library(dplyr)      #' arange, filter and join_left
library(lubridate)  #' ymd
library(stringr)    #' str_split and str_extract_all
library(webshot) })
#'
#' All DWD stations locations
#'
temp <- tempfile()
download.url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/TU_Stundenwerte_Beschreibung_Stationen.txt"
download.file(download.url,temp,mode="wb")
#'
suppressWarnings(
DWDstations <- read_table2(temp, locale = locale(encoding = "ASCII"),
                           skip = 1,col_types = cols()))
#'
colnames(DWDstations) <- c("id","from_date","to_date","height",
                          "lat", "long", "name", "state")
#'
DWDstations$to_date <- ymd(DWDstations$to_date)
DWDstations$from_date <- ymd(DWDstations$from_date)
#' 
unlink(temp)
#' 
#' **Coordination**
#' 
#' TUCC
#' * **Lat**: 52.512283°
#' * **Lon**: 13.327855°
#' 
#' Distance from TUB
DWDstations$distTUB <- spDistsN1(pts=as.matrix(cbind(DWDstations$long, DWDstations$lat)),
                                 pt=c(13.327855,52.512283),longlat=TRUE)
#'
DWDstations <- filter(DWDstations, distTUB <= 30 & to_date >= "2021-01-01")
#' 
#' Let see where they are
sf_DWDstations <- SpatialPointsDataFrame(coords = as.matrix(DWDstations[,c('long','lat')]),
                                         data = DWDstations, proj4string = CRS("+init=epsg:4326"))  
#' 
mapview(sf_DWDstations, zcol='name', legend = TRUE)
#'
#' Distance from ROTH
DWDstations$distROTH <- spDistsN1(pts=as.matrix(cbind(DWDstations$long,DWDstations$lat)),
                                  pt=c(13.315827,52.457232),longlat=TRUE)
#' 
arrange(DWDstations, distROTH)
#' 
#' **Download the DWD data**
#' hourly data is divided in historical and recent data
#' example for the station number 03987 - Potsdam
#' 
#' Available hourly data
#' example [5] "<a href=\"air_temperature/\">air_temperature/</a> 

writeLines(unlist(str_extract_all(
  readLines("http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/"),
          "\".+/\"")))
#' 
# [1] "=\"../\""                "=\"air_temperature/\""   "=\"cloud_type/\""       
# [4] "=\"cloudiness/\""        "=\"dew_point/\""         "=\"extreme_wind/\""     
# [7] "=\"moisture/\""          "=\"precipitation/\""     "=\"pressure/\""         
# [10] "=\"soil_temperature/\""  "=\"solar/\""             "=\"sun/\""              
# [13] "=\"visibility/\""        "=\"weather_phenomena/\"" "=\"wind/\""             
# [16] "=\"wind_synop/\"" 
#'
#' **air temperature (Ta) - stundenwerte_TU_03987**
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/historical/"
zipfile2 <- readLines(download.url2)
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_TU_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.temp.hist <- read_delim(metadata2[14], 
                          ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                        trim_ws = TRUE)
#' 
summary(dwd.temp.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_TU_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.temp.recent <- read_delim(metadata2[10],
                              ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                              trim_ws = TRUE)
#'
head(dwd.temp.recent)
#' 
#' Join the historical and recent data
Air_Temp <- rbind(dwd.temp.hist, filter(dwd.temp.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(Air_Temp) <- c("id","timestamp","QN_9","air_temp","RH","eor")
#' 
summary(Air_Temp)
#' Convert -999 to NA
Air_Temp <- na_if(Air_Temp, -999.00)
#'
#'
#' **precipitation (prec) - stundenwerte_RR_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/precipitation/historical/"
zipfile2 <- readLines(download.url2)
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_RR_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.prec.hist <- read_delim(metadata2[16], 
                            ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                            trim_ws = TRUE)
#' 
summary(dwd.prec.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/precipitation/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_RR_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.prec.recent <- read_delim(metadata2[16],
                              ";", escape_double = FALSE, 
                              col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                              trim_ws = TRUE)
#'
head(dwd.prec.recent)
#' 
#' Join the historical and recent data
precipitation <- rbind(dwd.prec.hist, filter(dwd.prec.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(precipitation) <- c("id", "timestamp", "QN_8" , "precip_mm",
                             "precip_h", "precip_form","eor")
#' 
summary(precipitation$V_precip)
#' Convert -999 to NA
precipitation <- na_if(precipitation, -999.000)
#'
#'
#'
#' **pressure (p) - stundenwerte_P0_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/pressure/historical/"
zipfile2 <- readLines(download.url2)
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_P0_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.press.hist <- read_delim(metadata2[14], 
                         ";", escape_double = FALSE, 
                         col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                         trim_ws = TRUE)
#' 
summary(dwd.press.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/pressure/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_P0_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.press.recent <- read_delim(metadata2[10],
                           ";", escape_double = FALSE, 
                           col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                           trim_ws = TRUE)
#'
head(dwd.p.recent)
#' 
#' Join the historical and recent data
pressure <- rbind(dwd.press.hist, filter(dwd.press.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(pressure) <- c("id","timestamp","QN_8","P_sealevel","p","eor")
#' 
summary(pressure)
#' Convert -999 to NA
pressure <- na_if(pressure, -999.0)
#'
#'
#' **wind (ws/wd) - stundenwerte_F_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/wind/historical/"
zipfile2 <- readLines(download.url2) 
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_FF_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.wind.hist <- read_delim(metadata2[14], 
                             ";", escape_double = FALSE, 
                             col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                             trim_ws = TRUE)
#' 
summary(dwd.wind.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/wind/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_FF_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.wind.recent <- read_delim(metadata2[10],
                               ";", escape_double = FALSE, 
                               col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                               trim_ws = TRUE)
#'
head(dwd.wind.recent)
#' 
#' Join the historical and recent data
wind <- rbind(dwd.wind.hist, filter(dwd.wind.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(wind) <- c("id","timestamp","QN_8","wind_speed","wind_direction","eor")
#' 
summary(wind)
#' Convert -999 to NA
wind <- na_if(wind, -999)
#'
#'
#'
#' **wind_synop (w) - stundenwerte_F_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/wind_synop/historical/"
zipfile2 <- readLines(download.url2) 
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_F_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.wind_S.hist <- read_delim(metadata2[8], 
                              ";", escape_double = FALSE, 
                              col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                              trim_ws = TRUE)
#' 
summary(dwd.wind_S.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/wind_synop/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_F_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.wind_S.recent <- read_delim(metadata2[8],
                                ";", escape_double = FALSE, 
                                col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                trim_ws = TRUE)
#'
head(dwd.wind_S.recent)
#' 
#' Join the historical and recent data
wind_S <- rbind(dwd.wind_S.hist, filter(dwd.wind_S.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(wind_S) <- c("id","timestamp","QN_8","ws_Synop","wd_Synop","eor")
#' 
summary(wind_S)
#' Convert -999 to NA
wind_S <- na_if(wind_S, -999.000)
#' 
#' 
#'
#' **soil_temperature  - stundenwerte_F_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/soil_temperature/historical/"
zipfile2 <- readLines(download.url2) 
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_EB_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.soil_temperature.hist <- read_delim(metadata2[12], 
                            ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                            trim_ws = TRUE)
#' 
summary(dwd.soil_temperature.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/soil_temperature/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_EB_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.soil_temperature.recent <- read_delim(metadata2[8],
                              ";", escape_double = FALSE, 
                              col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                              trim_ws = TRUE)
#'
head(dwd.soil_temperature.recent)
#' 
#' Join the historical and recent data
soil_temperature <- rbind(dwd.soil_temperature.hist, filter(dwd.soil_temperature.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(soil_temperature) <- c("id", "timestamp", "QN_2" , "soil_temp_2cm",
                                "soil_temp_5cm", "soil_temp_10cm",
                                "soil_temp_20cm","soil_temp_50cm",
                                "soil_temp_100cm","eor")
#' 
summary(soil_temperature)
#' Convert -999 to NA
soil_temperature <- na_if(soil_temperature, -999.0)
#' 
#'
#'
#' **visibility - stundenwerte_F_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/visibility/historical/"
zipfile2 <- readLines(download.url2) 
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_VV_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.visibility.hist <- read_delim(metadata2[12], 
                            ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                            trim_ws = TRUE)
#' 
summary(dwd.visibility.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/visibility/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_VV_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.visibility.recent <- read_delim(metadata2[8],
                              ";", escape_double = FALSE, 
                              col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                              trim_ws = TRUE)
#'
head(dwd.visibility.recent)
#' 
#' Join the historical and recent data
visibility <- rbind(dwd.visibility.hist, filter(dwd.visibility.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(visibility) <- c("id", "timestamp", "QN_8" ,  "V_VV_I",
                    "visibility_km","eor")
#' 
summary(visibility)
#' Convert -999 to NA
#visibility <- na_if(visibility, -999)
#'
#'
#'
#' **sun - stundenwerte_SD_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/sun/historical/"
zipfile2 <- readLines(download.url2) 
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_SD_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.sun.hist <- read_delim(metadata2[12], 
                            ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                            trim_ws = TRUE)
#' 
summary(dwd.sun.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/sun/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_SD_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.sun.recent <- read_delim(metadata2[8],
                              ";", escape_double = FALSE, 
                              col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                              trim_ws = TRUE)
#'
head(dwd.sun.recent)
#' 
#' Join the historical and recent data
sun <- rbind(dwd.sun.hist, filter(dwd.sun.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(sun) <- c("id", "timestamp", "QN_7" , "sun.duration","eor")
#' 
summary(sun)
#'
#'
#'
#' **dew_point - stundenwerte_TD_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/dew_point/historical/"
zipfile2 <- readLines(download.url2) 
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_TD_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.dew_point.hist <- read_delim(metadata2[10], 
                            ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                            trim_ws = TRUE)
#' 
summary(dwd.dew_point.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/dew_point/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_TD_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.dew_point.recent <- read_delim(metadata2[6],
                              ";", escape_double = FALSE, 
                              col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                              trim_ws = TRUE)
#'
head(dwd.dew_point.recent)
#' 
#' Join the historical and recent data
dew_point <- rbind(dwd.dew_point.hist, filter(dwd.dew_point.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(dew_point) <- c("id", "timestamp", "QN_8", "dewpoint_temp",
                         "drybulb_temp","eor")
#' 
summary(dew_point)
#' Convert -999 to NA
dew_point <- na_if(dew_point, -999.000)
#'
#'
#'
#' **cloud_type - stundenwerte_F_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/cloud_type/historical/"
zipfile2 <- readLines(download.url2) 
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_CS_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.cloud_type.hist <- read_delim(metadata2[20], 
                            ";", escape_double = T, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H"),
                                             .default = col_character()), 
                            trim_ws = TRUE)
#' 
summary(dwd.cloud_type.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/cloud_type/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_CS_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.cloud_type.recent <- read_delim(metadata2[16],
                              ";", escape_double = FALSE, 
                              col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H"),
                                               .default = col_character()), 
                              trim_ws = TRUE)
#'
head(dwd.cloud_type.recent)
#' 
#' Join the historical and recent data
cloud_type <- rbind(dwd.cloud_type.hist, filter(dwd.cloud_type.recent, year(MESS_DATUM)>=2020))
#'
#' Rename the columns                                
colnames(cloud_type) <- c("id", "timestamp", "QN_8" ,
                    "Cloud.coverage.total",
                    "V_N_I",
                    "Cloud.type.1stlayer",
                    "Cloud.type.1st.Abrev",
                    "Cloud.coverage.1stlayer",
                    "Cloud.height.1stlayer",
                    "Cloud.type.2stlayer",
                    "Cloud.type.2st.Abrev",
                    "Cloud.coverage.2stlayer",
                    "Cloud.height.2stlayer",
                    "Cloud.type.3stlayer",
                    "Cloud.type.3st.Abrev",
                    "Cloud.coverage.3stlayer",
                    "Cloud.height.3stlayer",
                    "Cloud.type.4stlayer",
                    "Cloud.type.4st.Abrev",
                    "Cloud.coverage.4stlayer",
                    "Cloud.height.4stlayer",
                    "eor")
#' 
summary(cloud_type)
#'
#' Convert -999 to NA
cloud_type <- na_if(cloud_type, -999)
#'
#'
#'
#' **cloudiness - stundenwerte_TD_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/cloudiness/historical/"
zipfile2 <- readLines(download.url2) 
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_N_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.cloudiness.hist <- read_delim(metadata2[12], 
                                 ";", escape_double = FALSE, 
                                 col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                 trim_ws = TRUE)
#' 
summary(dwd.cloudiness.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/cloudiness/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_N_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.cloudiness.recent <- read_delim(metadata2[8],
                                   ";", escape_double = FALSE, 
                                   col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                   trim_ws = TRUE)
#'
head(dwd.cloudiness.recent)
#' 
#' Join the historical and recent data
cloudiness <- rbind(dwd.cloudiness.hist, filter(dwd.cloudiness.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(cloudiness) <- c("id", "timestamp", "QN_8","V_N_I",
                         "cloud_cover","eor")
#' 
summary(cloudiness)
#'
#
#' **extreme_wind - stundenwerte_TD_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/extreme_wind/historical/"
zipfile2 <- readLines(download.url2) 
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_FX_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.extreme_wind.hist <- read_delim(metadata2[8], 
                                 ";", escape_double = FALSE, 
                                 col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                 trim_ws = TRUE)
#' 
summary(dwd.extreme_wind.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/extreme_wind/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_FX_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.extreme_wind.recent <- read_delim(metadata2[8],
                                   ";", escape_double = FALSE, 
                                   col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                   trim_ws = TRUE)
#'
head(dwd.extreme_wind.recent)
#' 
#' Join the historical and recent data
extreme_wind <- rbind(dwd.extreme_wind.hist, filter(dwd.extreme_wind.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(extreme_wind) <- c("id", "timestamp", "QN_8", 
                            "wind_highest_peak_h", "eor")
#' 
summary(extreme_wind)
#'
#'
#'
#' **moisture - stundenwerte_TF_03987**
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/moisture/historical/"
zipfile2 <- readLines(download.url2) 
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_TF_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.moisture.hist <- read_delim(metadata2[14], 
                                 ";", escape_double = FALSE, 
                                 col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                 trim_ws = TRUE)
#' 
summary(dwd.moisture.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/moisture/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_TF_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.moisture.recent <- read_delim(metadata2[10],
                                   ";", escape_double = FALSE, 
                                   col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                   trim_ws = TRUE)
#'
head(dwd.moisture.recent)
#' 
#' Join the historical and recent data
moisture <- rbind(dwd.moisture.hist, filter(dwd.moisture.recent, year(MESS_DATUM)>=2020))
#' 
#' Rename the columns                                
colnames(moisture) <- c("id", "timestamp", "QN_8" , "Abs_humidity","Vapor_Pressure",
                         "Wet_bulb_temperature","Air_pressure", "Air_temperature_2m",
                        "rel_humidity", "dew_point_temp", "eor")
#' 
summary(moisture)
#' 
#' Convert -999 to NA
moisture <- na_if(moisture, -99.900)
#'
#'
#'
#' **weather_phenomena - stundenwerte_TD_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/weather_phenomena/historical/"
zipfile2 <- readLines(download.url2) 
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_WW_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.weather_phenomena.hist <- read_delim(metadata2[6], 
                                 ";", escape_double = FALSE, 
                                 col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                 trim_ws = TRUE)
#' 
summary(dwd.weather_phenomena.hist)
#' 
#' Download recent data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/weather_phenomena/recent/"
zipfile2 <- readLines(download.url2)
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_WW_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2
#' 
#' Read recent data
dwd.weather_phenomena.recent <- read_delim(metadata2[6],
                                   ";", escape_double = FALSE, 
                                   col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                   trim_ws = TRUE)
#'
head(dwd.weather_phenomena.recent)
#' 
#' Join the historical and recent data
weather_phenomena <- rbind(dwd.weather_phenomena.hist, filter(dwd.weather_phenomena.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(weather_phenomena) <- c("id", "timestamp", "QN_8", "Weather_changes_factor","Weather_changes_Text","eor")
#' 
#' <weather_phenomena$Weather_changes_Text>
#'
#'
#' **solar (Rin) - stundenwerte_ST_03987 **
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/solar/"
zipfile2 <- readLines(download.url2) 
zipfile2
zipfile2 <- unlist(str_extract_all(zipfile2, "stundenwerte_ST_03987_.+(.zip)"))
zipfile2 <- str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
solar <- read_delim(metadata2[14],
                             ";", escape_double = FALSE, 
                             col_names = TRUE, 
                             col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H:%M"), 
                                              ATMO_LBERG = col_double(),
                                              FD_LBERG = col_double(),
                                              FG_LBERG = col_double(),
                                              SD_LBERG = col_double(),
                                              ZENIT = col_double(),
                                              MESS_DATUM_WOZ = col_datetime(format="%Y%m%d%H:%M")), 
                             trim_ws = TRUE)
#' 
#' Rename the columns                                
colnames(solar) <- c("id","MESS_DATUM", "QN_592" , "Rli",
                     "diffuse.radiation","Rin",
                     "sunshine.duration" ,"sun.zenith.angle",
                     "MESS_DATUM_WOZ","eor")
#' 
summary(solar)
#' 
#' Convert -999 to NA
solar <- na_if(solar, -999.0)
#'
#' convert J/cm^2 to W m-2
#' Radiation (W/m^2) = Radiation (J/cm^2) *100^2 / (step x 60 x 60 seconds) 
solar$Rin <- round((solar$Rin*100*100)/(60*60),2)
solar$Rli <- round((solar$Rli*100*100)/(60*60),2)
solar$diffuse.radiation <- round((solar$diffuse.radiation*100*100)/(60*60),2)
#'
summary(solar)
solar$MESS_DATUM
solar$MESS_DATUM_WOZ
#'
solar$timestamp <- with_tz(force_tz(solar$MESS_DATUM_WOZ,"CEST"),tz="UTC")
table(diff(solar$timestamp))
#'
#' creating an DWD data.frame or tibble df
ts <- seq(as.POSIXct("1893-01-01", tz = "UTC"),
          as.POSIXct("2020-12-31", tz = "UTC"),
          by = "hour") 
head(ts)
tail(ts)

#'
#'
DWD_data <- tibble(timestamp=ts)
DWD_data <- left_join(DWD_data, Air_Temp[,c(2,4,5)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, pressure[,c(2,5,4)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, wind[,c(2,4,5)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, wind_S[,c(2,4,5)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, extreme_wind[,c(2,4)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, solar[,c(6,4,5,7,8,11)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, visibility[,c(2,5)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, sun[,c(2,4)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, dew_point[,c(2,4,5)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, soil_temperature[,c(2,4,5,6,7,8,9)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, moisture[,c(2,4,5,6,7,8,9,10)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, precipitation[,c(2,4,5)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, cloud_type[,c(2,4,6,7,9,10,11,13,14,15,17,18,19,21)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, cloudiness[,c(2,5)], by="timestamp", type="left", match="first")
DWD_data <- left_join(DWD_data, weather_phenomena[,c(2,4,5)], by="timestamp", type="left", match="first")
#'
unique(diff((DWD_data$timestamp)))
summary(DWD_data)

type_convert(DWD_data)
#'
write_rds(DWD_data, file = "DWD_data.rds")
#' DWD_data <- read_rds("DWD_data.rds")
#'
plot(x=DWD_data$timestamp, y=DWD_data$ws_Synop)
DWD_dataset <- filter(DWD_data, year(timestamp)>=2000)
summary(DWD_dataset)
plot(x=DWD_dataset$timestamp, y=DWD_dataset$precip_mm)

library(suncalc)
########################################################################
night <- getSunlightTimes(date=date(DWD_data$timestamp), 
                          lat=52.45711, lon=13.31592, tz = "UTC")

night$time <- DWD_data$timestamp
night$sun <- NULL
night$sun[night$time>night$nadir] <- "Night" ##seting
night$sun[night$time>night$night & hour(night$time)<hour(night$nadir)] <- "Night"    #rising
night$sun[night$time>night$nightEnd & night$time<night$nauticalDawn] <- "Dawn" #Morn.Astr.night
night$sun[night$time>night$nauticalDawn & night$time<night$dawn] <- "Dawn"     #Morn.Nau.ECsun
night$sun[night$time>night$dawn & night$time<night$sunrise] <- "Dawn"          #Morn.Civil.ECsun
night$sun[night$time>night$sunrise & night$time<night$goldenHourEnd] <- "Goldenhour_morning"
night$sun[night$time>night$goldenHourEnd & night$time<night$solarNoon] <- "Sun_rising"
night$sun[night$time>night$goldenHourEnd & hour(night$time)<(hour(night$solarNoon)-2)] <- "Sun_rising"
night$sun[hour(night$time)==round(hour(night$solarNoon))] <- "Noon"
night$sun[night$time>night$solarNoon & night$time<night$goldenHour] <- "Sun_setting"
night$sun[night$time>night$goldenHour & night$time<night$sunset] <- "Goldenhour_afternoon"
night$sun[night$time>night$sunset & night$time<night$dusk] <- "Dusk"        #Even.Civil.ECsun
night$sun[night$time>night$dusk & night$time<night$nauticalDusk] <- "Dusk"  #Even.Nau.ECsun
night$sun[night$time>night$nauticalDusk & night$time<night$night] <- "Dusk" #Even.Astr.ECsun

unique(night$sun)

DWD_data$sunlight_times <- night$sun


##################################################
# calculate the raining window
##################################################
window.prec <- NULL

for (i in 1:400) {
  window.prec[[i]] <- data.frame(
    arrange(data.frame(row=unique(c(which(DWD_data$prec_hour==1)+i))),row),
    hour=i )
  colnames(window.prec[[i]]) <- c("row",paste("hour",i, sep=""))
}

row_0 <- data.frame(
  arrange(data.frame(row=unique(c(which(DWD_data$prec_hour==1)+0))),row),
  "hour_0" = 0)

row_timestamp <- data.frame(timestamp=DWD_data$timestamp)
row_timestamp$row <- row_number(DWD_data$timestamp)
row_timestamp <- left_join(row_timestamp, row_0, by="row")

for (i in 1:400) {
  row_timestamp <- left_join(row_timestamp, window.prec[[i]], by="row")
}

row_timestamp$prec.window <- 400

for (i in 399:0) {
  row_timestamp$prec.window[row_timestamp[i+3]==i] <- i 
}

DWD_data$prec.window <- row_timestamp$prec.window



library(lubridate)
saveRDS(DWD_dataset, file = "DWD_dataset.Rds")
DWD_dataset <- read_rds("DWD_data.rds")
print(DWD_dataset, n = 5, width = Inf)

# Cirrus        0 CI
# Cirrocumulus  1 CC
# Cirrostratus  2 CS
# Altocumulus   3 AC
# Altostratus   4 AS
# Nimbostratus  5 NS
# Stratocumulus 6 SC
# Stratus       7 ST
# Cumulus       8 CU
# Cumulonimbus  9 CB
# bei Instrumentenmessung -1 -1

class(DWD_dataset$Cloud.coverage.total)
typeof(DWD_dataset$Cloud.coverage.total)
attributes(DWD_dataset$Cloud.coverage.total)
unclass(DWD_dataset$Cloud.coverage.total)

DWD_dataset$Cloud_coverage_fct <- as_factor(DWD_dataset$Cloud.coverage.total)
class(DWD_dataset$Cloud_coverage_fct)
typeof(DWD_dataset$Cloud_coverage_fct)
attributes(DWD_dataset$Cloud_coverage_fct)
unclass(DWD_dataset$Cloud_coverage_fct)

DWD_dataset$Cloud_coverage_fct <- as_factor(as.numeric(DWD_dataset$Cloud.coverage.total))
class(DWD_dataset$Cloud_coverage_fct)
typeof(DWD_dataset$Cloud_coverage_fct)
attributes(DWD_dataset$Cloud_coverage_fct)
unclass(DWD_dataset$Cloud_coverage_fct)

levels(DWD_dataset$Cloud_coverage_fct)
fct_count(DWD_dataset$Cloud_coverage_fct)
fct_unique(DWD_dataset$Cloud_coverage_fct)

levels(DWD_dataset$Cloud_coverage_fct) <- c("by_instrument", "Cirrus", "Cirrocumulus", "Cirrostratus", "Altocumulus",
            "Altostratus","Nimbostratus","Stratocumulus","Stratus",
                 "Cumulus","Cumulonimbus")

fct_explicit_na(f1, na_level = "(Unknown)")
#combine factors
fct_collapse(mtcars$cyl, Other = c("4", "6"))
# Example showing keep as argument
fct_other(mtcars$cyl, keep = c("8"))
# Example showing drop as argument
fct_other(mtcars$cyl, drop = c("4", "6"))

fct_unique(as_factor(DWD_dataset$Weather_changes_Text))

library(tidyverse)
class(DWD_dataset$precip_h)
DWD_dataset$precip_h <- factor(DWD_dataset$precip_h, levels = c(0,1), labels=c("dry","rain_occur"))

typeof(DWD_dataset$precip_h)
attributes(DWD_dataset$precip_h)
unclass(DWD_dataset$precip_h)

attributes(DWD_dataset$precip_h)

typeof(DWD_dataset)

#fct_collapse() fct_recode() fct_lump()
# gss_cat %>%
#   mutate(df = fct_recode(df,
#                               "Republican, strong"    = "Strong republican",
#                               "Republican, weak"      = "Not str republican",
#                               "Independent, near rep" = "Ind,near rep",
#                               "Independent, near dem" = "Ind,near dem",
#                               "Democrat, weak"        = "Not str democrat",
#                               "Democrat, strong"      = "Strong democrat",
#                               "Other"                 = "No answer",
#                               "Other"                 = "Don't know",
#                               "Other"                 = "Other party"
#   )) %>%
# 
# gss_cat %>%
#   mutate(df = fct_collapse(df,
#                                 other = c("No answer", "Don't know", "Other party"),
#                                 rep = c("Strong republican", "Not str republican"),
#                                 ind = c("Ind,near rep", "Independent", "Ind,near dem"),
#                                 dem = c("Not str democrat", "Strong democrat")
#   )) %>%
#   count(df)


# gss_cat %>%
#   mutate(relig = fct_lump(relig, n = 10)) %>%
#   count(relig, sort = TRUE) %>%
#   print(n = Inf)


DWD_dataset$Weather_changes_Text

DWD_dataset %>%
 mutate(Weather_changes_Text = forcats::fct_lump(Weather_changes_Text, n = 20)) %>%
 count(Weather_changes_Text, sort = TRUE) %>%
 print(n = Inf)

library(ggplot2)
ggplot() +
  geom_bar() 

summary(DWD_dataset)
  
  ggplot(DWD_dataset, aes(x=factor(`Cloud_cover `))) +
  geom_bar()
  
  ggplot(DWD_dataset[,], aes(x = factor(`Cloud_cover `), y = air_temp)) +
  geom_col()

  ggplot(DWD_dataset, aes(x = `Cloud_cover `, y = air_temp)) +
    geom_col()
  
  ggplot(DWD_dataset) +
    geom_bar(aes(x = factor(precip_h)))
  
  ggplot(DWD_dataset) +
    geom_col(aes(y = (precip_h),x = factor(`Cloud_cover `)))
  
  ggplot(DWD_dataset, aes(x = factor(precip_h), y = air_temp)) +
    geom_col()
  
  ggplot(DWD_dataset, aes(y =factor(precip_h), x = air_temp)) +
    geom_col()
  
  ggplot(DWD_dataset, aes(y =precip_h, x = air_temp)) +
    geom_col()
  
  ggplot(DWD_dataset, aes(y = factor(precip_h), x = year(timestamp), fill = precip_h)) +
    geom_col(position = "dodge")
  
    ggplot(DWD_dataset, aes(x = year(timestamp), y=air_temp, fill = factor(precip_h))) +
    geom_col(position = "dodge")

    
    
    ggplot(DWD_dataset, aes(x = factor(`Cloud_cover `), fill = factor(precip_h))) +
      geom_bar(position = "dodge")
    
    ggplot(DWD_dataset, aes(x = factor(precip_h), fill = factor(`Cloud_cover `) )) +
      geom_bar(position = "dodge")
    
    
    ggplot(DWD_dataset, aes(x = Cloud_coverage_fct, fill = factor(precip_h))) +
      geom_bar(position = "dodge")
    
    ggplot(DWD_dataset, aes(x = factor(precip_h), fill = factor(`Cloud_cover `) )) +
      geom_bar(position = "dodge")
  