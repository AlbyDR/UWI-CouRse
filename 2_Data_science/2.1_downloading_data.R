#'---
#' title: "Downloading DWD Data"
#' author: "Alby"
#' date: "26/02/2021"
#' always_allow_html: yes
#' output: 
#'  github_document
#'---
#' 
suppressPackageStartupMessages({
library(readr)     #' #read files
library(sp)        #' #spDistsN1 function
library(mapview)   #' #mapview
library(dplyr, quietly = TRUE, verbose=F)     #' #arange
library(lubridate, quietly = TRUE, verbose=F) #' #ymd
library(stringr)   #'
library(webshot) })
#'
#' All DWD stations locations
#'
temp <- tempfile()
download.url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/TU_Stundenwerte_Beschreibung_Stationen.txt"
download.file(download.url,temp,mode="wb")
#'
#'
suppressWarnings(
DWDstations <- read_table2(temp, locale = locale(encoding = "ASCII"),skip = 1,col_types = cols())
)
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
DWDstations$distTUB <- spDistsN1(pts=as.matrix(cbind(DWDstations$long,
                                                     DWDstations$lat)),
                                 pt=c(13.327855,52.512283), 
                                 longlat=TRUE)
#'
DWDstations <- filter(DWDstations, distTUB <= 30 & to_date >= "2021-01-01")
#' 
#' Let see where they are
sf_DWDstations <- SpatialPointsDataFrame(coords = as.matrix(DWDstations[,c('long','lat')]),
                                         data = DWDstations,
                                       proj4string = CRS("+init=epsg:4326"))  
#' 
mapview(sf_DWDstations, zcol='name', legend = TRUE)
#' Distance from ROTH
DWDstations$distROTH <- spDistsN1(pts=as.matrix(cbind(DWDstations$long,
                                                    DWDstations$lat)),
                                  pt=c(13.315827,52.457232),
                                 longlat=TRUE)
#' 
arrange(DWDstations, distROTH)
#' 
#' **Download the DWD data**
#' hourly data is divided in historical and recent data
#' example for the station number 00403
#' 
#' **air temperature (Ta)**
#' 
#' Download historical data
temp2 <- tempfile()
download.url2 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/historical/"
zipfile2 <- readLines(download.url2) 
zipfile2 <- unlist(stringr::str_extract_all(zipfile2, "stundenwerte_TU_00403_.+(.zip)"))
zipfile2 <- stringr::str_split(zipfile2, ">")[[1]][2]
download.file(paste0(download.url2,zipfile2),temp2, mode="wb")
metadata2 <- unzip(temp2)
unlink(temp2)
metadata2 
#' 
#' Read historical data
dwd.temp.hist <- read_delim(metadata2[12], 
                          ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                        trim_ws = TRUE)
#' 
summary(dwd.temp.hist)
#' 
#' Download recent data
temp3 <- tempfile()
download.url3 <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/"
zipfile3 <- readLines(download.url3)
zipfile3 <- unlist(str_extract_all(zipfile3, "stundenwerte_TU_00403_.+(.zip)"))
zipfile3 <- str_split(zipfile3, ">")[[1]][2]
download.file(paste0(download.url3,zipfile3),temp3, mode="wb")
metadata3 <- unzip(temp3)
unlink(temp3)
metadata3
#' 
#' Read recent data
dwd.temp.recent <- read_delim(metadata3[12],
                              ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                              trim_ws = TRUE)
#'
head(dwd.temp.recent)
#' 
#' Join the historical and recent data
Air_Temp <- rbind(dwd.temp.hist, filter(dwd.temp.recent, year(MESS_DATUM)>=2020))
#' Rename the columns                                
colnames(Air_Temp) <- c("id","timestamp","QN_9","Ta","RH","eor")
#' 
summary(Air_Temp$Ta)
#' Convert -999 to NA
Air_Temp <- na_if(Air_Temp, -999)
plot(x=Air_Temp$timestamp, y=Air_Temp$Ta)
