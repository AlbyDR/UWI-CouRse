#'---
#'title: "Statistics Course - UWI"
#'author: "AlbyDR"
#'date: "`r format(Sys.Date())`"
#'output: github_document
#'---
#'
#'## Example 2.1 Tidy Data
#' 
library(tidyverse)
library(lubridate)
#' 
#' 
#'### DWD precipitation dataset (prec) 
#' 
#' **Download historical data**
#' *file RR -  id 03987 (Potsdam)*
#' 
#' `string::str_extract_all` and `string::str_split`
#' 
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/precipitation/historical/"
zipfile <- readLines(download.url)
zipfile <- unlist(str_extract_all(zipfile, "stundenwerte_RR_03987_.+(.zip)"))
zipfile
zipfile <- str_split(zipfile, ">")[[1]][2]
zipfile
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 
#' 
#' **Read historical data**
#' `readr::read_delim`
dwd_prec <- read_delim(metadata[16], 
                       ";", escape_double = FALSE, 
                       col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                       trim_ws = TRUE)
#'
dwd_prec 
#'
#' **Rename the variables/vectors/columns**
colnames(dwd_prec) <- c("id", "timestamp", "QN_8" , "precip_mm",
                        "prec_hour", "measure_form","eor")
#'
#' **convert the variable measure_form to integer
dwd_prec$measure_form <- as.integer(dwd_prec$measure_form)
#'
#' **Run variables summary to Check for NA or estrange values**
summary(dwd_prec)
#'
#' **Convert -999.0000 and -999.0 to NA**
dwd_prec <- na_if(dwd_prec, -999)
#' 
#'  
#'### DWD Cloudiness data set 
#' **stundenwerte_TD_03987**
#' 
#' **Download historical data**
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/cloudiness/historical/"
zipfile <- readLines(download.url) 
zipfile <- unlist(str_extract_all(zipfile, "stundenwerte_N_03987_.+(.zip)"))
zipfile <- str_split(zipfile, ">")[[1]][2]
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 
#' 
#' **Read historical data**
dwd.cloudiness <- read_delim(metadata[12], 
                             ";", escape_double = FALSE, 
                             col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                             trim_ws = TRUE)
#' 
dwd.cloudiness
#'
colnames(dwd.cloudiness) <- c("id", "timestamp", "QN_8","V_N_I",
                              "cloud_cover","eor")
#' 
summary(dwd.cloudiness)
#' 
class(dwd.cloudiness$cloud_cover)
#' 
#' **Convert to factor (categorical variable)**
#' `forcats::as_factor` and `forcats::fct_unique`'`
#' 
dwd.cloudiness$cloud_cover <- as_factor(dwd.cloudiness$cloud_cover)
#' 
#' **check the class and levels**
class(dwd.cloudiness$cloud_cover)
typeof(dwd.cloudiness$cloud_cover)
attributes(dwd.cloudiness$cloud_cover)
fct_unique(dwd.cloudiness$cloud_cover)
#' 
#' *name the levels of the factor*
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
#'
levels(dwd.cloudiness$cloud_cover) <- c("No_clouds", "Cirrus", "Cirrocumulus", "Cirrostratus", "Altocumulus", 
                                        "Altostratus","Nimbostratus","Stratocumulus","Stratus", "Cumulus",
                                        "Cumulonimbus")
fct_count(dwd.cloudiness$cloud_cover)
#'
#' **convert NA in NA level
dwd.cloudiness$cloud_cover <- fct_collapse(dwd.cloudiness$cloud_cover,
                                     "NA" = c("InstrumentM", NA))

dwd.cloudiness$cloud_cover <- fct_explicit_na(dwd.cloudiness$cloud_cover, na_level = "NA")
fct_count(dwd.cloudiness$cloud_cover)
summary(dwd.cloudiness$cloud_cover)
#'

#' If you try to join the two dataset by assign the variable to the other,
#' it will fail, because the number of observations are different
#dwd_prec$cloud_cover <- dwd.cloudiness$cloud_cover
#' 
summary(dwd_prec$timestamp)
#' If you try to join the two dataset by filtering the timestamp from cloudiness,
#' base on the precipitation, also will fail because they have different gaps. 
#' 
#dwd_prec$cloud_cover <- filter(dwd.cloudiness, 
#                               timestamp >= "1995-09-01 00:00:00" &
#                               timestamp <= "2019-12-31 23:00:00")$cloud_cover
#'
#' The best solution is to use a join function `dplyr::left_join`
#' select the variables that are relevant, for instance id (var1) is all the same
DWD_prec_data <- left_join(dwd_prec[,c(2,3,4,5)], dwd.cloudiness[,c(2,5)], by="timestamp", match="first")
DWD_prec_data
#' Now works, but lets check the gaps in the timestamp 
unique(diff((DWD_prec_data$timestamp)))
#'
#' Even better option is to create new timestamp for DWD data.frame or tibble df
ts <- seq(as.POSIXct("1995-09-01 00:00:00", tz = "UTC"),
          as.POSIXct("2019-12-31 23:00:00", tz = "UTC"),
          by = "hour")
#'
#' Now, lets join again
DWD_prec_data <- tibble(timestamp=ts)
DWD_prec_data <- left_join(DWD_prec_data, dwd_prec[,c(2,3,4,5)], by="timestamp", type="left", match="first")
DWD_prec_data <- left_join(DWD_prec_data, dwd.cloudiness[,c(2,5)], by="timestamp", type="left", match="first")
#'
#' lets check again the gaps in the timestamp 
unique(diff((DWD_prec_data$timestamp))) 
#' No gaps
#' 
lubridate::tz(DWD_prec_data$timestamp)
#' 
DWD_prec_data
#' **The data now looks tidy!**
#'
#'
#'#### In this example we saw how to:
#'* download and read data from DWD stations (`basic, util, readr, stringr`)
#'* redefine data type and create factors when necessary (`forcats, stringr`)
#'* how manipulation data and use logical operator such as filter and join (`dplyr`)
#'* convert to missing values (NA) (`basic, forcats`)
#'* check the timezone (`lubridate`)
#'  
#'  
#'  
#' **Note:** *go to the appendix <2.1_downloading_data> if you want to see 
#' the complete script to download all available hourly DWD_prec_data* 
#'  
#'### Exercise 2.1
#'1. read the data [DWD_prec_data.csv](https://github.com/AlbyDR/UWI-CouRse/tree/main/2_Data_science/DWD_prec_data.csv) 
#' using the function `read_csv()`;
#'1. check if the variable timestamp is assigned as datatime printing DWD_prec_data;
#'1. convert `DWD_prec_data$sunlight_times` to a factor using `as_factor()`;
#'1. create a variable `DWD_prec_data$day_night <- DWD_prec_data$sunlight_times` and 
#' reclassify to day or night only using `fct_collapse()`;
#'1. are the factors sunlight_times and day_night nominal or ordinal?;
#'1. save DWD_prec_data to binary (.rds) instead of text file (.csv) using `write_rds("DWD_prec_data.rds")`.
#'
#'*suggestion:* check the function help `??read_csv()`, `??as_factor()` and 
#' `??fct_collapse()` if you are in doubt how to use it.
#' 
#'
#'write_rds(DWD_prec_data, file = "C:/Users/Alby Rocha/Documents/UWI/UWI-CouRse/2_Data_science/DWD_prec_data.rds")
