# rm(list = ls())

#R. Piedrahita 07-01-2020
#Code to ingest PA-II-SD

library(tidyverse)
library(ggrepel)
library(tidyfast)
# devtools::install_github("dkahle/ggmap")
source('r_scripts/load.R')
source('r_scripts/plot_deployments.R')
source('r_scripts/import_odk.R')
# install.packages('MazamaSpatialUtils')
# devtools::install_github('MazamaScience/MazamaCoreUtils')
# devtools::install_github('MazamaScience/PWFSLSmoke', build_vignettes=FALSE)
# devtools::install_github('MazamaScience/AirSensor')
graphics.off()

path_inventory = "~/Dropbox/Jacaranda Kenya Field Folder/Data"

path_test = "PurpleAir Tests"
path_study = "~/Dropbox/Jacaranda Kenya Field Folder/Data"

path_temp = path_study #Change as needed.


##### Import all PurpleAir inventories

all_pa_inventories <- list.files(
  path = path_inventory,
  recursive = TRUE,
  full.names = TRUE
) %>%
  grep("purple", x = ., ignore.case = TRUE, value = TRUE) %>%
  grep("xlsx", x = ., ignore.case = TRUE, value = TRUE) %>%
  tibble(
    path = .) %>% 
  mutate(
    data = map(
      path,readxl::read_xlsx,
      skip = 1,
      sheet = 'Monitors',
      col_types = c("text", "text", "text","text", "numeric", "numeric", "text","text"),
      col_names = c("location", "id", "sitename","mac_address", "latitude", "longitude", "instrument_id","device_type")
    )
  ) %>%
  dplyr::select(-path) %>%
  unnest(cols = c(data)) %>% 
  as.data.table()

##### Import PA data

#List all files resembling a PurpleAir path
all_pa_paths <- list.files(
  path = path_temp,
  recursive = TRUE,
  full.names = TRUE
) %>%
  grep("csv", x = ., ignore.case = TRUE, value = TRUE) %>%
  grep("PurpleAir", x = ., ignore.case = TRUE, value = TRUE) %>%
  grep("plots", x = ., ignore.case = TRUE, value = TRUE,invert = TRUE) %>%
  grep("GPS", x = ., ignore.case = TRUE, value = TRUE, invert = TRUE) %>%
  print()


import_pa <-function(x){
  readr::read_csv(x,
                  col_names = TRUE,
                  col_types = paste0(paste0(replicate(4,'c'),collapse = ''),paste0(replicate(36,'d'),collapse = ''),'c',collapse = '')
  ) %>%
    dplyr::mutate(filename = x) %>%
    dplyr::mutate(basename = file_path_sans_ext(basename(filename))) %>%
    tidyr::separate(basename,c("hhid","instrument_id","datestart"),sep="_", extra = "merge")# %>%
    # dplyr::mutate(sampletype = str_extract(hhid, "[[:alpha:]]+")) %>%
    # dplyr::mutate(hhid = str_extract(hhid, "[[:digit:]]+"))
}

pa_data <- ldply(all_pa_paths,import_pa,.parallel = TRUE) %>%
  dplyr::bind_rows() %>%
  as.data.table()


pa_data$UTCDateTime = gsub("T"," ", pa_data$UTCDateTime)
pa_data$UTCDateTime = gsub("z","", pa_data$UTCDateTime)
pa_data = pa_data[!grepl("[+]", pa_data$UTCDateTime),]
pa_data = pa_data[str_length(pa_data$UTCDateTime)>15,]
pa_data = pa_data[pa_data$p_0_5_um != "",]

suppressWarnings(pa_data[,( colnames(pa_data)[5:40]):= lapply(.SD, as.numeric), .SDcols =  colnames(pa_data)[5:40]])

#Deal with timestamps
pa_data[,UTCDateTime := as.POSIXct(UTCDateTime,"UTC", "%Y/%m/%d %H:%M:%S")]
pa_data = na.omit(pa_data,"UTCDateTime")


#Merge inventory and data
pa_data = merge(pa_data,all_pa_inventories,by= "mac_address")


#Use the average of these channels as per Mailings et al. 2019b.  Also filter out data above 4000 when doing the average
pa_data[, pm2_5_cf_1_ave := dt_case_when(pm2_5_cf_1 <4000 & pm2_5_cf_1_b<4000 ~ rowMeans(pa_data[,c('pm2_5_cf_1','pm2_5_cf_1_b')],na.rm = TRUE) , #PM25 kitchen uses mostly ECM, but some corrected PATS data.
                                         pm2_5_cf_1 <4000  ~ pm2_5_cf_1,
                                         pm2_5_cf_1_b <4000  ~ pm2_5_cf_1_b)]

#Flag points above 4000 for inspection
pa_data[, flag_maxedout := ifelse(pm2_5_cf_1 > 4000 | pm2_5_cf_1_b > 4000, 1, 0)]

#For each PA, round the dates, leave opportunity to average as desired
rollup_pm = '1 min' #Time averaging 

# create time chunk columns
my_breaks = seq(floor_date(min(pa_data$UTCDateTime,na.rm=TRUE),'minutes'), 
                floor_date(max(pa_data$UTCDateTime,na.rm=TRUE),'minutes'),
                by = rollup_pm)
pa_data[,UTCDateTime := as.POSIXct(cut(UTCDateTime, breaks=my_breaks), tz = "UTC")]
setnames(pa_data, "instrument_id.x", "instrument_id")


#Calculate minute averages of key variables, grouped by relevant metadata
pa_data = pa_data[,list(current_temp_f = mean(current_temp_f, na.rm = T),
                        current_humidity = mean(current_humidity, na.rm = T),
                        pressure = mean(pressure, na.rm = T),
                        pm2_5_cf_1_ave = mean(pm2_5_cf_1_ave, na.rm = T),
                        pm10_0_cf_1 = mean(pm10_0_cf_1, na.rm = T),
                        pm2_5_cf_1 = mean(pm2_5_cf_1, na.rm = T),
                        pm10_0_cf_1_b = mean(pm10_0_cf_1_b, na.rm = T),
                        pm2_5_cf_1_b = mean(pm2_5_cf_1_b, na.rm = T),
                        flag_maxedout = mean(flag_maxedout,na.rm = T)), #Calculate sd of acceleration by minute for compliance calcs.
                  by=list(UTCDateTime, filename, mac_address, firmware_ver, id, sitename, latitude,longitude,instrument_id,hhid)] #sampletype

pa_data[,local_time := with_tz(UTCDateTime,tzone = "Africa/Nairobi")]


######Calculate concentrations
pa_data$pm25_larpa_ave = 0.778 * pa_data$pm2_5_cf_1_ave + 2.65

saveRDS(pa_data,"processed data/pa_data.rds")

return(pa_data)


