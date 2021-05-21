#R. Piedrahita 07-01-2020
#Code to ingest GPS logger data
# source('r_scripts/load.R')
# source('r_scripts/import_odk.R')
path_inventory = "~/Dropbox/Jacaranda Kenya Field Folder/Data"
path_test_gps = "GPS Tests"
path_study_gps = "~/Dropbox/Jacaranda Kenya Field Folder/Data/GPS"
path_temp_gps = path_study_gps
local_tz = 'Africa/Nairobi'


##### Import GPS data

#List of GPS files
gps_paths <- list.files(
  path = path_temp_gps,
  recursive = TRUE,
  full.names = TRUE
) %>%
  grep("csv", x = ., ignore.case = TRUE, value = TRUE) %>%
  grep("xls", x = ., ignore.case = TRUE, value = TRUE, invert = TRUE) %>%
  grep("old ", x = ., ignore.case = TRUE, value = TRUE, invert = TRUE) %>%
  print()

#Import GPS data, format the time stamp, get rid of cruft
import_gps_fun <-function(x){
  readr::read_csv(x,
                  col_names = TRUE,
                  col_types = cols(.default = col_character())
  ) %>%
    dplyr::mutate(filename = x,
                  filesize_kb = file.size(x)/1000) %>%
    dplyr::mutate(basename = file_path_sans_ext(basename(filename))) %>%
    tidyr::separate(basename,c("hhid","instrument_id","datestart"),sep="_", extra = "merge")# %>% 
    # dplyr::mutate(sampletype = str_extract(hhid, "[[:alpha:]]+")) %>%
    # dplyr::mutate(hhid = str_extract(hhid, "[[:digit:]]+"))
}

# regexp <- "[[:digit:]]+"
# str_extract(data, "[[:digit:]]+")

# Or sub("[^[:alpha:]]+", "", x) – David Arenburg Jun 18 '15 at 10:14

# process string
# str_extract(data, regexp)

gps_data <- ldply(gps_paths,import_gps_fun,.parallel = TRUE) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(lat = as.numeric(gsub("N|S","",`LATITUDE N/S`)),
                lat = case_when(`LATITUDE N/S` %like% 'S' ~ -lat,
                                TRUE ~ lat),
                lon = as.numeric(gsub("E|W","",`LONGITUDE E/W`)),
                lon = case_when(`LONGITUDE E/W` %like% 'W' ~ -lon,
                                TRUE ~ lon),
                HEIGHT = as.numeric(HEIGHT),
                SPEED = as.numeric(SPEED),
                UTCDateTime = as.POSIXct(paste(DATE,TIME), format="%y%m%d %H%M%S")) %>%
  dplyr::select(-DATE,-TIME,-INDEX,-TAG,-`LONGITUDE E/W`,-`LATITUDE N/S`) %>%
  as.data.table()


# create time chunk columns
my_breaks = seq(floor_date(min(gps_data$UTCDateTime,na.rm=TRUE),'minutes'), 
                floor_date(max(gps_data$UTCDateTime,na.rm=TRUE),'minutes'),
                by = '1 min')
gps_data[,UTCDateTime := as.POSIXct(cut(UTCDateTime, breaks=my_breaks), tz = "UTC")]


#Calculate minute averages of key variables, grouped by relevant metadata
gps_data = gps_data[,list(lat = mean(lat, na.rm = T),
                          lon = mean(lon, na.rm = T),
                          HEIGHT = mean(HEIGHT, na.rm = T),
                          SPEED = mean(SPEED, na.rm = T)),
                    by=list(UTCDateTime, filename,instrument_id,datestart,hhid,filesize_kb)] #sampletype
gps_data[,local_time := with_tz(UTCDateTime,tzone = "Africa/Nairobi")]
gps_data[,UTCDateTime := NULL]



saveRDS(gps_data,"processed data/gps_data.RDS")

gps_data
