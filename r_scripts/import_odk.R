#R. Piedrahita 07-01-2020
#Code to ingest ODK data 
library(data.table)
library(tidyverse)
library(readr)
local_tz = 'Africa/Nairobi'

not_all_na <- function(x) any(!is.na(x))

choices_odk <- read_xlsx("~/Dropbox/Jacaranda Kenya Field Folder/Sampling forms/ODK/Air_InstrumentEndODK_MNCH_v6_current.xlsx",
                         sheet = "choices",col_names = c("key","num","description"))


#Import ODK start files
odkstart_b = read_csv("~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/MNCH_InstrumentStart.csv/MNCH_InstrumentStart-D_householdmembersrepeat.csv",
                      col_names = TRUE,
                      col_types = cols(.default = col_character())) %>% 
  dplyr::select(-KEY) %>%
  pivot_wider(names_from = c(1), values_from = c(3:ncol(.)-1)) %>% 
  rename(KEY = PARENT_KEY)

odkstart = read_csv("~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/MNCH_InstrumentStart.csv/MNCH_InstrumentStart.csv",
                    col_names = TRUE,
                    col_types = cols(.default = col_character())) %>% 
  left_join(odkstart_b,by = "KEY") %>% 
  setNames(gsub("[[:punct:]]","", names(.))) %>% 
  dplyr::mutate(datetimestart = as.POSIXct(paste0(AsamplingtypeA2date,AsamplingtypeA21time),tz = local_tz),
                hhid = consentgivenCHHInfoC1hhid) %>% 
  dplyr::select(-rcodefref,-rcodefothersp,-metainstanceID,-SubmitterID,-SubmitterName,-AttachmentsPresent,
                -AttachmentsExpected,-Status,-consentgivenCHHInfoC1hhid)



odkend = read_csv("~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/MNCH_Instrumentend.csv/MNCH_Instrumentend.csv",
                  col_names = TRUE,
                  col_types = cols(.default = col_character())) %>% 
  dplyr::mutate(survey_datetime_end = paste0(`A_samplingtype-A2_date`,`A_samplingtype-A3_time`),
                j_datetime_end = paste0(`J_Firstactivity-J2_date`,`J_Firstactivity-J3_startdate`),
                j_datetime_start = paste0(`J_Firstactivity-J2_date`,`J_Firstactivity-J4_starttime`),
                hhid = `C_SampleEnd-C0_hhid`) %>% 
  as.data.frame() %>% 
  dplyr::na_if(.,"NANA") %>% 
  dplyr::mutate(survey_datetime_end = as.POSIXct(survey_datetime_end,tz = local_tz),
                j_datetime_end = as.POSIXct(j_datetime_end,tz = local_tz),
                j_datetime_start = as.POSIXct(j_datetime_start,tz = local_tz)) %>% 
  dplyr::select(where(not_all_na)) %>% 
  dplyr::select(-ends_with("_hhid"),
                -ends_with("conf"),
                -ends_with("NAME"),
                -ends_with("date"),
                -ends_with("time"),
                -ends_with("instanceID"),
                -ends_with("present"),
                -ends_with("expected")) 



odkend_multiple_activities = as.data.frame(rbindlist(
  #Import a bunch of variables saved separately.
  lapply( 
    list.files("~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/MNCH_Instrumentend.csv/",full.names = TRUE) %>% 
      grep("MNCH_InstrumentEnd-", ., ignore.case = TRUE, value = TRUE),
    function(x) read_csv(x)),
  fill=TRUE)) %>% 
  dplyr::rename(question = KEY,
                KEY = PARENT_KEY,
                K3_endtime = K3_startdate,
                K2_startdate = K2_date) %>% 
  dplyr::mutate(question = gsub("^(.+?)/","",question)) %>% 
  tidyr::unite("end_time",c("E1.4_endtime","F1.4_endtime","G1.4_endtime","H1.4_endtime",
                            "I1.4_endtime","K3_endtime","L1.4_endtime","M1.4_endtime"),sep = "", remove = F, na.rm = TRUE) %>% 
  tidyr::unite("start_time",c("E1.3_starttime","F1.3_starttime","G1.3_starttime","H1.3_starttime",
                              "I1.3_starttime","K4_starttime","L1.3_starttime", "M1.3_starttime"),sep = "", remove = F, na.rm = TRUE) %>% 
  tidyr::unite("date",c("E1.2_date","F1.2_date","G1.2_date","H1.2_date",
                        "I1.2_date","K2_startdate","L1.2_date","M1.2_date"),sep = "", remove = F, na.rm = TRUE) %>%
  dplyr::mutate(K5_activities_other = gsub(" ","_",K5_activities_other)) %>% 
  tidyr::unite("K5_activities",c("K5_activities","K5_activities_other"),sep = " ", remove = F, na.rm = TRUE) %>%
  dplyr::mutate(K5_activities = gsub("other","",K5_activities),
                datetime_start = as.POSIXct(paste(date,start_time),tz = local_tz),
                datetime_end = as.POSIXct(paste(date,end_time),tz = local_tz)) %>% 
  dplyr::select(-ends_with("time"),
                -ends_with("time"),
                -ends_with("date"),
                -ends_with("pos_index"),
                -K1_activity2,-K5_activities_other) %>% 
  dplyr::select(where(not_all_na)) 


odkend_transport1 <- odkend %>% 
  select(KEY,starts_with("J_"),-ends_with("otheractivity")) %>% 
  setNames(gsub("J_Firstactivity-","", names(.))) %>% 
  dplyr::mutate(J5_activities_other = gsub(" ","_",J5_activities_other)) %>% 
  tidyr::unite("J5_activities",c("J5_activities","J5_activities_other"),sep = " ", remove = F, na.rm = TRUE) %>%
  tidyr::unite("J6_transportation",c("J6_transportation","J6_transportation_other"),sep = " ", remove = F, na.rm = TRUE) %>%
  tidyr::unite("J7_transportation2",c("J7_transportation2","J7_transportation2_other"),sep = " ", remove = F, na.rm = TRUE) %>%
  tidyr::unite("J8_smokefumes",c("J8_smokefumes","J8_smokefumes_other"),sep = " ", remove = F, na.rm = TRUE) %>%
  dplyr::mutate(J5_activities = gsub("other","",J5_activities),
                J6_transportation = gsub("other","",J6_transportation),
                J7_transportation2 = gsub("other","",J7_transportation2),
                J8_smokefumes = gsub("other","",J8_smokefumes)) %>% 
  dplyr::rename(location_visit = J_firstactivitylog) %>% 
  setNames(gsub("J","K", names(.),ignore.case = T)) %>% 
  setNames(gsub("K_date","date", names(.),ignore.case = T)) %>% 
  dplyr::select(-ends_with('other'),-K7_transportation2,-location_visit)
# names(odkend_transport1)

odkend_transport2 <- odkend_multiple_activities %>% 
  dplyr::filter(!K5_activities %in% "") %>%
  dplyr::select(-question) %>% 
  tidyr::unite("K7_transportation2",c("K7_transportation2","K7_transportation2_other"),sep = " ", remove = F, na.rm = TRUE) %>%
  tidyr::unite("K8_smokefumes",c("K8_smokefumes","K8_smokefumes_other"),sep = " ", remove = F, na.rm = TRUE) %>%
  setNames(gsub("K_Firstactivity-K","", names(.))) %>% 
  setNames(gsub("K_","", names(.),ignore.case = T)) %>% 
  dplyr::rename(location_visit = K1_activity2_other) %>% 
  dplyr::select(-ends_with('other'),-K7_transportation2,-location_visit) #K7_transportation2 has nothing unique

odkend_transport_review <- rbind(odkend_transport1,odkend_transport2) #Wide version for reviewing.

odkend_transport <- rbind(odkend_transport1,odkend_transport2) %>% 
  tidyr::pivot_longer(cols = K6_transportation:K9_childwith) %>% 
  separate_rows(value,sep = " ") %>% 
  dplyr::mutate(value = gsub(pattern = " ","",value)) %>% 
  dplyr::filter(complete.cases(.),
                ! value == "") 


odkend_survey <- odkend %>%   
  select(-starts_with("J_")) 

odkend_time_activities <- odkend_multiple_activities %>% 
  dplyr::filter(K5_activities != "") %>% 
  separate_rows(K5_activities,sep = " ") %>% 
  dplyr::select(KEY,question,K5_activities,datetime_start,datetime_end)


saveRDS(odkstart,'processed data/odkstart_processed.RDS')
write_xlsx(odkstart,'processed data/odkstart_processed.xlsx')

#Survey portion of odkend
saveRDS(odkend_survey,'processed data/odkend_survey_processed.RDS')
write_xlsx(odkend_survey,'processed data/odkend_survey_processed.xlsx')

#Transport portion of odkend
saveRDS(odkend_transport,'processed data/odkend_transport_processed.RDS')
write_xlsx(odkend_transport,'processed data/odkend_transport_processed.xlsx')

saveRDS(odkend_time_activities,'processed data/odkend_time_activities_processed.RDS')
write_xlsx(odkend_time_activities,'processed data/odkend_time_activities_processed.xlsx')


