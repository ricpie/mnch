#R. Piedrahita 07-01-2020
#Code to ingest ODK data 
library(plyr)
library(data.table)
library(tidyverse)
library(readr)

local_tz = 'Africa/Nairobi'

allodkfiles = list.files("/Users/ricardopiedrahita/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/MNCH_InstrumentEnd.csv")
#Incorporate all fuel repeats info, K8_smoke, any others?
#Make sure all activities are in.  Make sure all transport are in.


not_all_na <- function(x) any(!is.na(x))

choices_odk <- read_xlsx("~/Dropbox/Jacaranda Kenya Field Folder/Sampling forms/ODK/Air_InstrumentEndODK_MNCH_v6_current.xlsx",
                         sheet = "choices",col_names = c("key","num","description"))



# Import ODK start files -------------------------------------------------


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



# Import ODK end files ----------------------------------------------------
# Clean and organize the activities, transport, and survey data

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
                j_datetime_end = as.POSIXct(j_datetime_start,tz = local_tz),
                j_datetime_start = as.POSIXct(j_datetime_end,tz = local_tz)) %>% 
  dplyr::select(where(not_all_na)) %>% 
  dplyr::select(-ends_with("_hhid"),
                -ends_with("conf"),
                -ends_with("NAME"),
                -ends_with("date"),
                -ends_with("time"),
                -ends_with("instanceID"),
                -ends_with("present"),
                -ends_with("expected")) %>% 
  dplyr::mutate(across(c(ends_with("yn"),ends_with("frequency"),ends_with("cooking")), as.numeric),
                across(c(ends_with("yn"),ends_with("cooking")),  ~ abs(.x - 2)),
                across(c(ends_with("yn"),ends_with("cooking")), as.factor)) %>% 
  dplyr::full_join(choices_odk %>% 
                     dplyr::filter(key == "lights") %>% 
                     rename(`D_followupquestions2-D8_lightingtype` = num),
                   by = "D_followupquestions2-D8_lightingtype") %>% 
  dplyr::rename(D8_lightingtype = description) %>% 
  dplyr::select(-key,-`D_followupquestions2-D8_lightingtype`)




odkend_multiple_activities = as.data.frame(rbindlist(
  #Import a bunch of variables saved separately.
  lapply( 
    list.files("~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/MNCH_Instrumentend.csv/",full.names = TRUE) %>% 
      grep("MNCH_InstrumentEnd-", ., ignore.case = TRUE, value = TRUE),
    function(x) read_csv(x)),
  fill=TRUE)) %>% 
  dplyr::rename(question = KEY,
                KEY = PARENT_KEY,
                K3_endtime = K4_starttime,
                K3_starttime = K3_startdate,
                K2_startdate = K2_date) %>% 
  dplyr::mutate(question = gsub("^(.+?)/","",question)) %>% 
  tidyr::unite("end_time",c("E1.4_endtime","F1.4_endtime","G1.4_endtime","H1.4_endtime",
                            "I1.4_endtime","K3_endtime","L1.4_endtime","M1.4_endtime"),sep = "", remove = F, na.rm = TRUE) %>% 
  tidyr::unite("start_time",c("E1.3_starttime","F1.3_starttime","G1.3_starttime","H1.3_starttime",
                              "I1.3_starttime","K3_starttime","L1.3_starttime", "M1.3_starttime"),sep = "", remove = F, na.rm = TRUE) %>% 
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
  dplyr::select(where(not_all_na)) %>% 
  dplyr::mutate(stovetype = case_when(question %like% "firewood" ~ "Wood_stove",
                                      question %like% "charcoal" ~ "Charcoal_stove",
                                      question %like% "kerosene" ~ "Kerosene_stove",
                                      question %like% "gas" ~ "LPG_stove",
                                      question %like% "mosquito" ~ "Mosquito_coil",
                                      question %like% "incense" ~ "Incense",
                                      TRUE ~ "")) %>% 
  tidyr::unite("K5_activities",c("K5_activities","stovetype"),sep = "") 



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

#Transport portion of odkend
odkend_transport <- rbind(odkend_transport1,odkend_transport2) %>% 
  tidyr::pivot_longer(cols = K6_transportation:K9_childwith) %>% 
  separate_rows(value,sep = " ") %>% 
  dplyr::mutate(value = gsub(pattern = " ","",value)) %>% 
  dplyr::filter(complete.cases(.),! value == "") 

odkend_transport_review <- rbind(odkend_transport1,odkend_transport2) #Wide version for reviewing.



# Save data ---------------------------------------------------------------
## Prepare data frames for output/saving

saveRDS(odkstart,'Results/processed data/odkstart_processed.RDS')
write_xlsx(odkstart,'Results/processed data/odkstart_processed.xlsx')


# 
# #Survey portion of odkend
#Not working right now because the types were changed (yn are factors, others are numeric).  But we only used this to review, so continue without.
# odkend_survey <- odkend %>%   
#   dplyr::select(-starts_with("J_"),-ends_with("InsertID")) %>% 
#   tidyr::unite(`D_followupquestions-D4_problemsdetailed`,`D_followupquestions-D4_problemsdetailed_other`,sep = " ", remove = F, na.rm = TRUE) %>%
#   dplyr::mutate(`D_followupquestions-D4_problemsdetailed` = gsub("other","",`D_followupquestions-D4_problemsdetailed`)) %>% 
#   tidyr::pivot_longer(cols = `B_AmbientEnd-B2_problemsdetails`:`M_incense-M1_incensecoilyn`) %>% 
#   dplyr::mutate(description = case_when(name %like% "yn|backpackon" & value == 1 ~ "yes",
#                                         name %like% "yn|backpackon" & value == 2 ~ "no",
#                                         name %like% "lightingtype" & value == 1 ~ "Electric/battery/solar",
#                                         name %like% "lightingtype" & value == 2 ~ "Kerosene/covered",
#                                         name %like% "lightingtype" & value == 3 ~ "Kerosene/tin wick",
#                                         name %like% "lightingtype" & value == 4 ~ "Candle",
#                                         name %like% "insideoutside" & value == 1 ~ "Inside",
#                                         name %like% "insideoutside" & value == 2 ~ "Outside",
#                                         name %like% "whynotworn" & value == 1 ~ "Out in public",
#                                         name %like% "whynotworn" & value == 2 ~ "School/playground",
#                                         name %like% "whynotworn" & value == 3 ~ "Household",
#                                         name %like% "whynotworn" & value == 4 ~ "Workplace",
#                                         TRUE ~ value)) %>% 
#   dplyr::select(-value,-rcodef,-`B_AmbientEnd-B0_gps-Accuracy`,-`B_AmbientEnd-B0_gps-Altitude`) %>% 
#   tidyr::pivot_wider(names_from = name,values_from = description)
# 
# saveRDS(odkend_survey,'Results/processed data/odkend_survey_processed.RDS')
# write_xlsx(odkend_survey,'Results/processed data/odkend_survey_processed.xlsx')


# odkend_transport_save <- odkend_transport %>% 
#   dplyr::filter(name == "K6_transportation") %>% 
#   dplyr::select(-K5_activities) %>% 
#   dplyr::full_join(choices_odk %>% 
#                      dplyr::filter(key == "transport") %>% 
#                      rename(value = num),
#                    by = "value") %>% 
#   dplyr::select(-name,-key,-value) %>% 
#   dplyr::select(KEY,description,datetime_start,datetime_end) %>% 
#   dplyr::distinct() %>% 
#   dplyr::left_join(odkend_survey %>%
#                      dplyr::select(KEY, survey_datetime_end, hhid),
#                    by = "KEY")
# 
# saveRDS(odkend_transport_save,'Results/processed data/odkend_transport_processed.RDS')
# write_xlsx(odkend_transport_save,'Results/processed data/odkend_transport_processed.xlsx')


#Save activities data
# odkend_time_activities <- rbind.fill(odkend_multiple_activities %>%  #Has smoke sources and stove activity
#                                        separate_rows(K5_activities,sep = " ") %>% 
#                                        separate_rows(K8_smokefumes,sep = " ") %>% 
#                                        dplyr::mutate(smoke_sources = case_when(
#                                          K8_smokefumes %like% "1"  ~ "Cookstove",
#                                          K8_smokefumes %like% "2"  ~ "Trash burning",
#                                          K8_smokefumes  %like%  "3"  ~ "Kerosene lamp",
#                                          K8_smokefumes  %like%  "4"  ~ "Traffic",
#                                          K8_smokefumes  %like%  "5"  ~ "Cigarettes",
#                                          K8_smokefumes  %like%  "6"  ~ "Candles",
#                                          K8_smokefumes  %like%  "7"  ~ "Generator",
#                                          K8_smokefumes  %like%  "8"  ~ "Smoke from neighbor's house",
#                                          K8_smokefumes  %like%  "9"  ~ "Incense",
#                                          K8_smokefumes  %like%  "99"  ~ "None",
#                                          K8_smokefumes  %like%  "Other"  ~ "Other",
#                                          TRUE ~ "")) %>% 
#                                        dplyr::select(KEY,K5_activities,smoke_sources,datetime_start,datetime_end),
#                                      odkend_transport %>%  
#                                        dplyr::select(KEY,K5_activities,datetime_start,datetime_end) %>% 
#                                        separate_rows(K5_activities,sep = " ") %>% 
#                                        dplyr::distinct() %>% 
#                                        dplyr::filter(!is.na(K5_activities),
#                                                      ! K5_activities == "")
# ) %>% 
#   dplyr::full_join(choices_odk %>% 
#                      dplyr::filter(key == "activity") %>% 
#                      dplyr::rename(K5_activities = num),
#                    by = "K5_activities") %>% 
#   dplyr::mutate(description = case_when(is.na(description)~K5_activities,
#                                         TRUE ~ description)) %>% 
#   dplyr::select(KEY,description,smoke_sources,datetime_start,datetime_end) %>%
#   dplyr::distinct() %>% 
#   dplyr::left_join(odkend_survey %>%
#                      dplyr::select(KEY, survey_datetime_end, hhid),
#                    by = "KEY") %>% 
#   dplyr::filter(description != "")
# 
# saveRDS(odkend_time_activities,'Results/processed data/odkend_time_activities_processed.RDS')
# write_xlsx(odkend_time_activities,'Results/processed data/odkend_time_activities_processed.xlsx')
# 

