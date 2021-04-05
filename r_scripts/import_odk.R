

#R. Piedrahita 07-01-2020
#Code to ingest ODK data 
library(data.table)
library(tidyverse)
library(readr)

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
  dplyr::mutate(datetime_start = as.POSIXct(paste0(`A_samplingtype-A2_date`,`A_samplingtype-A2.1_time`)),
                hhid = `consentgiven-C_HHInfo-C1_hhid`) %>% 
  dplyr::select(-rcodef_ref,-rcodef_othersp,-`meta-instanceID`,-SubmitterID,-SubmitterName,-AttachmentsPresent,
                -AttachmentsExpected,-Status,-`consentgiven-C_HHInfo-C1_hhid`) %>% 
  setNames(gsub("[[:punct:]]","", names(.))) 


#Import ODK end files
odkend_multiple = as.data.frame(
  rbindlist(
    #Import a bunch of variables saved separately.
    lapply( 
      list.files("~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/MNCH_Instrumentend.csv/",full.names = TRUE) %>% 
        grep("MNCH_InstrumentEnd-", ., ignore.case = TRUE, value = TRUE),
      function(x) read_csv(x) %>% 
        dplyr::select(-KEY) %>%
        gather(key,value,-PARENT_KEY,-c(1))),
    fill=TRUE))  %>% 
  unite("idx",c(E1.1_pos_index,F1.1_pos_index,G1.1_pos_index,H1.1_pos_index,I1.1_pos_index,
                K_pos_index,L1.1_pos_index,M1.1_pos_index),
        sep = ",", remove = FALSE,na.rm = TRUE) %>% 
  dplyr::select(idx,PARENT_KEY,key,value) %>% 
  # spread(key,value) %>% 
  pivot_wider(names_from = c(idx,key), values_from = value) %>% 
  rename(KEY = PARENT_KEY)  



odkend = read_csv("~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/MNCH_Instrumentend.csv/MNCH_Instrumentend.csv",
                  col_names = TRUE,
                  col_types = cols(.default = col_character())) %>% 
  left_join(odkend_multiple,by = "KEY") %>% 
  dplyr::mutate(datetime_end = as.POSIXct(paste0(`A_samplingtype-A2_date`,`G_End-G1_endtime`))+86400,
                hhid = `C_SampleEnd-C0_hhid`) %>% 
  dplyr::select(-`A_samplingtype-A2_date`,-`A_samplingtype-A3_time`,-`G_End-G1_endtime`,-SubmissionDate,
                -AttachmentsPresent,-AttachmentsExpected,-Status,-`C_SampleEnd-err_hhid`,-KEY,-rcodef,
                -`C_SampleEnd-C0_hhid`,-`C_SampleEnd-C0_hhid_conf`,-A0_fieldId,-`A_samplingtype-A1_instruments`) %>% 
  setNames(gsub("[[:punct:]]","", names(.))) 


odkstartend <- left_join(odkstart,odkend,by = "hhid")
saveRDS(odkstart,'~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/odkstart.RDS')
saveRDS(odkend,'~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/odkend.RDS')
saveRDS(odkstartend,'~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/odkend.RDS')

