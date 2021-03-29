

#R. Piedrahita 07-01-2020
#Code to ingest ODK data 
library(data.table)
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
  left_join(odkstart_b,by = "KEY")



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
  left_join(odkend_multiple,by = "KEY")


saveRDS(odkstart,'~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/odkstart.RDS')
saveRDS(odkend,'~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/odkend.RDS')
