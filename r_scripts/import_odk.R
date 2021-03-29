

#R. Piedrahita 07-01-2020
#Code to ingest ODK data 
library(data.table)
library(readr)

start_path = "~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/MNCH_InstrumentStart.csv/MNCH_InstrumentStart.csv"
start_path_b = "~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/MNCH_InstrumentStart.csv/MNCH_InstrumentStart-D_householdmembersrepeat.csv"

odkstart_b = read_csv(start_path_b,
                      col_names = TRUE,
                      col_types = cols(.default = col_character())) %>% 
  dplyr::select(-KEY) %>%
  pivot_wider(names_from = c(1), values_from = c(3:ncol(.)-1)) %>% 
  rename(KEY = PARENT_KEY)

odkstart = read_csv(start_path,
                    col_names = TRUE,
                    col_types = cols(.default = col_character())) %>% 
  left_join(odkstart_b,by = "KEY")



end_path = "~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/MNCH_Instrumentend.csv/MNCH_Instrumentend.csv"

odkend = read_csv(end_path,
                  col_names = TRUE,
                  col_types = cols(.default = col_character())) 

end_paths = list.files("~/Dropbox/Jacaranda Kenya Field Folder/Data/ODK Data/MNCH_Instrumentend.csv/",full.names = TRUE) %>% 
  grep("MNCH_InstrumentEnd-", ., ignore.case = TRUE, value = TRUE)

end_variable = as.data.frame(
  rbindlist(
    lapply(
      end_paths,
      function(x) read_csv(x) %>% 
        dplyr::select(-KEY) %>%
        gather(key,value,-PARENT_KEY,-c(1))),
    fill=TRUE))  %>% 
  unite("idx",c(E1.1_pos_index,F1.1_pos_index,G1.1_pos_index,H1.1_pos_index,I1.1_pos_index,K_pos_index,L1.1_pos_index,M1.1_pos_index),
        sep = ",", remove = FALSE,na.rm = TRUE) %>% 
  dplyr::select(idx,KEY,key,value) %>% 
  spread(key,value)

odkend_b = read_csv(end_path_b,
                    col_names = TRUE,
                    col_types = cols(.default = col_character())) %>% 
  dplyr::select(-PARENT_KEY) %>%
  pivot_wider(names_from = hh_pos_index, values_from = c(D_membersex,D_memberageyears,D_memberagemonths))




saveRDS(meta_merged,'processed data/metadata_merged.RDS')
