

#R. Piedrahita 07-01-2020
#Code to ingest ODK data 
library(data.table)
library(readr)
meta_start = read_csv("~/Dropbox/Jacaranda Kenya/Data Analysis/odk data/MNCH_InstrumentStart_v1.csv",
                                    col_types = cols(.default = col_character()))
meta_end = read_csv("~/Dropbox/Jacaranda Kenya/Data Analysis/odk data/MNCH_InstrumentEnd_v1.csv",
                                  col_types = cols(.default = col_character()))

meta_merged = left_join(meta_start,meta_end, by.x = "A0_HHID-A0_hhid", by.y = "C_SampleEnd-A0_hhid")
meta_merged$hhid = as.character(meta_merged$`A0_HHID-A0_hhid`)

saveRDS(meta_merged,'~/Dropbox/Jacaranda Kenya/Data Analysis/processed data/metadata_merged.RDS')
