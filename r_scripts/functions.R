
plot_deployment_merged <- function(all_merged_temp){
  # all_merged_temp <- all_merged[HHID == uniqueHHIDs[i]]
  tryCatch({
    
    all_merged_temp_ecm <- pivot_longer(all_merged_temp,
                                        cols = starts_with("PM25"),
                                        names_to = "PM25",
                                        names_prefix = "PM25",
                                        values_to = "values",
                                        values_drop_na = TRUE)
    
    p1pm <- all_merged_temp_ecm %>% filter(PM25 == 'Cook' | PM25 == 'Kitchen') %>%
      ggplot(aes(y = values, x = datetime)) + 
      geom_point(aes(colour = PM25), alpha=0.25) + 
      theme_bw(10) +
      theme(legend.title=element_blank(),axis.title.x = element_blank()) +
      scale_y_continuous(limits = c(0,1000))   +
      # theme(axis.text.x = element_text(angle = 30, hjust = 1,size=10))+
      ggtitle(all_merged_temp$HHID[1]) + 
      ylab("ECM ugm-3") 
    
    p2pm <- pivot_longer(all_merged_temp,
                         cols = starts_with("PATS"),
                         names_to = "PATS",
                         names_prefix = "PATS",
                         values_to = "values",
                         values_drop_na = TRUE) %>%
      mutate(PATS = gsub('_',' ',PATS)) %>%
      ggplot(aes(y = values, x = datetime)) +
      geom_point(aes(colour = PATS), alpha=0.25) +
      theme_bw(10) +
      theme(legend.title=element_blank(),axis.title.x = element_blank()) +
      scale_y_continuous(limits = c(10,1000)) +
      # theme(axis.text.x = element_text(angle = 30, hjust = 1,size=10))+
      ylab("PATS ugm-3")
    
    p3pm <- all_merged_temp_ecm %>% filter(PM25 != 'Cook' & PM25 != 'Kitchen') %>%
      ggplot(aes(y = values, x = datetime)) + 
      geom_point(aes(colour = PM25), alpha=0.25) + 
      theme_bw(10) +
      theme(legend.title=element_blank(),axis.title.x = element_blank()) +
      scale_y_continuous(limits = c(0,1000))   +
      # theme(axis.text.x = element_text(angle = 30, hjust = 1,size=10))+
      ggtitle(all_merged_temp$HHID[1]) + 
      ylab("Indirect ugm-3") 
    
    all_merged_temp_beacon <- pivot_longer(all_merged_temp,
                                           cols = starts_with("location"),
                                           names_to = "location",
                                           names_prefix = "location",
                                           values_to = "values",  
                                           values_drop_na = TRUE) %>%
      mutate(location = gsub('_',' ',location),
             locationvalues = location)
    levels(all_merged_temp_beacon$locationvalues) = 1:length(unique(all_merged_temp_beacon$locationvalues))
    
    p4 <- ggplot(aes(y = locationvalues, x = datetime), data = all_merged_temp_beacon) +
      geom_tile(aes(colour = values,fill=values ), alpha=0.25) +
      theme_bw(10) +
      theme(legend.title=element_blank(),axis.title.x = element_blank()) +
      # theme(axis.text.x = element_text(angle = 30, hjust = 1,size=10))+
      ylab("localization by approach")
    
    
    all_merged_temp_sums <- pivot_longer(all_merged_temp,
                                         cols = starts_with("sums"),
                                         names_to = "sums",
                                         names_prefix = "sums",
                                         values_to = "values",
                                         values_drop_na = TRUE) %>%
      mutate(sums = gsub('_',' ',sums),
             sumsvalues = sums)
    levels(all_merged_temp_sums$sumsvalues) = 1:length(unique(all_merged_temp_sums$sumsvalues))
    
    p5 <- ggplot(aes(y = as.factor(sums), x = datetime), data = all_merged_temp_sums) +
      geom_tile(aes(colour = values,fill=values), alpha=0.25) +
      theme_set(theme_bw(10) + theme(legend.background=element_blank())) +
      theme(legend.background=element_blank()) +
      
      theme(legend.title=element_blank(),axis.title.x = element_blank()) +
      # scale_y_continuous(limits = c(20,100)) +
      # theme(axis.text.x = element_text(angle = 30, hjust = 1,size=10))+
      ylab("Cooking indicator")
    
    
    
    plot_name = paste0("QA Reports/Instrument Plots/all_merged_pm_",all_merged_temp$HHID[1],"_",all_merged_temp$Date[1],".png")
    png(plot_name,width = 1000, height = 800, units = "px")
    egg::ggarrange(p1pm, p2pm,p3pm,p4,p5, heights = c(0.2,0.2, 0.2,.2,.2))
    dev.off()
    
    
    #### Create CO plot
    all_merged_temp_co <- pivot_longer(all_merged_temp,
                                       cols = starts_with("CO"),
                                       names_to = "CO",
                                       names_prefix = "CO",
                                       values_to = "values",
                                       values_drop_na = TRUE) %>%
      mutate(CO = gsub('_','',CO)) %>%
      mutate(CO = gsub('ppm','',CO)) 
    
    
    p1co <- all_merged_temp_co %>% filter(CO == 'Cook' | CO == 'Kitchen' | CO == 'LivingRoom') %>%
      ggplot(aes(y = values, x = datetime)) + 
      geom_point(aes(colour = CO), alpha=0.25) + 
      theme_bw(10) +
      theme(legend.title=element_blank(),axis.title.x = element_blank()) +
      scale_y_continuous(limits = c(0,100))   +
      # theme(axis.text.x = element_text(angle = 30, hjust = 1,size=10))+
      ggtitle(all_merged_temp$HHID[1]) + 
      ylab("CO ppm") 
    
    
    p2co <- all_merged_temp_co %>% filter(CO != 'Cook' & CO != 'Kitchen' & CO != 'LivingRoom') %>%
      ggplot(aes(y = values, x = datetime)) + 
      geom_point(aes(colour = CO), alpha=0.25) + 
      theme_bw(10) +
      theme(legend.title=element_blank(),axis.title.x = element_blank()) +
      scale_y_continuous(limits = c(0,100))   +
      # theme(axis.text.x = element_text(angle = 30, hjust = 1,size=10))+
      ggtitle(all_merged_temp$HHID[1]) + 
      ylab("CO ppm indirect") 
    
    plot_name = paste0("QA Reports/Instrument Plots/all_merged_co_",all_merged_temp$HHID[1],".png")
    png(plot_name,width = 1000, height = 800, units = "px")
    egg::ggarrange(p1co, p2co,p4,p5, heights = c(0.25,0.25, 0.25,.25))
    dev.off()
    
    # }
  }, error = function(error_condition) {
    print(paste0('Errore in HHID ', HHIDselected,', index ',i))
  }
  , finally={})
}


parse_filename_fun <- function(file){
  tryCatch({ 
    filename = data.table()
    filename$fullname = gsub(' ','',file) 
    filename$fullname = gsub('.txt','',filename$fullname)
    filename$fullname = gsub('.csv','',filename$fullname)
    filename$datestart = ymd(strsplit(basename(filename$fullname), "_")[[1]][1])
    filename$basename = basename(filename$fullname)
    filename$basename_sansext = file_path_sans_ext(filename$basename)
    filename$sampleID = strsplit(filename$basename_sansext , "_")[[1]][2]
    filename$loggerID = strsplit(filename$basename_sansext , "_")[[1]][3]
    filename$sampletype= strsplit(basename(filename$sampleID), "-")[[1]][3]
    filename$fieldworkerIDstr = strsplit(basename(filename$sampleID), "-")[[1]][2]
    filename$HHIDstr = strsplit(basename(filename$sampleID), "-")[[1]][1]
    matches <- regmatches(filename$HHIDstr, gregexpr("[[:digit:]]+", filename$HHIDstr))
    filename$HHID =  as.numeric(matches)
    filename$flag = strsplit(filename$basename_sansext , "_")[[1]][5]
    if(is.na(filename$flag)) {filename$flag = c("good")}
    num_underscores <- lengths(regmatches(filename$basename, gregexpr("_", filename$basename)))
    if ( num_underscores > 3 |  (is.na(filename$filterID) & num_underscores>3) | is.na(filename$HHID) | filename$fieldworkerID > 25 |
         filename$fieldworkerID < 0) {filename$filename_flag = 1}else{filename$filename_flag = 0}
    return(filename)
  }, error = function(e) {
    print('Error parsing filename')
    print(file)
    return(NULL)
  })  
}

