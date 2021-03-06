

######Plot up some trends/time series
plot_pa_with_gps <- function(pa_data_temp){
  
  plot_name = paste0("~/Dropbox/Jacaranda Kenya Field Folder/Data/Plots/PurpleAir/pa_gps_",
                     file_path_sans_ext(basename(pa_data_temp$filename.x[1])),".png")
  
  if(!file.exists(plot_name)){
    #Raw both sensosrs pm 2.5 by site
    p1 = ggplot(pa_data_temp)+
      # geom_point(aes(x=local_time, y=pm2_5_cf_1_ave, color= "green"))+
      geom_point(aes(x=local_time, y=pm25_bam_ave, color= "blue"))+
      scale_color_discrete(name = "Sensor", labels = c("Mean bam"))+#,"Mean Uncorrected"))+
      labs(title = "PurpleAir")
    
    p3 = ggplot(pa_data_temp) +
      geom_point(aes(x=local_time, y=lat, color= "blue")) +
      geom_point(aes(x=local_time, y=lon, color= "green")) +
      scale_color_discrete(name = "Sensor", labels = c("lat", "lon")) 
    
    p2 = ggplot(pa_data_temp) +
      geom_point(aes(x=local_time, y=current_temp_f, color= "blue")) +
      geom_point(aes(x=local_time, y=current_humidity, color= "green")) +
      scale_color_discrete(name = "Sensor", labels = c("T", "RH")) 
    
    png(plot_name,width = 1000, height = 800, units = "px")
    egg::ggarrange(p1, p2, heights = c(0.5,0.5))
    dev.off()
  }
}


#difference of sensor a and b by site
plot_files <- function(pa_data_temp){
  
  plot_name = paste0("~/Dropbox/Jacaranda Kenya Field Folder/Data/Plots/PurpleAir/pa_",
                     file_path_sans_ext(basename(pa_data_temp$filename[1])),".png")
  
  if(!file.exists(plot_name)){
    #Raw both sensosrs pm 2.5 by site
    p1 = ggplot(pa_data_temp)+
      geom_point(aes(x=local_time, y=pm25_bam_ave, color= "blue"))+
      geom_point(aes(x=local_time, y=pm2_5_cf_1_ave, color= "green"))+
      scale_color_discrete(name = "Sensor", labels = c("Mean Uncorrected", "Mean bam"))+
      labs(title = "PurpleAir")+
      facet_wrap(.~instrument_id)
    
    p2 = ggplot(pa_data_temp) +
      geom_point(aes(x=local_time, y=current_temp_f, color= "blue")) +
      geom_point(aes(x=local_time, y=current_humidity, color= "green")) +
      scale_color_discrete(name = "Sensor", labels = c("T", "RH")) + 
      labs(title = paste0("PA ",pa_data_temp$filename[1]))
    
    png(plot_name,width = 1000, height = 800, units = "px")
    egg::ggarrange(p1, p2, heights = c(0.6,0.4))
    dev.off()
  }
}


#difference of sensor a and b by site
# pa_data_temp <- all_merged[1:1000,]
plot_pa_with_activities <- function(pa_data_temp,odkend_transport_processed_mm,odkend_time_activities){
  
  odkend_time_activities_temp <- odkend_time_activities %>%
    dplyr::filter(hhid == pa_data_temp$hhid[1]) %>% 
    distinct()
  
  odkend_transport_temp <- odkend_transport_processed_mm %>% 
    dplyr::filter(hhid == pa_data_temp$hhid[1])
  
  plot_name = paste0("~/Dropbox/Jacaranda Kenya Field Folder/Data/Plots/PurpleAir/pa_transport_",
                     file_path_sans_ext(basename(pa_data_temp$filename.x[1])),".png")
  
  if(!file.exists(plot_name)){
    #Raw both sensosrs pm 2.5 by site
    p1 = ggplot(pa_data_temp)+
      # geom_point(aes(x=local_time, y=pm2_5_cf_1_ave, color= "green"))+
      geom_point(aes(x=local_time, y=pm25_bam_ave, color= "blue"))+
      labs(title = "PurpleAir")+
      ylab("PM2.5")
    
    p2 <- odkend_time_activities_temp %>% 
      ggplot() +  
      geom_dumbbell(aes(y=activity_clean, x=datetime_start, xend=datetime_end),
                    size=1.5, color="#b2b2b2", size_x=3, size_xend = 3) + 
      xlim(c(min(pa_data_temp$local_time,na.rm = T),
             max(pa_data_temp$local_time,na.rm = T)))+
      ylab("Activity")
    
    p3 <- odkend_transport_temp %>% 
      ggplot() +  
      geom_dumbbell(aes(y=description, x=datetime_start_mod, xend=datetime_end_mod),
                    size=1.5, color="#b2b2b2", size_x=3, size_xend = 3) + 
      xlim(c(min(pa_data_temp$local_time,na.rm = T),
             max(pa_data_temp$local_time,na.rm = T))) + 
      ylab("Transportation")
    
    p4 = ggplot(pa_data_temp) +
      geom_point(aes(x=local_time, y=lat)) +
      # geom_point(aes(x=local_time, y=lon, color= "green")) +
      scale_color_discrete(name = "Sensor", labels = c("lat"))+#, "lon")) 
      xlim(c(min(pa_data_temp$local_time,na.rm = T),
             max(pa_data_temp$local_time,na.rm = T))) 
    
    
    png(plot_name,width = 1000, height = 800, units = "px")
    egg::ggarrange(p1, p2,p3,p4, heights = c(0.4,.2,.2,.2))
    dev.off()
  }
}

#difference of sensor a and b by site
plot_gps_files <- function(gps_data_temp){
  
  plot_name = paste0("~/Dropbox/Jacaranda Kenya Field Folder/Data/Plots/GPS/gps_",
                     file_path_sans_ext(basename(gps_data_temp$filename[1])),".png")
  if(!file.exists(plot_name)){
    
    #Raw both sensosrs pm 2.5 by site
    p1 = ggplot(gps_data_temp)+
      geom_point(aes(x=local_time, y=lat, color= "blue"))+
      labs(title = "GPS lat value over time")+
      facet_wrap(.~instrument_id)
    
    p2 = ggplot(gps_data_temp) +
      geom_point(aes(x=local_time, y=lon, color= "blue"))+
      labs(title = "GPS lon value over time")+
      facet_wrap(.~instrument_id)
    
    
    png(plot_name,width = 1000, height = 800, units = "px")
    egg::ggarrange(p1, p2, heights = c(0.6,0.4))
    dev.off()
  }
}

# gps_data_temp <- gps_data[1:1000,]
plotly_gps_files <- function(gps_data_temp){
  
  plot_name = paste0("~/Dropbox/Jacaranda Kenya Field Folder/Data/Plots/GPS/gps_",
                     file_path_sans_ext(basename(gps_data_temp$filename[1])))
  # if(!file.exists(plot_name)){
  
  p1 <- plot_ly(gps_data_temp, x = ~local_time, y = ~lat, text = paste("Lat: ", gps_data_temp$lat),mode = "lines+markers",name = "Latitude over time")  
  p2 <- plot_ly(gps_data_temp, x = ~local_time, y = ~lon, text = paste("Lon: ", gps_data_temp$lon),mode = "lines+markers",name = "Longitude over time")
  p3 <- plot_ly(gps_data_temp, x = ~lat, y = ~lon,text = gps_data_temp$local_time,mode = "lines+markers",name = "Latitude vs. Longitude")
  
  fig <- subplot(p1, p2,p3,nrows = 3)
  htmlwidgets::saveWidget(as_widget(fig), paste0(plot_name,".html"))
  
  # }
}


plot_deployment <- function(selected_preplacement,beacon_logger_data,pats_data_timeseries,
                            CO_calibrated_timeseries,tsi_timeseries,ecm_dot_data){
  
  tryCatch({
    # selected_preplacement <- preplacement[i,]
    
    HHIDselected <- selected_preplacement$HHIDnumeric
    plot_name = paste0("~/Dropbox/Jacaranda Kenya Field Folder/Data/Plots/Combo_",HHIDselected,".png")
    
    if(!file.file.exists(plot_name)){
      
      mindatetime <- selected_preplacement$start_datetime
      maxdatetime <- mindatetime + 24*60*60
      
      #Use ECM start and stop time to truncate the data.
      maxdatetimeCO <- CO_calibrated_timeseries[CO_ppm>-1 & HHID %in% HHIDselected,lapply(.SD,max),.SDcols="datetime"]
      
      selected_COppm <- CO_calibrated_timeseries[CO_ppm>-1 & HHID %in% HHIDselected,c("CO_ppm","datetime","sampletype","emission_tags","qc")]
      selected_COppm<-sampletype_fix_function(selected_COppm)
      
      selected_pats <- pats_data_timeseries[HHID %in% HHIDselected,c("pm25_conc","datetime","sampletype","emission_tags","qc")]
      selected_pats<-sampletype_fix_function(selected_pats)
      
      selected_ecm <- ecm_dot_data[HHID %in% HHIDselected,c("pm25_conc","datetime","sampletype")]
      
      selected_beacon <- beacon_logger_data[HHID %in% HHIDselected,c("location_nearest","location_kitchen_threshold","datetime","nearest_RSSI")]
      # selected_beacon<-sampletype_fix_function(selected_beacon)
      
      selected_tsi <- tsi_timeseries[HHID %in% HHIDselected,c("datetime","loggerID","HHID","qc","emission_tags","CO_ppm","CO2_ppm")]
      
      p1 <- ggplot(aes(y = CO_ppm, x = datetime), data = selected_COppm) + 
        geom_point(aes(colour = sampletype, shape = qc), alpha=0.25) + 
        theme_bw(10) +
        theme(legend.title=element_blank(),axis.title.x = element_blank()) +
        scale_x_datetime(limits=c(mindatetime,maxdatetime)) +
        scale_y_continuous(limits = c(0,300))   +
        ggtitle(paste0("HHID KE",HHIDselected)) + 
        ylab("CO ppm") 
      
      p2 <- ggplot(aes(y = pm25_conc, x = datetime), data = selected_pats) +
        geom_point(aes(colour = sampletype, shape = qc), alpha=0.25) +
        theme_bw(10) +
        theme(legend.title=element_blank(),axis.title.x = element_blank()) +
        scale_x_datetime(limits=c(mindatetime,maxdatetime)) +
        scale_y_continuous(limits = c(0,3000)) +
        ylab("PATS+ ugm-3")
      
      p3 <- ggplot(aes(y = pm25_conc, x = datetime), data = selected_ecm) +
        geom_point(aes(colour = sampletype, shape = qc), alpha=0.25) +
        theme_bw(10) +
        scale_x_datetime(limits=c(mindatetime,maxdatetime)) +
        theme(legend.title=element_blank(),axis.title.x = element_blank()) +
        scale_y_continuous(limits = c(0,1000)) +
        ylab("ECM/MicroPEM ugm-3")
      
      p4 <- ggplot(aes(y = nearest_RSSI, x = datetime), data = selected_beacon) +
        geom_point(aes(colour = location_nearest), alpha=0.25)+
        theme_bw(10) +
        theme(legend.title=element_blank(),axis.title.x = element_blank()) +
        scale_x_datetime(limits=c(mindatetime,maxdatetime)) +
        # scale_y_continuous(limits = c(0,3000)) +
        ylab("Localization")
      
      p5 <- ggplot(data = selected_tsi,aes(y = CO_ppm, x = datetime, shape = emission_tags, colour = emission_tags,group = qc), alpha=0.25) +
        geom_point()+
        theme_bw(10) +
        scale_y_log10() +
        theme(legend.title=element_blank(),axis.title.x = element_blank()) +
        scale_x_datetime(limits=c(mindatetime,maxdatetime)) +
        ylab("TSI CO2 and CO ppm")
      
      png(plot_name,width = 1000, height = 800, units = "px")
      egg::ggarrange(p1, p2,p4,p5, heights = c(0.25, 0.25,.25,.25))
      
      dev.off()
    }
  }, error = function(error_condition) {
    print(paste0('Errore in HHID ', HHIDselected,', index ',i))
  }
  , finally={})
}


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
    
    
    
    plot_name = paste0("~/Dropbox/Jacaranda Kenya Field Folder/Data/Plots/all_merged_pm_",all_merged_temp$HHID[1],"_",all_merged_temp$Date[1],".png")
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
    
    plot_name = paste0("~/Dropbox/Jacaranda Kenya Field Folder/Data/Plots/all_merged_co_",all_merged_temp$HHID[1],".png")
    png(plot_name,width = 1000, height = 800, units = "px")
    egg::ggarrange(p1co, p2co,p4,p5, heights = c(0.25,0.25, 0.25,.25))
    dev.off()
    
    # }
  }, error = function(error_condition) {
    print(paste0('Errore in HHID ', HHIDselected,', index ',i))
  }
  , finally={})
}

give.n <- function(x){return(c(y = 0, label = length(x)))}
give.n5 <- function(x){return(c(y = 5, label = length(x)))}

give.mean <- function(x){return(c(y =mean(x)+.1, label = round(mean(x),digits=1)))}
give.mean5 <- function(x){return(c(y =mean(x)+5, label = round(mean(x),digits=1)))}
give.mean10 <- function(x){return(c(y =mean(x)+15, label = round(mean(x),digits=1)))}
#To make box and whiskers quantiles rather than IQRs.
f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# pa_data_temp <- all_merged %>% dplyr::filter(!hhid %like% "Ambient")
plot_by_filedeployment <- function(pa_data_temp,pa_ambient) {
  tryCatch({
    plot_name = paste0("~/Dropbox/Jacaranda Kenya Field Folder/Data/Plots/HHs with Amb/hhid_amb_",pa_data_temp$hhid[1],".png")
    
    if(!file.exists(plot_name)){
      
      p1 = pa_data_temp %>% 
        ggplot() +
        geom_point(aes(x=local_time, y=pm25_bam_ave,color=as.factor(home_flag)),alpha = .2) +
        # geom_smooth(aes(x=local_time, y=pm25_bam_ave),alpha = .2) +
        labs(title = paste0("PA for household: ",pa_data_temp$hhid[1]))+
        xlim(min(pa_data_temp$local_time),max(pa_data_temp$local_time))
      
      p2 = pa_ambient %>% 
        dplyr::filter(local_time > min(pa_data_temp$local_time,na.rm = T),
                      local_time < max(pa_data_temp$local_time,na.rm = T)) %>%
        ggplot() +
        geom_point(aes(x=local_time, y=pm25_bam_ave),alpha = .2) +
        # geom_line(aes(x=local_time, y=current_temp_f),alpha = .2) +
        labs(title = "Ambient")+
        xlim(min(pa_data_temp$local_time),max(pa_data_temp$local_time)) +
        ylim(0,max(pa_data_temp$pm25_bam_ave))
      
      
      egg::ggarrange(p1, p2, heights = c(0.5,0.5))
      png(plot_name,width = 1000, height = 800, units = "px")
      dev.off()
    }
  }, error = function(error_condition) {
    print(paste0('Errore in HHID ',pa_data_temp$hhid[1]))
  }
  , finally={})
}



