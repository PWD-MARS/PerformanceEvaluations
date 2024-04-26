### Script to generate visuals of water level at CS1, OW1 and OW2, SMP-ID = 61965
# 0.0 SETUP----

library(pwdgsi)
library(odbc)
library(lubridate)
library(tidyverse)
library(stats)
library(gridExtra)
library(grid)
library(gtable)
library(ggtext)
library(dplyr)
library(ggplot2)
library(pool)


#connection 
con <- dbPool(odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

# SMP IDs for performance eval
smp_id_df <- data.frame(smp_id = "61965")


folder <- "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\59 Performance Evaluation of Private Sites\\61965\\Plots"
current_date <- today()

dir.create(paste(folder, current_date, sep = "\\"), showWarnings = FALSE)

# get the data
ow_all <- dbGetQuery(con, "SELECT * from fieldwork.tbl_ow")
#ow_leveldata_raw <- dbGetQuery(con, "SELECT * FROM data.tbl_ow_leveldata_raw")
gage_event <- dbGetQuery(con, "SELECT * FROM data.tbl_gage_event")
smp_gage <- dbGetQuery(con, "SELECT * FROM admin.tbl_smp_gage")


# create a table with smp_id, ow_suffix, and rainevent_uid

parent_df <- smp_id_df %>%
  inner_join(ow_all, by="smp_id") %>%
  inner_join(smp_gage, by="smp_id") %>% 
  inner_join(gage_event, by= "gage_uid") %>%
  select(smp_id, ow_uid, ow_suffix, gage_event_uid, eventdatastart_edt, eventdepth_in, eventpeakintensity_inhr) %>%
  mutate(overtopping = NA) 

# recording errors
error_log <- data.frame(row = NA, error_message = NA, stringsAsFactors=FALSE)

#Warning: overwrites previous log files on the same day
write.table(error_log, file = paste0(folder, "\\", current_date, "\\error_log.csv"), sep = ",", append = FALSE, col.names = TRUE, row.names = FALSE)

# plot the hyetograph and water level combo for each storm and for each SMP from the public.shortcircuit_batch_bc

for (i in 1:nrow(parent_df)) {
  tryCatch({
    
    temp_df <- parent_df[i,]
    
    rain_start_date <- gage_event %>%
      filter(gage_event_uid == temp_df[1, "gage_event_uid"]) %>%
      select(eventdatastart_edt) %>%
      format("%Y-%m-%d") %>%
      pull
    
    rain_end_date <- gage_event %>%
      filter(gage_event_uid == temp_df[1, "gage_event_uid"]) %>%
      select(eventdataend_edt) %>%
      format("%Y-%m-%d") %>%
      pull
    
    event <- temp_df %>%
      select(gage_event_uid) %>%
      pull
    
    structure_name <- paste(temp_df$smp_id, temp_df$ow_suffix)
 
    storage_depth <- 3.43
    #orifice_height_ft <- snapshot$assumption_orificeheight_ft
    
    monitoringdata <- marsFetchMonitoringData(con = con, 
                                              target_id = temp_df$smp_id, 
                                              ow_suffix = temp_df$ow_suffix, 
                                              source = "gage",
                                              start_date = as.character(rain_start_date), 
                                              end_date = as.character(rain_end_date), 
                                              sump_correct = TRUE,
                                              debug = TRUE,
                                              level = TRUE)
    
    rain_event_data <- monitoringdata[["Rain Event Data"]]
    rain_data <- monitoringdata[["Rainfall Data"]]
    level_data <- monitoringdata[["Level Data"]]
    
    rain_plot_data <- monitoringdata[["Rainfall Data"]] %>%
      dplyr::filter(gage_event_uid == temp_df$gage_event_uid)
    
    rainfall_datetime	<- rain_plot_data$dtime_est
    rainfall_in <- rain_plot_data$rainfall_in
    
    obs_data <- dplyr::full_join(monitoringdata[["Level Data"]], monitoringdata[["Rainfall Data"]], 
                                 by = c("dtime_est", "gage_uid", "gage_event_uid")) %>% 
      dplyr::arrange(dtime_est) %>%
      dplyr::mutate(across(c("level_ft", "ow_uid"), ~ zoo::na.locf(., na.rm = FALSE))) %>%
      dplyr::mutate(across(c("level_ft", "ow_uid"), ~ zoo::na.locf(., fromLast = TRUE)))
    
    selected_event <- obs_data %>%
      dplyr::filter(gage_event_uid == temp_df$gage_event_uid)
    
    
    # 
    # plot <- marsCombinedPlot(event = event,
    #                          structure_name = structure_name,
    #                          storage_depth_ft = storage_depth,
    #                          obs_datetime = selected_event$dtime_est,
    #                          obs_level_ft = selected_event$level_ft,
    #                          rainfall_datetime = rainfall_datetime,
    #                          rainfall_in = rainfall_in
    # )
    # 
    # 
    # 
    # ggplot2::ggsave(paste0(folder,"\\" ,paste(temp_df$smp_id, temp_df$ow_suffix, temp_df$gage_event_uid, sep = "_"),".png"), plot = plot, width = 10, height = 8)
    # 
    # #Manually managing memory just in case
    # rm(plot)
    # rm(obs_data)
    # rm(level_data)

    
    #Draindown time
    # draindown_hr <- marsDraindown_hr(dtime_est = selected_event$dtime_est,
    #                                  rainfall_in = selected_event$rainfall_in,
    #                                  waterlevel_ft = selected_event$level_ft)
    
    #infiltration rate
    # infiltration_inhr <- marsInfiltrationRate_inhr(event = event,
    #                                                dtime_est = selected_event$dtime_est,
    #                                                rainfall_in = selected_event$rainfall_in,
    #                                                snapshot$dcia_ft2,
    #                                                snapshot$assumption_orificeheight_ft,
    #                                                snapshot$orifice_diam_in,
    #                                                storage_depth_ft = snapshot$storage_depth_ft,
    #                                                storage_vol_ft3 = snapshot$storage_volume_ft3,
    #                                                waterlevel_ft = selected_event$level_ft,
    #                                                depth_in = 6)
    # 
    #Observed relative storage utilization
    # percentstorageused_relative <- marsPeakStorage_percent(waterlevel_ft = selected_event$level_ft - dplyr::first(selected_event$level_ft), storage_depth_ft = snapshot$storage_depth_ft) %>% round(4) 
    
    #overtopping
    parent_df[i, "overtopping"] <- marsOvertoppingCheck_bool(selected_event$level_ft, storage_depth)

    write.table(parent_df, file =  "overtop.csv", sep = ",")
    

    
  }, error=function(e){
    error_log[1,1] <<- i
    error_log[1,2] <<- toString(conditionMessage(e))
    write.table(error_log, file = paste0(folder, "/", current_date, "/error_log.csv"), sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
  }
  )
}



