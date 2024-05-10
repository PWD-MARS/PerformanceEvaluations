### Script to generate visuals of water level at CS1, OW1 and OW2, SMP-ID = 61369
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
smp_id_df <- data.frame(smp_id = "61369")


folder <- "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\59 Performance Evaluation of Private Sites\\61369"
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
  filter(ow_suffix %in% c("CS1", "SW1")) %>%
  filter(eventdatastart_edt > as.Date("2017-07-20")) %>%
  mutate(percent_peak = NA)
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
 
    storage_depth <- 1.5
    #orifice_height_ft <- snapshot$assumption_orificeheight_ft
    
    monitoringdata <- marsFetchMonitoringData(con = con, 
                                              target_id = temp_df$smp_id, 
                                              ow_suffix = temp_df$ow_suffix, 
                                              source = "gage",
                                              start_date = as.character(rain_start_date), 
                                              end_date = as.character(rain_end_date), 
                                              sump_correct = FALSE,
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

    
    if(length(selected_event$level_ft) == 0){
      parent_df[i, "percent_peak"] <- -1000
    } else {
      parent_df[i, "percent_peak"] <- marsPeakStorage_percent(selected_event$level_ft, storage_depth)
      
    }

    write.table(parent_df, file =  "overtop.csv", sep = ",")
    

    
  }, error=function(e){
    error_log[1,1] <<- i
    error_log[1,2] <<- toString(conditionMessage(e))
    write.table(error_log, file = paste0(folder, "/", current_date, "/error_log.csv"), sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
  }
  )
}




#read data 
parent_df <- read.csv("//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/59 Performance Evaluation of Private Sites/61369/2024-05-07/percent use.csv")

# SW1
plot_dt <- parent_df %>% 
  filter(percent_peak != -1000 & ow_suffix == "SW1") 

plot_dt$eventdatastart_edt <-as.Date(plot_dt$eventdatastart_edt)

plot_storage_sw1 <- ggplot(plot_dt, aes(x = eventdatastart_edt, y= percent_peak)) +
  geom_point(aes(color = eventdepth_in), size = 5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Date of Event", y = "Percent Storage Used") +
  scale_x_date(date_labels="%b-%y",date_breaks  ="6 month") +
  scale_colour_gradient2() 

plot_depth_sw1 <- ggplot(plot_dt, aes(x = eventdatastart_edt, y= eventdepth_in)) +
  geom_point(aes(color = percent_peak), size = 5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Date of Event", y = "Rain Depth (in)") +
  scale_x_date(date_labels="%b-%y",date_breaks  ="6 month") +
  geom_hline(yintercept= 1.5, color = "brown", size=2, linetype="dashed") +
  geom_vline(xintercept= as.Date('2022-06-01'), color = "purple", size=2, linetype="dashed") +
  ggtitle("SW1 (61369)- From 2017-07-22 To 2023-11-01") +
  annotate("text", x = as.Date('2018-03-01'), y = 1.7, label = 'Design Storm: 1.5"', size = 20/.pt) +
  annotate("text", x = as.Date('2021-06-01'), y = 4, label = 'Pipe Change Occured on June 2022', size = 20/.pt) +
  scale_colour_gradient2()

# CS1
plot_dt <- parent_df %>% 
  filter(percent_peak != -1000 & ow_suffix == "CS1") 

plot_dt$eventdatastart_edt <-as.Date(plot_dt$eventdatastart_edt)

plot_storage_cs1 <- ggplot(plot_dt, aes(x = eventdatastart_edt, y= percent_peak)) +
  geom_point(aes(color = eventdepth_in), size = 5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Date of Event", y = "Percent Storage Used") +
  scale_x_date(date_labels="%b-%y",date_breaks  ="6 month") +
  scale_colour_gradient2()

plot_depth_cs1 <- ggplot(plot_dt, aes(x = eventdatastart_edt, y= eventdepth_in)) +
  geom_point(aes(color = percent_peak), size = 5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Date of Event", y = "Rain Depth (in)") +
  scale_x_date(date_labels="%b-%y",date_breaks  ="6 month") +
  geom_hline(yintercept= 1.5, color = "brown", size=2, linetype="dashed") +
  geom_vline(xintercept= as.Date('2022-06-01'), color = "purple", size=2, linetype="dashed") +
  ggtitle("CS1 (61369)- From 2017-07-22 To 2023-11-01") +
  annotate("text", x = as.Date('2018-03-01'), y = 1.7, label = 'Design Storm: 1.5"', size = 20/.pt) +
  annotate("text", x = as.Date('2021-06-01'), y = 4, label = 'Pipe Change Occured on June 2022', size = 20/.pt) +
  scale_colour_gradient2()

