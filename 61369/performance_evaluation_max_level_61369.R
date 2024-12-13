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
library(readr)
library(egg)
library(padr)
library(zoo)


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
  mutate(max_peak = NA)
# recording errors
error_log <- data.frame(row = NA, error_message = NA, stringsAsFactors=FALSE)

#Warning: overwrites previous log files on the same day
write.table(error_log, file = paste0(folder, "\\", current_date, "\\error_log.csv"), sep = ",", append = FALSE, col.names = TRUE, row.names = FALSE)


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
      parent_df[i, "max_peak"] <- -1000
    } else {
      parent_df[i, "max_peak"] <- max(selected_event$level_ft)
      
    }

    write.table(parent_df, file =  "overtop.csv", sep = ",")
    

    
  }, error=function(e){
    error_log[1,1] <<- i
    error_log[1,2] <<- toString(conditionMessage(e))
    write.table(error_log, file = paste0(folder, "/", current_date, "/error_log.csv"), sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
  }
  )
}




#read old data if not calcualted above
parent_df <- read.csv("//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/59 Performance Evaluation of Private Sites/61369/2024-05-22/max level.csv")

### Plotting 
# SW1
plot_dt <- parent_df %>% 
  filter(max_peak != -1000 & ow_suffix == "SW1") 

plot_dt$eventdatastart_edt <-as.Date(plot_dt$eventdatastart_edt)

plot_storage_sw1 <- ggplot(plot_dt, aes(x = eventdatastart_edt, y= max_peak)) +
  geom_point(aes(color = eventdepth_in), size = 5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Date of Event", y = "Max Level (ft)") +
  scale_x_date(date_labels="%b-%y",date_breaks  ="6 month") +
  scale_colour_gradient2() 

plot_depth_sw1 <- ggplot(plot_dt, aes(x = eventdatastart_edt, y= eventdepth_in)) +
  geom_point(aes(color = max_peak), size = 5) +
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
  filter(max_peak != -1000 & ow_suffix == "CS1") 

plot_dt$eventdatastart_edt <-as.Date(plot_dt$eventdatastart_edt)

plot_storage_cs1 <- ggplot(plot_dt, aes(x = eventdatastart_edt, y= max_peak)) +
  geom_point(aes(color = eventdepth_in), size = 5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Date of Event", y = "Max Level (ft)") +
  scale_x_date(date_labels="%b-%y",date_breaks  ="6 month") +
  scale_colour_gradient2()

plot_depth_cs1 <- ggplot(plot_dt, aes(x = eventdatastart_edt, y= eventdepth_in)) +
  geom_point(aes(color = max_peak), size = 5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Date of Event", y = "Rain Depth (in)") +
  scale_x_date(date_labels="%b-%y",date_breaks  ="6 month") +
  geom_hline(yintercept= 1.5, color = "brown", size=2, linetype="dashed") +
  geom_vline(xintercept= as.Date('2022-06-01'), color = "purple", size=2, linetype="dashed") +
  ggtitle("CS1 (61369)- From 2017-07-22 To 2023-11-01") +
  annotate("text", x = as.Date('2018-03-01'), y = 1.7, label = 'Design Storm: 1.5"', size = 20/.pt) +
  annotate("text", x = as.Date('2021-06-01'), y = 4, label = 'Pipe Change Occured on June 2022', size = 20/.pt) +
  scale_colour_gradient2()



### Looking at the flow meter data
flowdata_loc <- "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\59 Performance Evaluation of Private Sites\\61369\\2024-05-22\\Flowmeter Data"
flowdata_files <- list.files(flowdata_loc)
setwd(flowdata_loc)

# creating a container 
all_fd <- read_csv(flowdata_files[1])[0,] %>%
  select(Time, `Flow (MGD)`)
all_fd$Time <-  mdy_hms(all_fd$Time)
all_fd$`Flow (MGD)` <- as.numeric(all_fd$`Flow (MGD)`)


for (i in 1:length(flowdata_files)) {
  
  temp <- read_csv(flowdata_files[i]) %>%
    select(Time, `Flow (MGD)`)

  temp$Time <-  mdy_hms(temp$Time)
  temp$`Flow (MGD)` <- as.numeric(temp$`Flow (MGD)`)

  temp <- na.omit(temp)

  all_fd <- bind_rows(all_fd, temp)


}

# looking at 2024 flow data, rain data, level data

plot_fd <- all_fd 

plot(plot_fd$Time, plot_fd$`Flow (MGD)`)


plot_flow <- ggplot(plot_fd, aes(x = Time, y= `Flow (MGD)`)) +
  geom_point() +
  scale_y_reverse()


# getting the rain data

monitoringdata <- marsFetchMonitoringData(con = con, 
                                          target_id = "61369", 
                                          ow_suffix = "CS1", 
                                          source = "gage",
                                          start_date = "2017-11-08", 
                                          end_date = "2024-05-31", 
                                          sump_correct = FALSE,
                                          debug = TRUE,
                                          level = TRUE)

rain_event_data <- monitoringdata[["Rain Event Data"]]
rain_data <- monitoringdata[["Rainfall Data"]]
level_data <- monitoringdata[["Level Data"]]

plot(rain_data$dtime_est, rain_data$rainfall_in)

plot_rain <- ggplot(rain_data, aes(x = dtime_est, y= rainfall_in)) +
  geom_point()

ggarrange(plot_rain, plot_flow)


# rain data processing
rain_event_data_peak <- rain_event_data 
rain_event_data_peak$eventdatastart_est <- as.Date(rain_event_data_peak$eventdatastart_est)
rain_event_data_peak$eventdataend_est <- as.Date(rain_event_data_peak$eventdataend_est)


### analysis based on peak intensity
flow_data_daily <- all_fd 
flow_data_daily$Time <- as.Date(flow_data_daily$Time)

flow_peak_per_storm <- data.frame(gage_event_uid = as.integer(),
                                  peak_flow = as.numeric())

# getting the peak flow per storm duration

for (i in 1:nrow(rain_event_data_peak)) {
  temp_flow_df <- flow_data_daily %>%
    filter(Time >= rain_event_data_peak[i, "eventdatastart_est"] & Time <= rain_event_data_peak[i, "eventdataend_est"] + 1)
  
  if (nrow(temp_flow_df) !=  0) {
  temp_output <- data.frame(gage_event_uid = rain_event_data_peak[i, "gage_event_uid"],
                            peak_flow = max(temp_flow_df$`Flow (MGD)`))
  }
  
  flow_peak_per_storm <- bind_rows(flow_peak_per_storm, temp_output)
  
  
}



# rain-flow joined
joined_rain_flow <- rain_event_data_peak %>%
  left_join(flow_peak_per_storm, by = "gage_event_uid")


plot(joined_rain_flow$eventpeakintensity_inhr, joined_rain_flow$peak_flow)

ggplot(joined_rain_flow, aes(x = eventdepth_in, y= peak_flow)) +
  geom_point(size = 2.5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Depth (in)", y = "Maximum Flow Rate during the Rain Event (MGD)") + 
  theme(legend.position = "none")

# Calculating the peak intesity over an hour instead of 15-minute
rain_data_filtered <- rain_data %>%
  filter(gage_event_uid %in% joined_rain_flow$gage_event_uid) %>%
  na.omit()
  
# getting the event ids
unique_event_id <- rain_data_filtered %>%
  select(gage_event_uid) %>%
  distinct() %>%
  pull

hourly_peak <- data.frame(gage_event_uid = NA,
                          peak_intensity_hourly_inhr = NA)

hourly_peak <- na.omit(hourly_peak)
# calculate the peak intesity based on 1-hour interval
for (i in 1:length(unique_event_id)) {
  
  temp_id <- unique_event_id[i]
  temp_rain_data <- rain_data_filtered %>%
    filter(gage_event_uid == temp_id) %>%
    pad() %>%
    fill_by_value(rainfall_in, value = 0) %>%
    fill_by_prevalent(gage_event_uid) %>%
    fill_by_prevalent(gage_uid)
  
  if(nrow(temp_rain_data) >= 4) {
    # calculating 
    temp_rain_data_ave <- temp_rain_data %>%
      dplyr::mutate(hourly_intensity = zoo::rollsum(rainfall_in, k = 4, fill = NA)) 
    # hourly peak intensity
    temp_peak_hourly_intensity <- max(na.omit(temp_rain_data_ave$hourly_intensity))
    
    temp_df <- data.frame(gage_event_uid = temp_id,
                          peak_intensity_hourly_inhr = temp_peak_hourly_intensity)
    
    hourly_peak <- bind_rows(hourly_peak, temp_df)
  } else {
    temp_df <- data.frame(gage_event_uid = temp_id,
                          peak_intensity_hourly_inhr = sum(temp_rain_data$rainfall_in))
    
    hourly_peak <- bind_rows(hourly_peak, temp_df)
    
  }
}


# recreating the plot with hourly peak
joined_rain_flow <- joined_rain_flow %>%
  inner_join(hourly_peak, by = "gage_event_uid")


ggplot(joined_rain_flow, aes(x = peak_intensity_hourly_inhr, y= peak_flow)) +
  geom_point(aes(color = eventdepth_in), size = 5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Hourly Peak Intensity (in/hr)", y = "Maximum Flow Rate during the Rain Event (MGD)") +
  scale_color_gradient(low = "blue", high = "red")
