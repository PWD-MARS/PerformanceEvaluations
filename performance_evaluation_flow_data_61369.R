### Script to generate visuals of performance at SMP-ID = 61369
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
library(data.table)


#connection 
con <- dbPool(odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

# SMP IDs for performance eval
smp_id_df <- data.frame(smp_id = "61369")


folder <- "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\59 Performance Evaluation of Private Sites\\61369"
current_date <- today()

# get the data
ow_all <- dbGetQuery(con, "SELECT * from fieldwork.tbl_ow")
#ow_leveldata_raw <- dbGetQuery(con, "SELECT * FROM data.tbl_ow_leveldata_raw")
gage_event <- dbGetQuery(con, "SELECT * FROM data.tbl_gage_event")
smp_gage <- dbGetQuery(con, "SELECT * FROM admin.tbl_smp_gage")


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
    filter(Time >= rain_event_data_peak[i, "eventdatastart_est"] & Time <= rain_event_data_peak[i, "eventdataend_est"])
  
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


# Compare before and after major events
# Peak intensity before pipe change on June 2022
peak_before_pipechange <- ggplot(filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01")), aes(x = eventpeakintensity_inhr, y= peak_flow)) +
  geom_point(size = 2.5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Peak Intensity (in/hr)", y = "Maximum Flow Rate during the Rain Event (MGD)") + 
  theme(legend.position = "none")

# Peak intensity after pipe change on June 2022
peak_after_pipechange <- ggplot(filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01")), aes(x = eventpeakintensity_inhr, y= peak_flow)) +
  geom_point(size = 2.5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Peak Intensity (in/hr)", y = "Maximum Flow Rate during the Rain Event (MGD)") + 
  theme(legend.position = "none")

# Depth before pipe change on June 2022
depth_before_pipechange <- ggplot(filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01")), aes(x = eventdepth_in, y= peak_flow)) +
  geom_point(size = 2.5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Depth (in)", y = "Maximum Flow Rate during the Rain Event (MGD)") + 
  theme(legend.position = "none")

# Depth intensity after pipe change on June 2022
depth_after_pipechange <- ggplot(filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01")), aes(x = eventdepth_in, y= peak_flow)) +
  geom_point(size = 2.5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Depth (in)", y = "Maximum Flow Rate during the Rain Event (MGD)") + 
  theme(legend.position = "none")



# calculate the volume of water to sewer

# Multiply the MGD by time step to get volume-then sum up during the storm
all_fd_vol <- all_fd
all_fd_vol["storm_volume_G"] <- (all_fd_vol$`Flow (MGD)`*5*1000000)/(24*60)
all_fd_vol["date"] <- as.Date(all_fd_vol$Time)

volume_df <- data.frame(gage_event_uid = 0,
                        volume_G = 0)

for (i in 1:nrow(joined_rain_flow)) {
  temp_df <- all_fd_vol %>%
    filter(date >= as.Date(joined_rain_flow[i, "eventdatastart_est"]) & date <= as.Date(joined_rain_flow[i, "eventdataend_est"]))
  
  if (nrow(temp_df) !=  0) {
    volume <- data.frame(gage_event_uid = joined_rain_flow[i, "gage_event_uid"],
                         volume_G = sum(temp_df$storm_volume_G))
  }
  
  volume_df <- bind_rows(volume_df, volume)
}

joined_rain_flow <- joined_rain_flow %>%
  inner_join(volume_df, by = "gage_event_uid") %>%
  mutate(volume_MG = volume_G/1000000) %>%
  distinct()

# Sewer volume plots
# Peak intensity before pipe change on June 2022
peak_before_pipechange_vs_vol <- ggplot(filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01") & eventdatastart_est > as.Date("2018-12-19")), aes(x = eventpeakintensity_inhr, y= volume_MG)) +
  geom_point(size = 3.5, color = "blue") +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Peak Intensity (in/hr)", y = "Volume Run-off through Diversion Structure During the Rain Event (MG)") + 
  theme(legend.position = "none") +
  labs(title="Dependable Distribution Wetland (61369) before Pipe Change (2018-12-19 to 2022-06-01)")

# Peak intensity after pipe change on June 2022
peak_after_pipechange_vs_vol <- ggplot(filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01")), aes(x = eventpeakintensity_inhr, y= volume_MG)) +
  geom_point(size = 3.5, color = "blue") +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Peak Intensity (in/hr)", y = "Volume Run-off through Diversion Structure During the Rain Event (MG)") + 
  theme(legend.position = "none") +
  labs(title="Dependable Distribution Wetland (61369) after Pipe Change on June 2022")

# Depth before pipe change on June 2022
depth_before_pipechange_vs_vol <- ggplot(filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01") & eventdatastart_est > as.Date("2018-12-19")), aes(x = eventdepth_in, y= volume_MG)) +
  geom_point(size = 3.5, color = "blue") +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Depth (in)", y = "Volume Overflow through Diversion Structure During the Rain Event (MG)") + 
  scale_y_continuous(limits = c(0, 0.35)) + 
  scale_x_continuous(limits = c(0, 3.6)) + 
  theme(legend.position = "none") +
  geom_vline(xintercept= 1.5, color = "darkgreen", size=2, linetype="dashed") +
  annotate("text", x = 1.2, y = 0.2, color = "purple", label = 'Design Storm: 1.5"', size = 20/.pt) +
  labs(title="Dependable Distribution Wetland (61369) before Pipe Change (2018-12-19 to 2022-06-01)")


# Depth intensity after pipe change on June 2022
depth_after_pipechange_vs_vol <- ggplot(filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01")), aes(x = eventdepth_in, y= volume_MG)) +
  geom_point(size = 3.5, color = "blue") +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Depth (in)", y = "Volume Overflow through Diversion Structure During the Rain Event (MG)") + 
  scale_y_continuous(limits = c(0, 0.35)) + 
  scale_x_continuous(limits = c(0, 3.6)) + 
  theme(legend.position = "none") +
  geom_vline(xintercept= 1.5, color = "darkgreen", size=2, linetype="dashed") +
  annotate("text", x = 1.2, y = 0.2, color = "purple", label = 'Design Storm: 1.5"', size = 20/.pt) +
  labs(title="Dependable Distribution Wetland (61369) after Pipe Change (2022-06-01)")



# Depth before diversion control raise 12/19/2018
depth_before_divcontrol_vs_vol <- ggplot(filter(joined_rain_flow, eventdatastart_est < as.Date("2018-12-19")), aes(x = eventdepth_in, y= volume_MG)) +
  geom_point(size = 3.5, color = "blue") +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Depth (in)", y = "Volume Overflow through Diversion Structure During the Rain Event (MG)") + 
  theme(legend.position = "none") +
  geom_vline(xintercept= 1.5, color = "darkgreen", size=2, linetype="dashed") +
  annotate("text", x = 1.2, y = 0.3, color = "purple", label = 'Design Storm: 1.5"', size = 20/.pt) +
  labs(title="Dependable Distribution Wetland (61369) before Forebay Lowering/Weir Raising (2018-12-19)")

# Depth after diversion control raise 12/19/2018
depth_after_divcontrol_vs_vol <- ggplot(filter(joined_rain_flow, eventdatastart_est > as.Date("2018-12-19") & eventdatastart_est < as.Date("2022-06-01")), aes(x = eventdepth_in, y= volume_MG)) +
  geom_point(size = 3.5, color = "blue") +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Depth (in)", y = "Volume Overflow through Diversion Structure During the Rain Event (MG)") + 
  theme(legend.position = "none") +
  geom_vline(xintercept= 1.5, color = "darkgreen", size=2, linetype="dashed") +
  annotate("text", x = 1.2, y = 0.3, color = "purple", label = 'Design Storm: 1.5"', size = 20/.pt) +
  labs(title="Dependable Distribution Wetland (61369) after Forebay Lowering/Weir Raising (2018-12-19 to 2022-06-01)")

# Peak before diversion control raise 12/19/2018
peak_before_divcontrol_vs_vol <- ggplot(filter(joined_rain_flow, eventdatastart_est < as.Date("2018-12-19")), aes(x = eventpeakintensity_inhr, y= volume_MG)) +
  geom_point(size = 3.5, color = "blue") +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Peak Intensity (in/hr)", y = "Volume Overflow through Diversion Structure During the Rain Event (MG)") + 
  theme(legend.position = "none") +
  geom_vline(xintercept= 1.5, color = "darkgreen", size=2, linetype="dashed") +
  annotate("text", x = 1.2, y = 0.3, color = "purple", label = 'Design Storm: 1.5"', size = 20/.pt) +
  labs(title="Dependable Distribution Wetland (61369) before Forebay Lowering/Weir Raising (2018-12-19)")


# Peak after diversion control raise 12/19/2018
peak_after_divcontrol_vs_vol <- ggplot(filter(joined_rain_flow, eventdatastart_est > as.Date("2018-12-19") & eventdatastart_est < as.Date("2022-06-01")), aes(x = eventpeakintensity_inhr, y= volume_MG)) +
  geom_point(size = 3.5, color = "blue") +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Rain Event Peak Intensity  (in/hr)", y = "Volume Overflow through Diversion Structure During the Rain Event (MG)") + 
  theme(legend.position = "none") +
  geom_vline(xintercept= 1.5, color = "darkgreen", size=2, linetype="dashed") +
  annotate("text", x = 1.2, y = 0.3, color = "purple", label = 'Design Storm: 1.5"', size = 20/.pt) +
  labs(title="Dependable Distribution Wetland (61369) after Forebay Lowering/Weir Raising (2018-12-19 to 2022-06-01)")



# combined before/after both events
ggarrange(depth_before_divcontrol_vs_vol, depth_after_pipechange_vs_vol, nrow = 1, ncol = 2)


# combined graph

joined_rain_flow_combined <- joined_rain_flow %>%
  filter(eventdatastart_est < as.Date("2018-12-19") | eventdatastart_est > as.Date("2022-06-01")) %>%
  mutate(`When Storm Occured?` =  fifelse(eventdatastart_est < as.Date("2018-12-19"), "Before Retrofit","After Retrofit"))

joined_rain_flow_combined$`When Storm Occured?` <- factor(joined_rain_flow_combined$`When Storm Occured?`, levels = c("Before Retrofit","After Retrofit"))

# Depth intensity after pipe change on June 2022
combo_performance_plot <- ggplot(joined_rain_flow_combined, aes(x = eventdepth_in, y= volume_MG, color = `When Storm Occured?`)) +
  geom_point(size = 10) +
  theme(text = element_text(size = 60), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  #geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Rain Event Depth (in)", y = "Volume Bypass through Diversion Structure DST-010-01 (MG)") + 
  scale_y_continuous(limits = c(0, 0.35)) + 
  scale_x_continuous(limits = c(0, 3.6)) +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 15)) + 
  scale_color_manual(values = c("#FC4E07", "#00AFBB"), name = "") +
  geom_vline(xintercept= 1.5, color = "darkgreen", size=2, linetype="dashed") +
  annotate("text", x = 1.55, y = 0.25, color = "darkgreen", label = 'Design Storm: 1.5"', size = 30/.pt, angle = 90) +
  labs(title="Dependable Distribution Wetland (61369) Bypass to Mill Creek Sewer", caption = "*Retrofits include 1) lowering earthen dam of forebay 1 and raising bypass weirs 3 inches in DST-010-01 on 2018-12-19 and 2) rerouting inflow piping to reduce restrictions due to utility conflicts on 2022-06-01")

# increased size

combo_performance_plot_enlarged <- ggplot(joined_rain_flow_combined, aes(x = eventdepth_in, y= volume_MG, color = `When Storm Occured?`)) +
  geom_point(size = 15) +
  theme(text = element_text(size = 60), 
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 40), # Increased caption size to 20
        legend.position = "bottom") + 
  labs(x = "Rain Event Depth (in)", y = "Volume Bypass through Diversion Structure DST-010-01 (MG)",
       title = "Dependable Wetland (61369) Bypass to Mill Creek Sewer",
       caption = "*Retrofits: 1) Lowering earthen dam of forebay 1 and raising bypass weirs 3\" in DST-010-01 on 2018-12-19 and 2) Rerouting inflow piping to reduce restrictions on 2022-06-01") + 
  scale_y_continuous(limits = c(0, 0.35)) + 
  scale_x_continuous(limits = c(0, 3.6)) +
  scale_color_manual(values = c("#FC4E07", "#00AFBB"), name = "") +
  geom_vline(xintercept = 1.5, color = "darkgreen", size = 2, linetype = "dashed") +
  annotate("text", x = 1.55, y = 0.25, color = "darkgreen", label = 'Design Storm: 1.5"', size = 40 / .pt, angle = 90)


box_p <- ggplot(joined_rain_flow_combined, aes(x=as.factor(`When Storm Occured?`), y=volume_MG)) + 
  geom_boxplot(color = "blue") +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "When Storm Occured?", y = "Volume Bypass through Diversion Structure DST-010-01 (MG)") +
  labs(title="Box Plot of Dependable Distribution Wetland (61369) Bypass to Mill Creek Sewer")



# metric calc- volume/depth
before <- filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01")) 
after <- filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01")) 
metric_before_voldepth <- sum(before$volume_G)/sum(before$eventdepth_in)
metric_after_voldepth <- sum(after$volume_G)/sum(after$eventdepth_in)


# metric calc- volume/peak
metric_before_volpeak <- sum(before$volume_G)/sum(before$eventpeakintensity_inhr)
metric_after_volpeak <- sum(after$volume_G)/sum(after$eventpeakintensity_inhr)

# metric calc- volume/peak
metric_before_voldepth <- sum(before$volume_G)/sum(before$eventdepth_in)
metric_after_voldepth <- sum(after$volume_G)/sum(after$eventdepth_in)

# metric- maxflow/depth
metric_before_flowdepth <- sum(before$peak_flow)/sum(before$eventdepth_in)
metric_after_flowdepth <- sum(after$peak_flow)/sum(after$eventdepth_in)

# metric- maxflow/peak
metric_before_flowpeak <- sum(before$peak_flow)/sum(before$eventpeakintensity_inhr)
metric_after_flowpeak <- sum(after$peak_flow)/sum(after$eventpeakintensity_inhr)


# Calculating metrics incorporating drainage area for pipe change
da_a1_1b_ft2 <- 99415.10 + 159987.07 + 52255.54 + 84535.73

#### Pipe change metrics
# total volume of overflow over diversion structure
total_volume_overflow_mg_before_pipe_millcreek <- filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01") & eventdatastart_est > as.Date("2018-12-19")) %>%
  select(volume_MG) %>%
  sum()

total_volume_overflow_mg_after_pipe_millcreek <- filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01")) %>%
  select(volume_MG) %>%
  sum()

# total volume of rainfall draining to the system
total_volume_rain_ft3_before_pipe_system <- (filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01") & eventdatastart_est > as.Date("2018-12-19")) %>%
  select(eventdepth_in) %>%
  sum())*da_a1_1b_ft2/12

total_volume_rain_ft3_after_pipe_system <- (filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01")) %>%
  select(eventdepth_in) %>%
  sum())*da_a1_1b_ft2/12


# total volume of overflow over diversion structure below 1.5 in 
total_volume_overflow_mg_before_pipe_millcreek_1.5 <- filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01") & eventdatastart_est > as.Date("2018-12-19") & eventdepth_in < 1.5) %>%
  select(volume_MG) %>%
  sum()

total_volume_overflow_mg_after_pipe_millcreek_1.5 <- filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01") & eventdepth_in < 1.5) %>%
  select(volume_MG) %>%
  sum()

# total volume of rainfall draining to the system
total_volume_rain_ft3_before_pipe_system_1.5 <- (filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01") & eventdatastart_est > as.Date("2018-12-19") & eventdepth_in < 1.5) %>%
                                               select(eventdepth_in) %>%
                                               sum())*da_a1_1b_ft2/12

total_volume_rain_ft3_after_pipe_system_1.5 <- (filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01") & eventdepth_in < 1.5) %>%
                                              select(eventdepth_in) %>%
                                              sum())*da_a1_1b_ft2/12


# metric calc- volume/peak
# metric calc- volume/peak
metric_before_voldepth_smallar <- sum(filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01") & eventdatastart_est > as.Date("2018-12-19")) $volume_G)/sum(filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01") & eventdatastart_est > as.Date("2018-12-19")) $eventdepth_in)
metric_after_voldepth_smaller <- sum(filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01") ) $volume_G)/sum(filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01")) $eventdepth_in)



metric_before_voldepth_smallar1.5 <- sum(filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01") & eventdatastart_est > as.Date("2018-12-19") & eventdepth_in < 1.5) $volume_G)/sum(filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01") & eventdatastart_est > as.Date("2018-12-19") & eventdepth_in < 1.5) $eventdepth_in)
metric_after_voldepth_smaller1.5 <- sum(filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01") & eventdepth_in < 1.5) $volume_G)/sum(filter(joined_rain_flow, eventdatastart_est > as.Date("2022-06-01") & eventdepth_in < 1.5) $eventdepth_in)




#### Forebay lowering and DST raising metrics
# total volume of overflow over diversion structure
total_volume_rain_mg_before_forebay_millcreek <- filter(joined_rain_flow, eventdatastart_est < as.Date("2018-12-19")) %>%
  select(volume_MG) %>%
  sum()

total_volume_rain_mg_after_forebay_millcreek <- filter(joined_rain_flow, eventdatastart_est > as.Date("2018-12-19") & eventdatastart_est < as.Date("2022-06-01")) %>%
  select(volume_MG) %>%
  sum()

# total volume of rainfall draining to the system
total_volume_rain_ft3_before_forebay_system <- (filter(joined_rain_flow, eventdatastart_est < as.Date("2018-12-19")) %>%
                                               select(eventdepth_in) %>%
                                               sum())*da_a1_1b_ft2/12

total_volume_rain_ft3_after_forebay_system <- (filter(joined_rain_flow, eventdatastart_est > as.Date("2018-12-19") & eventdatastart_est < as.Date("2022-06-01")) %>%
                                              select(eventdepth_in) %>%
                                              sum())*da_a1_1b_ft2/12


# total volume of overflow over diversion structure below 1.5 in 
total_volume_rain_mg_before_forebay_millcreek_1.5 <- filter(joined_rain_flow, eventdatastart_est < as.Date("2018-12-19") & eventdepth_in < 1.5) %>%
  select(volume_MG) %>%
  sum()

total_volume_rain_mg_after_forebay_millcreek_1.5 <- filter(joined_rain_flow, eventdatastart_est > as.Date("2018-12-19") & eventdatastart_est < as.Date("2022-06-01") & eventdepth_in < 1.5) %>%
  select(volume_MG) %>%
  sum()

# total volume of rainfall draining to the system
total_volume_rain_ft3_before_forebay_system_1.5 <- (filter(joined_rain_flow, eventdatastart_est < as.Date("2018-12-19") & eventdepth_in < 1.5) %>%
                                               select(eventdepth_in) %>%
                                               sum())*da_a1_1b_ft2/12

total_volume_rain_ft3_after_forebay_system_1.5 <- (filter(joined_rain_flow, eventdatastart_est > as.Date("2018-12-19") & eventdatastart_est < as.Date("2022-06-01") & eventdepth_in < 1.5) %>%
                                              select(eventdepth_in) %>%
                                              sum())*da_a1_1b_ft2/12

# metric calc- volume/peak
metric_before_voldepth_forebay_smallar <- sum(filter(joined_rain_flow, eventdatastart_est < as.Date("2018-12-19")) $volume_G)/sum(filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01"))$eventdepth_in)
metric_after_voldepth_forebay_smaller <- sum(filter(joined_rain_flow, eventdatastart_est > as.Date("2018-12-19") & eventdatastart_est < as.Date("2022-06-01"))$volume_G)/sum(filter(joined_rain_flow, eventdatastart_est > as.Date("2018-12-19") & eventdatastart_est < as.Date("2022-06-01"))$eventdepth_in)




# metric calc- volume/peak < 1.5 in
metric_before_voldepth_forebay_smallar1.5 <- sum(filter(joined_rain_flow, eventdatastart_est < as.Date("2018-12-19") & eventdepth_in < 1.5) $volume_G)/sum(filter(joined_rain_flow, eventdatastart_est < as.Date("2022-06-01") & eventdepth_in < 1.5) $eventdepth_in)
metric_after_voldepth_forebay_smaller1.5 <- sum(filter(joined_rain_flow, eventdatastart_est > as.Date("2018-12-19") & eventdatastart_est < as.Date("2022-06-01") & eventdepth_in < 1.5) $volume_G)/sum(filter(joined_rain_flow, eventdatastart_est > as.Date("2018-12-19") & eventdatastart_est < as.Date("2022-06-01") & eventdepth_in < 1.5) $eventdepth_in)




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
