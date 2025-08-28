#### Script to generate storm-specific timeseries data from monitoring locations at Parkside 63761

#### Setup

# Library necessary packages
library(tidyverse) 
library(ggplot2)
library(pwdgsi)
library(odbc)
library(DBI)
library(lubridate)
library(magrittr)
library(ggpubr)

# Create database connection
mars_con <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_prod",
  user = Sys.getenv("mars_uid"),
  password = Sys.getenv("mars_pwd"),
  timezone = NULL
)

smp_id <- '63761'

## Storm 1
storm_start_time <- '2023-12-17 12:15:00' # Must be in EST
storm_end_time <- '2023-12-18 12:00:00' # Must be in EST

# ##Storm 2
# storm_start_time <- '2024-08-06 14:30:00' # Must be in EST
# storm_end_time <- '2024-08-07 08:15:00' # Must be in EST


key_elevs <- c(109.0, 111.5, 111.9, 111.98, 112.1,  112.75, 114.5, 117.0, 117.4, 118.7, 119.4)
key_elev_descrips <-
  c(
    "bottom of CS1",
    "bottom of stone",
    '1" orifice invert',
    "bottom of OW1",
    "dist pipe invert",
    '2" orifice invert',
    "top of stone",
    "weir notch elevation",
    "top of CS1 weir",
    "CS1 rim",
    "top of OW1"
  )

sys_invert_elev <- 111.5
key_depths <- key_elevs - sys_invert_elev

# CS1
cs1_suffix <- 'CS1'
cs1_ref_depth <- sys_invert_elev - key_elevs[1]


# OW1
ow1_suffix <- "OW1"
ow1_ref_depth <- sys_invert_elev - key_elevs[3]
  
# Create times for start/end of data
data_start_time <- force_tz(ymd_hms(storm_start_time) - hours(3),'EST')
data_end_time <- force_tz(ymd_hms(storm_end_time) + hours(12),'EST')
data_start_date <- format(date(data_start_time), '%Y-%m-%d')
data_end_date <- format(date(data_end_time), '%Y-%m-%d')

# Import CS1 monitoring data
cs1_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = cs1_suffix,
                                       start_date = data_start_date,
                                       end_date = data_end_date,
                                       sump_correct = FALSE) %>% 
  mutate(dtime_est = force_tz(dtime, 'EST')) %>%
  filter(dtime_est >= data_start_time & dtime_est <= data_end_time) %>% 
  mutate(CS1 = level_ft - cs1_ref_depth) %>% 
  select(dtime_est, CS1)


# Import OW1 monitoring data
ow1_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = ow1_suffix,
                                       start_date = data_start_date,
                                       end_date = data_end_date,
                                       sump_correct = FALSE) %>%
  mutate(dtime_est = force_tz(dtime, 'EST')) %>%
  filter(dtime_est >= data_start_time & dtime_est <= data_end_time) %>% 
  mutate(OW1 = level_ft - ow1_ref_depth) %>%
  select(dtime_est, OW1) 

# Import rainfall data and filters for values for which we have monitoring data
rainfall_data <- marsFetchRainfallData(mars_con, target_id = smp_id, start_date = data_start_date,
                                       end_date = data_end_date, 'gage') %>%
  mutate(dtime_est = force_tz(dtime, "EST")) %>%
  filter(dtime_est >= data_start_time & dtime_est <= data_end_time) %>%
  right_join(cs1_monitor_data, by = 'dtime_est') %>%
  select(dtime_est, rainfall_in) %>%
  mutate(rainfall_in = replace_na(rainfall_in, 0))

# Combine CWL data into single table and reshape
full_data <- cs1_monitor_data %>%
  left_join(ow1_monitor_data, by = 'dtime_est') %>%
  left_join(rainfall_data, by = "dtime_est") %>% 
  mutate(rainfall_in = replace_na(rainfall_in, 0))

# Reshape monitoring data
reshaped_data <- full_data %>%
  select(-rainfall_in) %>%
  pivot_longer(-dtime_est, names_to = "location", values_to = "water_level_ft")

wl_ts <- ggplot(reshaped_data, aes(x = dtime_est, y = water_level_ft, color = location)) + 
  geom_line(linewidth=1) + 
  ylab("Water Level (ft)") + 
  xlab("Date") + 
  ggtitle(paste0("Water Level Response from ", data_start_date, " Storm")) + 
  scale_y_continuous(breaks = scales::breaks_width(2),
                     labels = scales::number_format(accuracy = 0.1)) + 
  scale_x_datetime(date_breaks = "6 hours", minor_breaks = "3 hours") + 
  labs(color = "Location")

if(length(key_depths) > 0 & length(key_elev_descrips) > 0){
#  for(i in 1:length(key_depths)){
  for (i in 2:7){
     wl_ts <- wl_ts + geom_hline(yintercept = key_depths[i],  color = "black", size = 0.8, linetype = "dashed") +
      annotate("text", x = data_start_time, y = key_depths[i] + 0.2,
               label = key_elev_descrips[i], hjust = 0) #+ 
#      geom_ribbon(aes(ymin = key_depths[4], ymax = key_depths[6]), fill = "blue", alpha = 0.01)
  }
}


rain_ts <- ggplot(full_data, aes(dtime_est)) +
  geom_col(aes(y = rainfall_in)) +
  ylab("Rainfall (in)") +
  xlab("Date") +
  ggtitle(paste0("Rainfall from ", data_start_date, " Storm")) +
  scale_x_datetime(date_breaks = "6 hours", minor_breaks = "3 hours") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggarrange(rain_ts, wl_ts, nrow = 2, heights = c(1,4), legend = "bottom")
ggsave(paste0("63761/output/storm_plot_", data_end_date, ".png"))

# Close database connection
poolClose(mars_con)

