#### Script to generate storm-specific timeseries data from monitoring locations at Ferko 411

#### Setup

# Library necessary packages
library(tidyverse) 
library(ggplot2)
library(pwdgsi)
library(odbc)
library(pool)
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

smp_id <- '411-1-1'

# ## Storm 1
# storm_start_time <- '2021-06-15 00:45:00'
# storm_end_time <- '2021-06-15 07:45:00'

# ## Storm 2

# storm_start_time <- '2022-05-27 19:30:00'
# storm_end_time <- '2022-05-27 21:15:00'

# # Storm 3
# storm_start_time <- '2023-09-10 21:00:00'
# storm_end_time <- '2023-09-11 09:45:00'

## Storm 4
storm_start_time <- '2025-04-01 00:30:00'
storm_end_time <- '2025-04-01 06:45:00'


key_elevs <- c(47.16, 49.64, 49.92, 51.05, 51.75, 55.12, 59.2)
key_elev_descrips <- c("Bottom of CS1", "Bottom of OW2",  "Bottom of CS2", "Bottom of Stone",  
                       "Bottom of Manifold", "Top of CS1 Weir", "Top of CS1")
sys_invert_elev <- key_elevs[4]
key_depths <- key_elevs - sys_invert_elev

# CS1
cs1_suffix <- 'CS1'
cs1_ref_depth <- sys_invert_elev - key_elevs[1]

# CS2
cs2_suffix <- "CS2"
cs2_ref_depth <- sys_invert_elev - key_elevs[3]

# OW2
ow2_suffix <- "OW2"
ow2_ref_depth <- sys_invert_elev - key_elevs[2]
  
# Create times for start/end of data
data_start_time <- ymd_hms(storm_start_time) - hours(6)
data_end_time <- ymd_hms(storm_end_time) + hours(12)
data_start_date <- format(date(data_start_time), '%Y-%m-%d')
data_end_date <- format(date(data_end_time), '%Y-%m-%d')

# Import CS1 monitoring data
cs1_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = 'CS1',
                                       start_date = data_start_date,
                                       end_date = data_end_date,
                                       sump_correct = FALSE) %>% 
  filter(dtime >= data_start_time & dtime <= data_end_time) %>% 
  mutate(CS1 = level_ft - cs1_ref_depth) %>% 
  select(dtime, CS1)


# Import CS2 monitoring data
cs2_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = 'CS2',
                                       start_date = data_start_date,
                                       end_date = data_end_date,
                                       sump_correct = FALSE) %>% 
  filter(dtime >= data_start_time & dtime <= data_end_time) %>%
  mutate(CS2 = level_ft - cs2_ref_depth) %>%
  select(dtime, CS2)


# Import OW2 monitoring data
ow2_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = 'OW2',
                                       start_date = data_start_date,
                                       end_date = data_end_date,
                                       sump_correct = FALSE) %>%
  filter(dtime >= data_start_time & dtime <= data_end_time) %>% 
  mutate(OW2 = level_ft - ow2_ref_depth) %>%
  select(dtime, OW2) 

# Import rainfall data and correct for time offset
rainfall_data <- marsFetchRainfallData(mars_con, target_id = smp_id, start_date = data_start_date,
                                       end_date = data_end_date, 'radar') %>%
  filter(dtime >= data_start_time & dtime <= data_end_time) %>%
  right_join(cs1_monitor_data, by = 'dtime') %>%
  select(dtime, rainfall_in) %>%
  mutate(rainfall_in = replace_na(rainfall_in, 0))

# Combine CWL data into single table and reshape
full_data <- cs1_monitor_data %>%
  left_join(cs2_monitor_data, by = 'dtime') %>%
  left_join(ow2_monitor_data, by = 'dtime') %>%
  left_join(rainfall_data, by = "dtime") %>% 
  mutate(rainfall_in = replace_na(rainfall_in, 0))

# Reshape monitoring data
reshaped_data <- full_data %>%
  select(-rainfall_in) %>%
  pivot_longer(-dtime, names_to = "location", values_to = "water_level_ft")

wl_ts <- ggplot(reshaped_data, aes(x = dtime, y = water_level_ft, color = location)) + 
  geom_line(linewidth=1) + 
  ylab("Water Level (ft)") + 
  xlab("Date") + 
  ggtitle(paste0("Water Level Response from ", data_start_date, " Storm")) + 
  scale_y_continuous(breaks = scales::breaks_width(2),
                     labels = scales::number_format(accuracy = 0.1)) + 
  scale_x_datetime(date_breaks = "6 hours", minor_breaks = "3 hours") + 
  labs(color = "Location")

if(length(key_depths) > 0 & length(key_elev_descrips) > 0){
  for(i in 4:length(key_depths)){
    wl_ts <- wl_ts + geom_hline(yintercept = key_depths[i],  color = "black", size = 0.8, linetype = "dashed") +
      annotate("text", x = data_start_time, y = key_depths[i] + 0.4,
               label = key_elev_descrips[i], hjust = 0) + 
      geom_ribbon(aes(ymin = key_depths[4], ymax = key_depths[6]), fill = "blue", alpha = 0.01)
  }
}


rain_ts <- ggplot(full_data, aes(dtime)) +
  geom_col(aes(y = rainfall_in)) +
  ylab("Rainfall (in)") +
  xlab("Date") +
  ggtitle(paste0("Rainfall from ", data_start_date, " Storm")) +
  scale_x_datetime(date_breaks = "6 hours", minor_breaks = "3 hours") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggarrange(rain_ts, wl_ts, nrow = 2, heights = c(1,4), legend = "bottom")
ggsave(paste0(smp_id, "/output/storm_plot_", data_end_date, ".png"))


# Close database connection
poolClose(mars_con)
