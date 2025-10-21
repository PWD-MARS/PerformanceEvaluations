#### Script to generate storm-specific timeseries data from monitoring locations at Parkside 63761

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

smp_id <- '63761'
eval_start <- '2023-10-17'
eval_end <- '2025-02-28'

key_elevs <-
  c(109.0,
    111.5,
    111.9,
    111.7,
    112.0,
    112.8,
    114.5,
    117.0,
    117.4)
key_elev_descrips <-
  c(
    'bottom of CS1',
    "bottom of stone",
    '1" orifice invert',
    'bottom of OW1',
    "dist pipe invert",
    '2" orifice invert',
    "weir notch elevation",
    "top of stone",
    "top of CS1 weir"
  )

# Note: Bot. of OW1 calculated as 111.98 based on surveyed rim elevation and
# measured well depth. Changed to 111.7 ft to force CS1 and OW1 water depths to
# match.

sys_invert_elev <- 111.5
key_depths <- key_elevs - sys_invert_elev
cs1_ref_depth <- sys_invert_elev - key_elevs[1]
ow1_ref_depth <- sys_invert_elev - key_elevs[4]

# Choose threshold storm metrics
minpeakintensity_inhr <- 0 
mindepth_in <- 1.5

# Import CS1 monitoring data for full monitoring period
cs1_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = 'CS1',
                                       start_date = eval_start,
                                       end_date = eval_end,
                                       sump_correct = FALSE) %>%
  mutate(cs1level_ft = level_ft - cs1_ref_depth) 

# Import OW1 monitoring data for full monitoring period
ow1_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = 'OW1',
                                       start_date = eval_start,
                                       end_date = eval_end,
                                       sump_correct = FALSE) %>%
  mutate(ow1level_ft = level_ft - ow1_ref_depth)

# Import rainfall data for full monitoring period
rain_data <- marsFetchRainfallData(
  mars_con,
  target_id = smp_id,
  start_date = eval_start,
  end_date = eval_end,
  'gage'
) 

# Combine cs1 and ow1 data into single dataframe
full_data <- right_join(cs1_monitor_data, ow1_monitor_data, by = 'dtime') %>%
  left_join(rain_data, by = 'dtime') %>%
  select(dtime, ow1level_ft, cs1level_ft, rainfall_in) %>%
  mutate(rainfall_in = replace_na(rainfall_in, 0))

rm(cs1_monitor_data, ow1_monitor_data, rain_data)



# Import event metrics
event_data <- read.csv('63761/output/metrics_2025-02-27.csv') %>%
  mutate(eventdatastart = as_datetime(eventdatastart), 
         eventdataend = as_datetime(eventdataend)) %>%
  filter(eventdepth_in >= mindepth_in & eventpeakintensity_inhr >= minpeakintensity_inhr)

### Create storm plot for each big storm
for(i in 1:length(event_data$gage_event_uid)) {
  data_start_date <- format(date(event_data$eventdatastart[i]), '%Y-%m-%d')
  # Subset monitoring data
  full_data_i <- full_data %>%
    filter(dtime >= event_data$eventdatastart[i] - hours(6) & 
             dtime <= event_data$eventdataend[i] + days(1))
           
  # Reshape monitoring data
  reshaped_data_i <- full_data_i %>%
    select(-rainfall_in) %>%
    pivot_longer(-dtime, names_to = "location", values_to = "water_level_ft")
  #Create wl plot
  wl_ts <-
    ggplot(reshaped_data_i,
           aes(x = dtime, y = water_level_ft, color = location)) +
    geom_line(linewidth = 1) +
    ylab("Water Level (ft)") +
    xlab("Date") +
    labs(title = paste0('Water Level Response'), 
         subtitle = paste0('Max Response = ', 
                           round(event_data$peak_level_ft_ow[i], digits = 2), 
                           ' ft, ', 
                           round(event_data$rpsu[i], digits = 1), "% of storage used")) + 
    scale_color_manual(labels = c("CS1", "OW1"), values = c("darkorange2", "dodgerblue")) + 
    scale_y_continuous(
      breaks = scales::breaks_width(1),
      labels = scales::number_format(accuracy = 0.1)
    ) +
    scale_x_datetime(date_breaks = "12 hours", minor_breaks = "6 hours") +
    labs(color = "Location")
  if (length(key_depths) > 0 & length(key_elev_descrips) > 0) {
    for(j in c(2, 3, 5, 7, 8)){
      wl_ts <-
        wl_ts + geom_hline(
          yintercept = key_depths[j],
          color = "black",
          size = 0.4,
          linetype = "dashed"
         ) # +
        # annotate(
        #   "text",
        #   size = unit(2.6, 'pt'),
        #   x = event_data$eventdataend + days(1),
        #   y = key_depths[j] + 0.05,
        #   label = key_elev_descrips[j],
        #   hjust = 1
        # )
    }
  }
  # Create rainfall plot
  rain_ts <- ggplot(full_data_i, aes(dtime)) +
    geom_col(aes(y = rainfall_in)) +
    ylab("Rainfall (in)") +
    xlab("Date") +
    labs(title = paste0("Rainfall from ", data_start_date, " Storm"), 
         subtitle = paste0("Rainfall Depth = ", event_data$eventdepth_in[i], 
                           " in ,  Peak Intensity = ", event_data$eventpeakintensity_in[i], " in/hr")) +
    scale_x_datetime(date_breaks = "12 hours", minor_breaks = "6 hours") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  # Create combined plot
  ggarrange(
    rain_ts,
    wl_ts,
    nrow = 2,
    heights = c(1, 3),
    legend = "bottom"
  )
  ggsave(paste0("63761/output/storm_plot_", data_start_date, ".png"))
}




# Close database connection
poolClose(mars_con)
