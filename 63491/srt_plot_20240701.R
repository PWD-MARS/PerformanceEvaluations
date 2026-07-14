#### Script to generate storm-specific timeseries data from monitoring locations at Dependable Site 99 63182

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
library(readxl)

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

smp_id <- '63491'
eval_start <- '2024-07-01'
eval_start_time <- ymd_hm('2024-07-01 11:15')
eval_end <- '2024-07-04'
eval_end_time <- ymd_hm('2024 07-04 23:55')

key_elevs <- c(67.28, 69.25, 69.87, 71.00, 72.25, 72.75)
key_elev_descrips <- c('Bottom of CS1','Bottom of Stone/ \n 2.125" Orifice Invert','Bottom of OW1','6" Orifice Invert','Top of Weir', 'Top of Stone')

sys_invert_elev <- 69.25
key_depths <- key_elevs - sys_invert_elev
cs1_ref_depth <- sys_invert_elev - key_elevs[1]
ow1_ref_depth <- sys_invert_elev - key_elevs[3]


# Import CS1 monitoring data for full monitoring period

cs1_monitor_data <- read_excel("63491/QAQC_SRT_63491_20240701_SPM_20240715.xlsx",
                               skip = 1, 
                               sheet = 'CS1_Data') %>%
  select(dtime = 5, cs1level_ft = 11) %>%
  filter(dtime > eval_start_time ) %>%
  filter(dtime <= eval_end_time) %>%
  mutate(cs1level_ft =  cs1level_ft) 

# Import OW1 monitoring data for full monitoring period
ow1_monitor_data <- read_excel("63491/QAQC_SRT_63491_20240701_SPM_20240715.xlsx",
                               skip = 1, 
                               sheet = 'OW1_Data') %>%
  select(dtime = 5, ow1level_ft = 11) %>%
  filter(dtime > eval_start_time ) %>%
  filter(dtime <= eval_end_time) %>%
  mutate(ow1level_ft =  ow1level_ft) 


# Import rainfall data for full monitoring period
# rain_data <- marsFetchRainfallData(
#   mars_con,
#   target_id = smp_id,
#   start_date = eval_start,
#   end_date = eval_end,
#   'gage'
# ) 

### There is no rainfall data from 7/1/24 through 7/4/24.marsFetchRainfallData 
### throws an error, so for now we will create an empty data frame instead
rain_data <- data.frame(
  gage_rain_uid = integer(),
  dtime = POSIXct(),
  gage_uid = integer(),
  rainfall_in = double(),
  gage_event_uid = integer()
)

# Combine cs1 and ow1 data into single dataframe
full_data <- right_join(cs1_monitor_data, ow1_monitor_data, by = 'dtime') %>%
  left_join(rain_data, by = 'dtime') %>%
  select(dtime, ow1level_ft, cs1level_ft, rainfall_in) %>%
  mutate(rainfall_in = replace_na(rainfall_in, 0))

# rm(cs1_monitor_data, ow1_monitor_data, rain_data)

# Reshape data
reshaped_data <- full_data %>%
  select(-rainfall_in) %>%
  pivot_longer(-dtime, names_to = "location", values_to = "water_level_ft")
  
#Create wl plot
wl_ts <-
  ggplot(reshaped_data,
          aes(x = dtime, y = water_level_ft, color = location)) +
  geom_line(linewidth = 1) +
  ylab("Water Level (ft)") +
  xlab("Date") +
  labs(title = paste0('Water Level Response'), 
        subtitle = paste0('SRT Performed ', eval_end)) + 
  scale_color_manual(labels = c('CS1', 'OW1'), values = c('darkorange2', 'dodgerblue')) + 
  scale_y_continuous(
    breaks = scales::breaks_width(1),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  scale_x_datetime(date_breaks = "7 days", minor_breaks = "1 day") +
  labs(color = "Location")
for(j in c(2, 3, 4, 5, 6)){
  wl_ts <-
    wl_ts + geom_hline(
      yintercept = key_depths[j],
      color = "black",
      size = 0.4,
      linetype = "dashed"
    ) +
  annotate(
    "text",
    size = unit(2.6, 'pt'),
    x = full_data$dtime[1]+days(2),
    y = key_depths[j] + 0.04,
    label = key_elev_descrips[j],
    hjust = 0,
    vjust = 0,
    lineheight = 0.8
  )
}
  
wl_ts
  
ggsave(paste0(smp_id, '/output/srt_plot_', eval_start, '.png'))


# Close database connection
poolClose(mars_con)

