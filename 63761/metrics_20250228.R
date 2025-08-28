#### Script to generate metrics table for full monitoring period at Parkside 63761

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
library(scales)

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

# Set parameters
smp_id <- '63761'
ow_suffix <- 'CS1'
eval_start <- '2023-10-17'
eval_end <- '2025-02-28'
storage_depth_ft <- 5.3 # Weir depth relative to dist pipe invert

smp_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = ow_suffix,
                                       start_date = eval_start,
                                       end_date = eval_end,
                                       sump_correct = TRUE) %>% ##Revisit the sump issue
  mutate(dtime_est = with_tz(dtime, tzone = "EST"))

### Create metrics table 
# Read event data 
smp_metrics <- marsFetchRainEventData(mars_con,
                                     target_id = smp_id,
                                     source = 'gage',
                                     start_date = eval_start,
                                     end_date = eval_end) %>%
  # Convert start and end times to est
  mutate(eventdatastart_est = with_tz(eventdatastart, tzone = "EST"), 
         eventdataend_est = with_tz(eventdatastart, tzone = "EST"), 
         # Create empty cols for metrics
         peak_level_ft = NA, overtop = NA, rpsu = NA) %>%
  # Remove unnecessary cols
  select(-gage_uid, -eventdatastart, -eventdataend)

# Loop through events and calculate metrics
for(i in 1:length(smp_metrics$gage_event_uid)){
  event_data_i <- smp_monitor_data %>% filter(between(dtime_est, 
                                                      smp_metrics$eventdatastart_est[i] - hours(6), 
                                                      smp_metrics$eventdataend_est[i] + days(1)))
  smp_metrics$peak_level_ft[i] = max(event_data_i$level_ft)
  smp_metrics$overtop[i] = smp_metrics$peak_level_ft[i] > storage_depth_ft
  smp_metrics$rpsu[i] = smp_metrics$peak_level_ft[i] / storage_depth_ft * 100
}

# Close database connection
poolClose(mars_con)

# Save the data #####
write_csv(x = smp_metrics,
          paste0("63761/output/metrics_", eval_end, ".csv"))

# Plot event depth vs. rpsu  
ggplot(data = smp_metrics, mapping = aes(x = eventdepth_in, y = rpsu)) + 
  geom_point() + 
  scale_y_continuous(breaks = seq(from=0, to=100, by = 5.0))

# Calculate median RPSU for storms over 1.5"
smp_metrics_big_storms <- filter(smp_metrics, eventdepth_in > 1.5)
median_rpsu <- median(smp_metrics_big_storms$rpsu)

