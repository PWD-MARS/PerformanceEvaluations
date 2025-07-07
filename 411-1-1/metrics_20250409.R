#### Script to generate CSV file with metrics for each rain event during monitoring period

# Library necessary packages 
library(tidyverse)
library(pwdgsi)
library(odbc)
library(lubridate)

# Create database connection 
mars_con <- odbc::dbConnect(drv  = odbc::odbc(),
                            dsn  = "mars14_data")

#Set parameters for eval 
smp_id <- '411-1-1'
ow_suffix <- 'CS1'
eval_start <- '2021-02-23'
eval_end <- '2025-04-09'
rainfall_file <- "raw_data/Ferko_rain.csv" # In the future, import this via pwdgsi
rain_event_file <- "raw_data/Ferko_events.csv" # In the future, import this via pwdgsi
id_type = "gage"
storage_depth_ft <- 3.86; # Weir depth relative to bottom of structure
dls_x = FALSE;

# Import monitoring data and force time zone to EST
smp_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = ow_suffix,
                                       start_date = eval_start,
                                       end_date = eval_end,
                                       sump_correct = TRUE) %>%
  mutate(dtime_est = force_tz(dtime_est, "EST"))

# Find the dates of the first and last CWL datapoint
first_date <- min(smp_monitor_data$dtime_est) %>% as_date()
last_date <- max(smp_monitor_data$dtime_est) %>% as_date()

# Read event and rainfall files, force time zone to EST, and filter to only include dates for which we have CWL data
smp_events <- read.csv(rain_event_file) %>%
  mutate(eventdatastart_est = with_tz(force_tz(lubridate::ymd_hms(eventdatastart_edt), "America/New_York"),"EST")) %>%
  mutate(eventdataend_est = with_tz(force_tz(lubridate::ymd_hms(eventdataend_edt), "America/New_York"),"EST")) %>%
  select(-eventdatastart_edt, -eventdataend_edt)  %>%
  filter(date(eventdatastart_est) >= first_date &
           date(eventdataend_est) <= last_date)
smp_rainfall <- read.csv(rainfall_file) %>%
  mutate(dtime_est = with_tz(force_tz(lubridate::ymd_hms(dtime_edt), "America/New_York"),"EST")) %>%
  select(-dtime_edt) %>%
  filter(date(dtime_est) >= first_date &
           date(dtime_est) <= last_date)

# Create empty data frame for storing metrics
smp_metrics <- data.frame(smp_id = character(0), ow_suffix = character(0), event_uid = integer(0), 
                          gage_uid = integer(0), eventduration_hr = numeric(0), eventpeakintensity_inhr = numeric(0),
                          eventavgintensity_inhr = numeric(0), eventdepth_in = numeric(0), 
                          eventdatastart_est = character(0), eventdataend_est = character(0), peak_level_ft = numeric(0),
                          overtop = character(0), draindown_hr = numeric(0), rpsu = numeric(0))

# Loop through events
for(i in 1:length(smp_events$event_id)){
  # Select necessary data about event, water level, rainfall, and SMP
  event_x <- smp_events[i,]
  rainfall_x <- smp_rainfall %>% filter(dtime_est >= (event_x$eventdatastart_est - hours(6)) &
                                          dtime_est <= event_x$eventdataend_est + days(1))
  event_data_x <- inner_join(smp_monitor_data, rainfall_x, by = "dtime_est")
  if(nrow(event_data_x) > 0){
    snap_x <- marsFetchSMPSnapshot(con = mars_con, 
                                   smp_id = smp_id, ow_suffix = ow_suffix, request_date = event_x$eventdatastart_est)
    metric_x <- data.frame(smp_id = smp_id, 
                           ow_suffix = ow_suffix, 
                           event_uid = event_x$event_id,
                           gage_uid = event_x$gagename,
                           eventduration_hr = event_x$eventduration_hr,
                           eventpeakintensity_inhr = event_x$eventpeakintensity_inhr,
                           eventavgintensity_inhr = event_x$eventavgintensity_inhr,
                           eventdepth_in = event_x$eventdepth_in,
                           eventdatastart_est = format(event_x$eventdatastart_est, format = "%Y-%m-%d %H:%M:%S"),
                           eventdataend_est = format(event_x$eventdataend_est, format = "%Y-%m-%d %H:%M:%S"),
                           peak_level_ft = max(event_data_x$level_ft),
                           overtop = marsOvertoppingCheck_bool(waterlevel_ft = event_data_x$level_ft, 
                                                               storage_depth_ft = storage_depth_ft),
                           draindown_hr = marsDraindown_hr(dtime_est =  event_data_x$dtime_est,
                                                           rainfall_in = event_data_x$rainfall_in,
                                                           waterlevel_ft = event_data_x$level_ft),
                           rpsu = marsPeakStorage_percent(waterlevel_ft = event_data_x$level_ft,
                                                          storage_depth_ft = storage_depth_ft))
    # Write event data to metrics table
    smp_metrics <- bind_rows(smp_metrics, metric_x)
  } else {
    print(paste0('Error found at event: ',event_x$event_id,". No water level data found."))
  }
}

# Save the data #####
write.csv(x = smp_metrics,
          file = paste0("output/metrics_", last_date, ".csv"))

# Disconnect from the database ####
mars_con <- odbc::dbConnect(drv  = odbc::odbc(),
                            dsn  = "mars14_data")

