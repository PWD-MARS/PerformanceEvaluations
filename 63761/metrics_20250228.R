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
ow_suffix <- 'OW1'
cs_suffix <- 'CS1'
eval_start <- '2023-10-17'
eval_end <- '2025-02-28'
ow_storage_depth_ft <- 5.5 # Top of stone relative to bottom of stone
cs_storage_depth_ft <- 5.9 # Weir depth relative to bottom of stone
ow_sump_depth <- -0.48 # Bottom of stone relative to bottom of OW
cs_sump_depth <- 2.5 # Bottom of stone relative to bottom of CS

# Import OW data
ow_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = ow_suffix,
                                       start_date = eval_start,
                                       end_date = eval_end,
                                       sump_correct = FALSE) %>% ##Revisit the sump issue
  mutate(dtime_est = with_tz(dtime, tzone = "EST"),
         owlevel_ft = level_ft - ow_sump_depth)

# Import CS data
cs_monitor_data <- marsFetchLevelData(mars_con,
                                      target_id = smp_id,
                                      ow_suffix = cs_suffix,
                                      start_date = eval_start,
                                      end_date = eval_end,
                                      sump_correct = FALSE) %>% ##Revisit the sump issue
  mutate(dtime_est = with_tz(dtime, tzone = "EST"),
         cslevel_ft = level_ft - cs_sump_depth)

# Combine OW and CS data into single dataframe and select relevant cols
smp_monitor_data <- right_join(ow_monitor_data, cs_monitor_data, by = 'dtime_est') %>%
  select(dtime_est, owlevel_ft, cslevel_ft)
# Delete OW and CS dataframes
rm(cs_monitor_data, ow_monitor_data)


### Create metrics table 
# Read event data 
smp_metrics <- marsFetchRainEventData(mars_con,
                                     target_id = smp_id,
                                     source = 'gage',
                                     start_date = eval_start,
                                     end_date = eval_end) %>%
  # Convert start and end times to est
  mutate(eventdatastart_est = with_tz(eventdatastart, tzone = "EST"), 
         eventdataend_est = with_tz(eventdataend, tzone = "EST"), 
         # Create empty cols for metrics
         peak_level_ft_ow = NA, peak_level_ft_cs = NA, overtop = NA, rpsu = NA) %>%
  # Remove unnecessary cols
  select(-gage_uid, -eventdatastart, -eventdataend)

# Loop through events and calculate metrics
for(i in 1:length(smp_metrics$gage_event_uid)){
  event_data_i <- smp_monitor_data %>% filter(between(dtime_est, 
                                                      smp_metrics$eventdatastart_est[i] - hours(6), 
                                                      smp_metrics$eventdataend_est[i] + days(1)))
  smp_metrics$peak_level_ft_ow[i] = max(event_data_i$owlevel_ft)
  smp_metrics$peak_level_ft_cs[i] = max(event_data_i$cslevel_ft)
  smp_metrics$overtop[i] = smp_metrics$peak_level_ft_cs[i] > cs_storage_depth_ft
  smp_metrics$rpsu[i] = smp_metrics$peak_level_ft_ow[i] / ow_storage_depth_ft * 100
}

# Close database connection
poolClose(mars_con)

# Save the data #####
write_csv(x = smp_metrics,
          paste0("63761/output/metrics_", eval_end, ".csv"))

# Plot event depth vs. rpsu  
ggplot(data = smp_metrics, mapping = aes(x = eventdepth_in, y = rpsu)) + 
  geom_point() + 
  #scale_y_continuous(breaks = seq(from=0, to=100, by = 5.0)) + 
  labs(title = 'Relative Percent of Storage Used at SMP 63761',
       subtitle = 'All Rain Events from 10/17/2023 - 2/28/2025') + 
  xlab('Rain Event Depth (in)') + 
  ylim(0, 50) + 
  ylab('% Storage Used') 

ggsave(paste0("63761/output/rpsu_vs_depth_", eval_start, ".png"))

# Calculate median RPSU for storms over 1.5"
smp_metrics_big_storms <- filter(smp_metrics, eventdepth_in > 1.5)
median_rpsu <- median(smp_metrics_big_storms$rpsu)

