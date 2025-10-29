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
smp_id <- '63182'
ow_suffix <- 'OW1' ## Note: RPSU will be calc based on OW1 only
cs_suffix <- 'CS1'
eval_start <- '2018-05-01'
eval_end <- '2025-07-14'
ow_storage_depth_ft <- 5.98 # Top of stone relative to bottom of stone
cs_storage_depth_ft <- 5.48 # Weir depth relative to bottom of stone
ow_sump_depth <- 0.35 # Bottom of stone relative to bottom of OW1
# Note: OW sump depth calculated as -0.48 based on surveyed rim elevation,
# bottom of stone elevation, and measured well depth. Changed to -0.2 ft to
# force CS1 and OW1 water depths to match.
cs_sump_depth <- 1.46 # Bottom of stone relative to bottom of CS

# Import OW data
ow_monitor_data <- marsFetchLevelData(mars_con,
                                      target_id = smp_id,
                                      ow_suffix = ow_suffix,
                                      start_date = eval_start,
                                      end_date = eval_end,
                                      sump_correct = FALSE) %>%
  mutate(owlevel_ft = level_ft - ow_sump_depth)

# Import CS data
cs_monitor_data <- marsFetchLevelData(mars_con,
                                      target_id = smp_id,
                                      ow_suffix = cs_suffix,
                                      start_date = eval_start,
                                      end_date = eval_end,
                                      sump_correct = FALSE) %>%
  mutate(cslevel_ft = level_ft - cs_sump_depth)

# Combine OW and CS data into single dataframe and select relevant cols
smp_monitor_data <- full_join(ow_monitor_data, cs_monitor_data, by = 'dtime') %>%
  select(dtime, owlevel_ft, cslevel_ft)
# Delete OW and CS dataframes
rm(cs_monitor_data, ow_monitor_data)


### Create metrics table 
# Read event data 
smp_metrics <- marsFetchRainEventData(mars_con,
                                      target_id = smp_id,
                                      source = 'gage',
                                      start_date = eval_start,
                                      end_date = eval_end) %>%
  
  mutate(
    # Create empty cols for metrics
    peak_level_ft_ow = NA, peak_level_ft_cs = NA, overtop = NA, rpsu = NA) %>%
    # Remove unnecessary cols
    select(-gage_uid)


# Loop through events and calculate metrics
for(i in 1:length(smp_metrics$gage_event_uid)){
  event_data_i <- smp_monitor_data %>% filter(between(dtime, 
                                                      smp_metrics$eventdatastart[i] - hours(6), 
                                                      smp_metrics$eventdataend[i] + days(1)))
  smp_metrics$peak_level_ft_ow[i] = max(event_data_i$owlevel_ft)
  smp_metrics$peak_level_ft_cs[i] = max(event_data_i$cslevel_ft)
  smp_metrics$overtop[i] = smp_metrics$peak_level_ft_cs[i] > cs_storage_depth_ft
  smp_metrics$rpsu[i] = smp_metrics$peak_level_ft_ow[i] / ow_storage_depth_ft * 100
}

# Close database connection
poolClose(mars_con)

# Save the data #####
write_csv(x = smp_metrics,
          paste0(smp_id, "/output/metrics_", eval_end, ".csv"))

# Plot event depth vs. rpsu 
ggplot(data = filter(smp_metrics, rpsu>= 0), mapping = aes(x = eventdepth_in, y = rpsu)) + 
  geom_point() + 
  labs(title = paste0('Relative Percent of Storage Used at SMP ',smp_id),
       subtitle = paste0('All Rain Events from ', eval_start, ' - ', eval_end)) + 
  xlab('Rain Event Depth (in)') + 
  #ylim(0, 50) + 
  ylab('% Storage Used') 
ggsave(paste0(smp_id,"/output/rpsu_vs_depth_", eval_end, ".png"))

# Calculate median RPSU for storms over 1.5"
smp_metrics_big_storms <- filter(smp_metrics, eventdepth_in > 1)
median_rpsu <- median(smp_metrics_big_storms$rpsu)
