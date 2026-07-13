#### Script to generate metrics table for full monitoring period at Summer St 63491

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
smp_id <- '63491'
ow_suffix <- 'OW1' ## Note: RPSU will be calc based on OW1 only
cs_suffix <- 'CS1'
eval_start <- '2024-11-01'
eval_end <- '2026-03-11'
ow_storage_depth_ft <- 3.5 # Top of stone relative to bottom of stone
cs_storage_depth_ft <- 3.0 # Weir depth relative to bottom of stone
ow_sump_depth <- -0.62 # Bottom of stone relative to bottom of OW1
cs_sump_depth <- 1.97 # Bottom of stone relative to bottom of CS

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
smp_monitor_data <- inner_join(ow_monitor_data, cs_monitor_data, by = 'dtime') %>%
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
    peak_level_ft_ow = NA, peak_level_ft_cs = NA, overtop = NA, ow_resp = NA, rpsu = NA, rpsu_cs = NA, monitored = NA) %>%
  # Remove unnecessary cols
  select(-gage_uid)


# Loop through events and calculate metrics

for(i in 1:length(smp_metrics$gage_event_uid)){
  event_data_i <- smp_monitor_data %>% filter(between(dtime, 
                                                      smp_metrics$eventdatastart[i] - hours(6), 
                                                      smp_metrics$eventdataend[i] + days(1)))
  if (nrow(event_data_i) > 0){
    smp_metrics$peak_level_ft_ow[i] = max(event_data_i$owlevel_ft)
    smp_metrics$peak_level_ft_cs[i] = max(event_data_i$cslevel_ft)
    smp_metrics$overtop[i] = smp_metrics$peak_level_ft_cs[i] > cs_storage_depth_ft
    smp_metrics$ow_resp[i] = smp_metrics$peak_level_ft_ow[i] > 0-ow_sump_depth+1/12
    smp_metrics$rpsu[i] = smp_metrics$peak_level_ft_ow[i] / ow_storage_depth_ft * 100
    smp_metrics$rpsu_cs[i] = smp_metrics$peak_level_ft_cs[i] / ow_storage_depth_ft * 100
    smp_metrics$monitored[i] = TRUE
  }
}

# Remove rows from events with no data
smp_metrics <- filter(smp_metrics, monitored == TRUE) %>%
  select(-monitored)

# Close database connection
poolClose(mars_con)

# Save the data #####
write_csv(x = smp_metrics,
          paste0(smp_id, "/output/metrics_", eval_end, ".csv"))

# Plot event depth vs. rpsu in OW1 
ggplot(data = smp_metrics, mapping = aes(x = eventdepth_in, y = rpsu)) + 
  geom_point() + 
  labs(title = paste0('Relative Percent of Storage Used (as Measured in OW1) at SMP ',smp_id),
       subtitle = paste0('All Rain Events in which both OW1 and CS1 were monitored')) + 
  xlab('Rain Event Depth (in)') + 
  ylim(0, 100) + 
  ylab('% Storage Used') 
ggsave(paste0(smp_id,"/output/rpsu_ow_vs_depth_", eval_end, ".png"))

# Plot event depth vs. rpsu_cs
ggplot(data = smp_metrics, mapping = aes(x = eventdepth_in, y = rpsu_cs)) + 
  geom_point() + 
  labs(title = paste0('Relative Percent of Storage Used (as Measured in CS1) at SMP ',smp_id),
       subtitle = paste0('All Rain Events in which both OW1 and CS1 were monitored')) + 
  xlab('Rain Event Depth (in)') + 
  ylim(0, 100) + 
  ylab('% Storage Used') 
ggsave(paste0(smp_id,"/output/rpsu_cs_vs_depth_", eval_end, ".png"))

# Plot event depth vs. rpsu_cs, code by whether response was observed in OW
ggplot(data = smp_metrics, mapping = aes(x = eventdepth_in, y = rpsu_cs, shape = ow_resp)) + 
  geom_point() + 
  scale_shape_manual(values = c(21, 19)) + 
  labs(title = paste0('Relative Percent of Storage Used (as Measured in CS1) at SMP ',smp_id),
       subtitle = paste0('All Rain Events in which both OW1 and CS1 were monitored'), 
       shape = "OW1 Response") + 
  xlab('Rain Event Depth (in)') + 
  ylim(0, 100) + 
  ylab('% Storage Used') 
ggsave(paste0(smp_id,"/output/rpsu_cs_vs_depth_resp_", eval_end, ".png"))

# Plot event intensity vs. rpsu_cs 
ggplot(data = smp_metrics, mapping = aes(x = eventpeakintensity_inhr, y = rpsu_cs)) + 
  geom_point() + 
  labs(title = paste0('Relative Percent of Storage Used (as Measured in CS1) at SMP ',smp_id),
       subtitle = paste0('All Rain Events in which both OW1 and CS1 were monitored')) + 
  xlab('Rain Event Peak Intensity (in/hr)') + 
  ylim(0, 100) + 
  ylab('% Storage Used') 
ggsave(paste0(smp_id,"/output/rpsu_cs_vs_intensity_", eval_end, ".png"))

# Plot event intensity vs. rpsu, code by whether response was observed in OW
ggplot(data = smp_metrics, mapping = aes(x = eventpeakintensity_inhr, y = rpsu_cs, shape = ow_resp)) + 
  geom_point() + 
  scale_shape_manual(values = c(21, 19)) + 
  labs(title = paste0('Relative Percent of Storage Used (as Measured in CS1) at SMP ',smp_id),
       subtitle = paste0('All Rain Events in which both OW1 and CS1 were monitored'),
       shape = 'OW1 Response') + 
  xlab('Rain Event Peak Intensity (in/hr)') + 
  ylim(0, 100) + 
  ylab('% Storage Used') 
ggsave(paste0(smp_id,"/output/rpsu_cs_vs_intensity_resp_", eval_end, ".png"))

# Plot event depth vs. intensity, with shading to indicate rpsu_cs
ggplot(data = smp_metrics, mapping = aes(x = eventdepth_in, y = eventpeakintensity_inhr, alpha = rpsu_cs)) + 
  geom_point(size = 2) + 
  labs(title = paste0('Relative Percent of Storage Used (as Measured in CS1) at SMP ',smp_id),
       subtitle = paste0('All Rain Events in which both OW1 and CS1 were monitored'),
       alpha = 'RPSU') + 
  guides(alpha = guide_legend(reverse = TRUE)) + 
  xlab('Rain Event Depth (in))') + 
  ylab('Rain Even Peak Intensity (in/hr)') 
ggsave(paste0(smp_id,"/output/rpsu_vs_depth_intensity_opacity_", eval_end, ".png"))

# Plot event depth vs. intensity, with color to indicate rpsu_cs
ggplot(data = smp_metrics, mapping = aes(x = eventdepth_in, y = eventpeakintensity_inhr, color = rpsu_cs)) + 
  geom_point(size = 2) + 
  scale_color_gradient(low = 'black', high = 'blue') + 
  labs(title = paste0('Relative Percent of Storage Used (as Measured in CS1) at SMP ',smp_id),
       subtitle = paste0('All Rain Events in which both OW1 and CS1 were monitored'),
       color = 'RPSU') + 
  xlab('Rain Event Depth (in))') + 
  ylab('Rain Even Peak Intensity (in/hr)') 
ggsave(paste0(smp_id,"/output/rpsu_vs_depth_intensity_color_", eval_end, ".png"))

# Plot event depth vs. intensity, with dot size to indicate rpsu_cs
ggplot(data = smp_metrics, mapping = aes(x = eventdepth_in, y = eventpeakintensity_inhr, size = rpsu_cs)) + 
  geom_point() + 
  labs(title = paste0('Relative Percent of Storage Used (as Measured in CS1) at SMP ',smp_id),
       subtitle = paste0('All Rain Events in which both OW1 and CS1 were monitored'),
       size = 'RPSU') + 
  guides(size = guide_legend(reverse = TRUE)) + 
  xlab('Rain Event Depth (in))') + 
  ylab('Rain Even Peak Intensity (in/hr)') 
ggsave(paste0(smp_id,"/output/rpsu_vs_depth_intensity_size_", eval_end, ".png"))

# Plot event depth vs. intensity, code by whether response was observed in OW
ggplot(data = smp_metrics, mapping = aes(x = eventdepth_in, y = eventpeakintensity_inhr, shape = ow_resp)) + 
  geom_point() + 
  scale_shape_manual(values = c(21, 19)) + 
  labs(title = paste0('Relative Percent of Storage Used (as Measured in CS1) at SMP ',smp_id),
       subtitle = paste0('All Rain Events in which both OW1 and CS1 were monitored'),
       shape = 'OW1 response') + 
  xlab('Rain Event Depth (in))') + 
  ylab('Rain Even Peak Intensity (in/hr)') + 
  geom_hline(yintercept = 0.46, color = 'blue', size = 0.4, linetype = 'dashed') + 
  geom_vline(xintercept = 0.66, color = 'blue', size = 0.4, linetype = 'dashed')
ggsave(paste0(smp_id,"/output/resp_vs_depth_intensity_", eval_end, ".png"))
