#### Script to generate metrics table for full monitoring period at Dependable Site 99 63182

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
eval_start <- '2018-05-18'
eval_end <- '2026-02-11'
ow_storage_depth_ft <- 5.98 # Top of stone relative to bottom of stone
cs_storage_depth_ft <- 5.48 # Weir depth relative to bottom of stone
ow_sump_depth <- -0.27 # Bottom of stone relative to bottom of OW1
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
    peak_level_ft_ow = NA, peak_level_ft_cs = NA, overtop = NA, rpsu = NA, norm_resp = NA, monitored = NA, storm_size = NA,
    # Add flag for before/after 2022-2024 gap     
    period = ifelse(eventdatastart < "2024-05-05 00:00:00 EST", "Before Repairs", "After Repairs"),
    # Add storm size designation
    storm_size = ifelse(eventdepth_in <= 0.5, 'small', ifelse(eventdepth_in < 1.5, 'medium', 'large'))) %>%
    # Remove unnecessary cols
    select(-gage_uid)

# Make period and storm size factors and reorder
smp_metrics$period <- factor(smp_metrics$period, levels = c('Before Repairs','After Repairs'), ordered = TRUE)
smp_metrics$storm_size <- factor(smp_metrics$storm_size, levels = c('small','medium','large'), ordered = TRUE)

# Loop through events and calculate metrics

for(i in 1:length(smp_metrics$gage_event_uid)){
  event_data_i <- smp_monitor_data %>% filter(between(dtime, 
                                                      smp_metrics$eventdatastart[i], 
                                                      smp_metrics$eventdataend[i] + hours(1)))
  if (nrow(event_data_i) > 0){
    smp_metrics$peak_level_ft_ow[i] = min(max(event_data_i$owlevel_ft), ow_storage_depth_ft) # Ignore any response above storage depth
    smp_metrics$peak_level_ft_cs[i] = max(event_data_i$cslevel_ft)
    smp_metrics$overtop[i] = smp_metrics$peak_level_ft_cs[i] > cs_storage_depth_ft
    smp_metrics$rpsu[i] = smp_metrics$peak_level_ft_ow[i] / ow_storage_depth_ft * 100
    smp_metrics$norm_resp[i] = smp_metrics$peak_level_ft_ow[i] / smp_metrics$eventdepth_in[i] * 12
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

# Plot event depth vs. rpsu 
ggplot(data = filter(smp_metrics, rpsu>= 0), mapping = aes(x = eventdepth_in, y = rpsu)) + 
  geom_point() + 
  labs(title = paste0('Relative Percent of Storage Used at SMP ',smp_id),
       subtitle = paste0('All Rain Events in which both OW1 and CS1 were monitored')) + 
  xlab('Rain Event Depth (in)') + 
  #ylim(0, 50) + 
  ylab('% Storage Used') 
ggsave(paste0(smp_id,"/output/rpsu_vs_depth_", eval_end, ".png"))

# Recreate plot of event depth vs. rpsu, including color coding for dates before
# and after 5/5/24
ggplot(data = smp_metrics, mapping = aes(x = eventdepth_in, y = rpsu, color = period)) + 
  geom_point() + 
  scale_color_manual(values = c('Before Repairs' = 'orange', 'After Repairs' = 'blue')) + 
  #scale_y_continuous(breaks = seq(from=0, to=100, by = 5.0)) + 
  labs(title = 'Relative Percent of Storage Used at SMP 63182',
       subtitle = 'All Rain Events in which both OW1 and CS1 were monitored') + 
  xlab('Rain Event Depth (in)') + 
  ylab('% Storage Used') + 
  labs(color = "Period")
ggsave(paste0(smp_id, "/output/rpsu_vs_depth_with_period_", eval_end, ".png"))

# Make some box plots comparing performance before/after repairs

# Make storm size labeller function
size_labels <- list(
  'small' = 'Event depth <= 0.5"',
  'medium' = '0.5 < Event depth < 1.5"',
  'large' = 'Event depth \u2265 1.5"'
)
size_labeller <- function(variable, value){
  return(size_labels[value])
}

#RPSU, all rain events
rpsu_all <- ggplot(smp_metrics, aes(x = period, y = rpsu)) + 
  geom_boxplot(outliers = FALSE) + 
  ylim(0, 100) + 
  geom_jitter(color='purple', size=1, width = 0.05) + 
  labs(title = 'Relative Percent of Storage Used for All Rain Events') +
  ylab('RPSU') +
  theme(axis.title.x = element_blank()) +  
  stat_summary(fun = median, geom = "text", 
               aes(label = paste0('Median: ', round(..y.., 1))), position = position_nudge(y = 3, x = 0.2)) 
ggsave(paste0(smp_id, "/output/rpsu_boxplot_all_", eval_end, ".png"))

rpsu_by_size <- ggplot(filter(smp_metrics, storm_size != 'small'), aes(x = period, y = rpsu)) + 
  geom_boxplot(outliers = FALSE) + 
  ylim(0, 100) + 
  labs(title = 'Relative Percent of Storage Used Before and After Repairs') +
  ylab('RPSU') +
  theme(axis.title.x = element_blank()) +  
  geom_jitter(color='purple', size=1, width = 0.05) +
  stat_summary(fun = median, geom = "text", 
               aes(label = paste0('Median: ', round(..y.., 1))), position = position_nudge(y = 1.75, x = 0.32)) + 
  facet_grid(cols = vars(storm_size), labeller = size_labeller)
ggsave(paste0(smp_id, "/output/rpsu_by_size_boxplot_", eval_end, ".png"))

norm_resp_by_size <- ggplot(filter(smp_metrics, storm_size != 'small'), aes(x = period, y = norm_resp)) + 
  geom_boxplot(outliers = FALSE) + 
  ylim(0, 50) + 
  labs(title = 'Normalized Response Before and After Repairs') +
  ylab('Normalized Response') + 
  theme(axis.title.x = element_blank()) + 
  geom_jitter(color='purple', size=1, width = 0.05) +
  stat_summary(fun = median, geom = "text", 
               aes(label = paste0('Median: ', round(..y.., 1))), position = position_nudge(y = 1.7, x = 0.32)) + 
  facet_grid(cols = vars(storm_size), labeller = size_labeller)
ggsave(paste0(smp_id, "/output/norm_resp_by_size_boxplot_", eval_end, ".png"))
