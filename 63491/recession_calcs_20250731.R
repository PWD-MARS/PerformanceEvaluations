#### Script to compare theoretical and observed OW1 recession rates from 20250731 storm at Summer St 63491

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

# Helper function to calculate recession rate (in ft/s) based on water level
get_rr <- function(Cd, Aorifice_sf, g_fpss, h_ft, Abasin_sf, porosity) {
  Qout_cfs <- Cd * Aorifice_sf * sqrt(2*g_fpss*h_ft)
  return(Qout_cfs / Abasin_sf / porosity)
}

smp_id <- '63491'
eval_start_date <- '2025-07-31'
eval_end_date <- '2025-07-31'
eval_start_time <- ymd_hm('2025-07-31 12:00 pm', tz = 'Etc/GMT+5')
eval_end_time <- ymd_hm('2025-07-31 22:00 pm', tz = 'Etc/GMT+5')
draindown_start_time <- ymd_hm('2025-07-31 14:00 pm', tz = 'Etc/GMT+5')
draindown_end_time <- ymd_hm('2025-07-31 22:00 pm', tz = 'Etc/GMT+5')
Abasin_sf <- 40 * 18 # Area of basin, in square feet
Aorifice_sf <- pi * (2.125/12/2)**2 # Area of orifice at bottom of system, in square feet
Cd <- 0.62 # Dimensionless discharge coefficient. Exact coefficient depends on geometry but 0.62 is used in PWD GSI # calculations
g_fpss <- 32.2 # Gravitational acceleration constant, in feet per square second
porosity <- 0.51 # Average porosity throughout basin (local porosity varies due to pipe-in-stone design)

key_elevs <- c(67.28, 69.25, 69.87, 71.00, 72.25, 72.75)
key_elev_descrips <- c('Bottom of CS1','Bottom of Stone/ \n 2.125" Orifice Invert','Bottom of OW1','6" Orifice Invert','Top of Weir', 'Top of Stone')

sys_invert_elev <- 69.25
key_depths <- key_elevs - sys_invert_elev
cs1_ref_depth <- sys_invert_elev - key_elevs[1]
ow1_ref_depth <- sys_invert_elev - key_elevs[3]

# Import OW1 monitoring data for full monitoring period
ow1_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = 'OW1',
                                       start_date = eval_start_date,
                                       end_date = eval_end_date,
                                       sump_correct = FALSE) %>%
  mutate(ow1level_ft = level_ft - ow1_ref_depth,
         dtime = with_tz(dtime, tzone = 'Etc/GMT+5'))

# Import CS1 monitoring data for full monitoring period
cs1_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = 'CS1',
                                       start_date = eval_start_date,
                                       end_date = eval_end_date,
                                       sump_correct = FALSE) %>%
  mutate(cs1level_ft = level_ft - cs1_ref_depth,
         dtime = with_tz(dtime, tzone = 'Etc/GMT+5'))

# Import rainfall data for full monitoring period
rain_data <- marsFetchRainfallData(
  mars_con,
  target_id = smp_id,
  start_date = eval_start_date,
  end_date = eval_end_date,
  'gage') %>%
  mutate(dtime = with_tz(dtime, tzone = 'Etc/GMT+5'))

# Combine cs1, ow1 and rainfall data into single dataframe and remove data 
# outside time frame of interest
full_data <- right_join(cs1_monitor_data, ow1_monitor_data, by = 'dtime') %>%
  left_join(rain_data, by = 'dtime') %>%
  select(dtime, ow1level_ft, cs1level_ft, rainfall_in) %>%
  mutate(rainfall_in = replace_na(rainfall_in, 0)) %>%
  filter(between(dtime, eval_start_time, eval_end_time))

rm(cs1_monitor_data, ow1_monitor_data, rain_data)

# Add calculated data
full_data$ow1calclevel_ft <- NA # Create new column for calculated water levels

# Loop through rows in full_data
for(i in 1:nrow(full_data)) {
  print(i)
  # At peak, set calc water level to observed water level
  if (full_data$dtime[i] == draindown_start_time) {
    full_data$ow1calclevel_ft[i] <- full_data$ow1level_ft[i]
  }
  # After peak, decrement as needed based on estimated recession rate
  else if (between(full_data$dtime[i], draindown_start_time, draindown_end_time)){
    # Calculate recession rate based on previous calculated water level
    rr <- get_rr(Cd, Aorifice_sf, g_fpss, full_data$ow1calclevel_ft[i-1], Abasin_sf, porosity)
    # Calculate water level loss since prior measurement
    diff_s <- as.numeric(difftime(full_data$dtime[i], full_data$dtime[i-1], units = 'secs'))
    wl_loss <- rr * diff_s
    # Write new water level
    full_data$ow1calclevel_ft[i] <- full_data$ow1calclevel_ft[i-1] - wl_loss
  }
}


# Reshape monitoring data
reshaped_data <- full_data %>%
  select(-rainfall_in) %>%
  pivot_longer(-dtime, names_to = "data_source", values_to = "water_level_ft")

#Create wl plot
wl_ts <-
  ggplot(reshaped_data,
    aes(x = dtime, y = water_level_ft, color = data_source)) +
    geom_line(linewidth = 1) +
    ylab("Water Level (ft)") +
    xlab("Date") +
    labs(title = 'Observed and Calculated Water Levels for 2025-07-31 Storm') + 
    scale_color_manual(labels = c('CS1 Observed', 'OW1 Calculated', 'OW1 Observed'), 
                       values = c('darkorange2', 'purple', 'dodgerblue')) + 
    scale_y_continuous(
      breaks = scales::breaks_width(1),
      labels = scales::number_format(accuracy = 0.1)
    ) +
    labs(color = "Data Type")
if (length(key_depths) > 0 & length(key_elev_descrips) > 0) {
  for(j in 2:length(key_elevs)){
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
        x = full_data$dtime[1],
        y = key_depths[j] + 0.04,
        label = key_elev_descrips[j],
        hjust = 0,
        vjust = 0,
        lineheight = 0.8
      )
  }
}

# Create rainfall plot
rain_ts <- ggplot(full_data, aes(dtime)) +
  geom_col(aes(y = rainfall_in)) +
  ylab("Rainfall (in)") +
  xlab("Date") +
  labs(title = "Rainfall for 2025-07-31 Storm")  +
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

ggsave(paste0(smp_id, '/output/recession_plot_', eval_start_date, '.png'))





# Close database connection
poolClose(mars_con)
