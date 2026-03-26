#### Script to generate timeseries plots from monitoring data at Dependable Site 99 

#### Setup

# Library necessary packages
library(tidyverse)
library(ggplot2)
library(pwdgsi)
library(odbc)
library(lubridate)
library(magrittr)
library(pool)

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

# Function to create timeseries plot

plot_ts <-
  function(smp_id,
           ow_suffix,
           eval_start,
           eval_end,
           sys_invert_elev,
           loc_invert_elev,
           key_elevs,
           key_elev_descrips,
           key_dates,
           key_date_descrips,
           man_wl = c(),
           man_wl_dates = c(),
           color = "black", 
           adjustment = 0) {
    
    # Import monitoring data 
    smp_monitor_data <- marsFetchLevelData(
      mars_con,
      target_id = smp_id,
      ow_suffix = ow_suffix,
      start_date = eval_start,
      end_date = eval_end,
      sump_correct = FALSE) %>% 
      #Remove unnecessary cols
      select(-ow_leveldata_raw_uid, -ow_uid, -date_added) %>%
      # Add 'type' column and set all existing values to 'HOBO'
      mutate(type = 'HOBO') %>%
      # Add manual measurements
      add_row(dtime = man_wl_dates, level_ft = man_wl, type = 'manual')
      

    
    # Create vector of key depths
    key_depths <- key_elevs - sys_invert_elev
    
    # Find reference elevation (depth of system invert relative to location invert)
    ref_depth <- sys_invert_elev - loc_invert_elev
    
    ts <-
      ggplot(smp_monitor_data, aes(x = dtime, y = level_ft - ref_depth, color = type, shape = type)) +
      geom_point(size = 1) + 
      ggtitle(paste0(smp_id, " ", ow_suffix, " Response Plot")) +
      ylab("Water Level (ft)") +
      xlab("Date") +
      scale_y_continuous(breaks = scales::breaks_width(1))
    scale_x_datetime(
      date_breaks = "6 months",
      minor_breaks = "3 months",
      date_labels = "%F"
    )
    
    # Add horizontal lines for key depths
    if (length(key_depths) > 0 & length(key_elev_descrips) > 0) {
      for (i in 1:length(key_depths)) {
        ts <-
          ts + geom_hline(
            
            yintercept = key_depths[i],
            color = "black",
            size = 0.4,
            linetype = "dashed"
          ) #+
         # annotate("text",
         #          x = min(smp_monitor_data$dtime)-months(2),
         #          y = key_depths[i] + 0.2,
         #          label = key_elev_descrips[i],
         #          hjust = 0)
      }
    }

    
    # Add vertical lines for key dates
    if (length(key_dates) > 0 & length(key_date_descrips) > 0) {
      for (i in 1:length(key_dates)) {
        ts <-
          ts + geom_vline(
            xintercept = key_dates[i],
            color = "blue",
            size = 0.8,
            linetype = "dashed"
          ) #+
        annotate(
          "text",
          x = key_dates[i] - days(24),
          y = min(key_depths) + 0.2,
          angle = 90,
          label = key_date_descrips[i],
          hjust = 0
        )
      }
    }
    print(ts)
  }

# Arguments that are the same for all calls to plot_ts
smp_id <- '63182'
sys_invert_elev <- 25.24
eval_start <- '2020-01-21'
eval_end <- '2026-02-11'
key_dates <-
  c(
    '2020-01-21',
    '2020-10-27',
    '2021-05-27',
    '2021-12-22',
    '2024-05-26',
    '2024-12-10',
    '2025-07-14',
    '2026-02-11'
  ) %>% lubridate::as_datetime()
lubridate::hour(key_dates) = 12  # Assume all redeployments occurred at noon
key_date_descrips <-
  c(
    'initial deployment',
    'redeployment',
    'redeployment',
    'redeployment',
    'redeployment',
    'redeployment',
    'redeployment',
    'collection'
  )

# CS1
cs1_suffix <- 'CS1'
cs1_elevs <-
  c(23.78, 25.24, 25.72, 26.06, 30.72, 31.22) # Not including top of CS1 at 38.01
cs1_elev_descrips <-
  c(
    'bottom of CS',
    'bottom of stone',
    'underdrain invert',
    'weir orifice invert',
    'top of weir',
    'top of stone'
  )
cs1_invert_elev <- cs1_elevs[1]
cs1_man_wl_dates <-
  c(
    '2020-01-21',
    '2021-05-27',
    '2021-12-22',
    '2024-05-06',
    '2024-12-10',
    '2025-07-14',
    '2026-02-11'
  ) %>% lubridate::as_datetime()
# Assume all manual measurements taken at noon
lubridate::hour(man_wl_dates) = 12
cs1_man_wl <- c(1.71, 1.98, 1.66, 1.71, 1.73, 1.74, 1.71)

cs1_plot <-
  plot_ts(
    smp_id = smp_id,
    ow_suffix = cs1_suffix,
    eval_start = eval_start,
    eval_end = eval_end,
    sys_invert_elev = sys_invert_elev,
    loc_invert_elev = cs1_invert_elev,
    key_elevs = cs1_elevs,
    key_elev_descrips = cs1_elev_descrips,
    key_dates = key_dates,
    key_date_descrips = key_date_descrips,
    man_wl = cs1_man_wl,
    man_wl_dates = cs1_man_wl_dates
  )




ggsave(paste0(smp_id, '/output/cs1_ts_', eval_end, '.png'))


# OW1
ow1_suffix <- "OW1"
ow1_elevs <- c(25.51, 25.24, 25.72, 26.06, 31.22) # Removed top of OW1 39.01
ow1_elev_descrips <- c('bottom of OW1', 
                       'bottom of stone', 
                       'underdrain invert',
                       'weir invert',
                       'top of stone')
ow1_invert_elev <- ow1_elevs[1]
ow1_man_wl_dates <-
  c(
    '2020-01-21',
    '2020-10-27',
    '2021-05-27',
    '2021-12-22',
    '2024-05-26',
    '2024-12-10',
    '2025-07-14',
    '2026-02-11'
  ) %>% lubridate::as_datetime()
# Assume all manual measurements taken at noon
lubridate::hour(man_wl_dates) = 12
ow1_man_wl <- c(0, 0, 0.57, 0, 0.21, 0, 0, 0)

ow1_plot <-
  plot_ts(
    smp_id = smp_id,
    ow_suffix = ow1_suffix,
    eval_start = eval_start,
    eval_end = eval_end,
    sys_invert_elev = sys_invert_elev,
    loc_invert_elev = ow1_invert_elev,
    key_elevs = ow1_elevs,
    key_elev_descrips = ow1_elev_descrips,
    key_dates = key_dates,
    key_date_descrips = key_date_descrips,
    man_wl = ow1_man_wl,
    man_wl_dates = ow1_man_wl_dates
  )
ggsave(paste0(smp_id, '/output/ow1_ts_', eval_end, '.png'))

# OW2
ow2_suffix <- 'OW2'
ow2_elevs <- c(25.51, 25.24, 25.72, 26.06, 31.22) #Removed top of OW 31.14
ow2_elev_descrips <- c('bottom of OW2', 
                       'bottom of stone', 
                       'underdrain invert',
                       'weir orifice',
                       'top of stone')
ow2_invert_elev <- ow2_elevs[1]
ow2_man_wl_dates <-
  c(
    '2020-01-21',
    '2020-10-27',
    '2021-05-27',
    '2021-12-22',
    '2024-05-26',
    '2024-12-10',
    '2025-07-14',
    '2026-02-11'
  ) %>% lubridate::as_datetime()
# Assume all manual measurements taken at noon
lubridate::hour(man_wl_dates) = 12
ow2_man_wl <- c(0, 0, 0.72, 0.31, 0.41, 0, 0.18, 0)

ow2_plot <-
  plot_ts(
    smp_id = smp_id,
    ow_suffix = ow2_suffix,
    eval_start = eval_start,
    eval_end = eval_end,
    sys_invert_elev = sys_invert_elev,
    loc_invert_elev = ow2_invert_elev,
    key_elevs = ow2_elevs,
    key_elev_descrips = ow2_elev_descrips,
    key_dates = key_dates,
    key_date_descrips = key_date_descrips,
    man_wl = ow2_man_wl,
    man_wl_dates = ow2_man_wl_dates
  )
ggsave(paste0(smp_id, '/output/ow2_ts_', eval_end, '.png'))

# Close database connection
poolClose(mars_con)

