#### Script to generate timeseries plots from monitoring data at Parkside 63761

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
           color = "black") {
    
    # Import monitoring data 
    smp_monitor_data <- marsFetchLevelData(
      mars_con,
      target_id = smp_id,
      ow_suffix = ow_suffix,
      start_date = eval_start,
      end_date = eval_end,
      sump_correct = FALSE) 
    
    # Create vector of key depths
    key_depths <- key_elevs - sys_invert_elev
    
    # Find reference elevation (depth of system invert relative to location invert)
    ref_depth <- sys_invert_elev - loc_invert_elev
    
    ts <-
      ggplot(smp_monitor_data, aes(x = dtime, y = level_ft - ref_depth)) +
      geom_line(color = color) +
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
smp_id <- '63761'
sys_invert_elev <- 111.5
eval_start <- '2023-10-19'
eval_end <- '2025-02-27'
key_dates <- c() %>% lubridate::as_datetime()
key_date_descrips <- c()

# CS1
cs1_suffix <- 'CS1'
cs1_elevs <- c(111.5, 111.9, 112.0,
               112.8, 114.5, 117.0, 117.4)
cs1_elev_descrips <-
  c(
    "bottom of stone",
    '1" orifice invert',
    'Distributin pipe invert',
    '2" orifice invert',
    "weir notch elevation",
    "top of stone",
    "top of CS1 weir"
  )
cs1_invert_elev <- 109.0
cs1_plot <-
  plot_ts(
    smp_id,
    cs1_suffix,
    eval_start,
    eval_end,
    sys_invert_elev,
    cs1_invert_elev,
    cs1_elevs,
    cs1_elev_descrips,
    key_dates,
    key_date_descrips,
    "darkorange2"
  )
ggsave(paste0("63761/output/cs1_ts_", eval_end, ".png"))

#CS1 zoomed plot - shows only bot of stone to weir notch
cs1_zoomed_elevs <- c(111.5, 111.9, 112.0,
               112.8, 114.5)
cs1_zoomed_elev_descrips <-
  c(
    "bottom of stone",
    '1" orifice invert',
    'Distributin pipe invert',
    '2" orifice invert',
    "weir notch elevation"
  )
cs1_plot <-
  plot_ts(
    smp_id,
    cs1_suffix,
    eval_start,
    eval_end,
    sys_invert_elev,
    cs1_invert_elev,
    cs1_zoomed_elevs,
    cs1_zoomed_elev_descrips,
    key_dates,
    key_date_descrips,
    "darkorange2"
  )
ggsave(paste0("63761/output/cs1_ts_", eval_end, "_zoomed.png"))

# OW1
ow1_suffix <- "OW1"
ow1_elevs <- c(111.5, 111.7, 117.0)
# Note: Bot. of OW1 calculated as 111.98 based on surveyed rim elevation and
# measured well depth. Changed to 111.7 ft to force CS1 and OW1 water depths to
# match.
ow1_elev_descrips <- c('bottom of stone', 'bottom of OW1', 
                       'top of stone')
ow1_invert_elev <- 111.98
ow1_plot <-
  plot_ts(
    smp_id,
    ow1_suffix,
    eval_start,
    eval_end,
    sys_invert_elev,
    ow1_invert_elev,
    ow1_elevs,
    ow1_elev_descrips,
    key_dates,
    key_date_descrips,
    "dodgerblue"
  )
ggsave(paste0("63761/output/ow1_ts_", eval_end, ".png"))

# Close database connection
poolClose(mars_con)

