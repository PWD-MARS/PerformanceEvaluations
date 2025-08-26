#### Generate timeseries plots from monitoring data at Parkside 63761

#### Setup

# Library necessary packages
library(tidyverse)
library(ggplot2)
library(pwdgsi)
library(odbc)
library(DBI)
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
           key_elevs,
           key_elev_descrips,
           key_dates,
           key_date_descrips) {
    # Import monitoring data and create dtime_est col
    smp_monitor_data <- marsFetchLevelData(
      mars_con,
      target_id = smp_id,
      ow_suffix = ow_suffix,
      start_date = eval_start,
      end_date = eval_end,
      sump_correct = FALSE
    ) %>%
      mutate(dtime_est = with_tz(dtime, tzone = 'EST'))
    
    # Create vector of key depths
    key_depths <- key_elevs - sys_invert_elev
    
    # Find reference elevation (depth of system invert relative to location invert)
    ref_depth <- sys_invert_elev - key_elevs[1]
    
    ts <-
      ggplot(smp_monitor_data, aes(x = dtime_est, y = level_ft - ref_depth)) +
      geom_line(color = "black") +
      ggtitle(paste0(smp_id, " ", ow_suffix, " Response Plot")) +
      ylab("Water Level (ft)") +
      xlab("Date") +
      scale_y_continuous(breaks = scales::breaks_width(2))
    scale_x_datetime(
      date_breaks = "6 months",
      minor_breaks = "3 months",
      date_labels = "%D"
    )
    
    # Add horizontal lines for key depths
    if (length(key_depths) > 0 & length(key_elev_descrips) > 0) {
      for (i in 1:length(key_depths)) {
        ts <-
          ts + geom_hline(
            yintercept = key_depths[i],
            color = "orange",
            size = 0.8,
            linetype = "dashed"
          ) +
        annotate("text",
                 x = min(smp_monitor_data$dtime_est),
                 y = key_depths[i] - 0.2,
                 label = key_elev_descrips[i],
                 hjust = 0)
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
eval_start <- '2023-10-17'
eval_end <- '2025-02-28'
key_dates <- c() %>% lubridate::as_datetime()
key_date_descrips <- c()

# CS1
cs1_suffix <- 'CS1'
cs1_elevs <- c(109.0, 112.1, 111.9, 112.75, 114.5, 117.4, 118.7)
cs1_elev_descrips <-
  c(
    "bottom of CS1",
    "dist pipe invert",
    '1" orifice invert',
    '2" orifice invert',
    "weir notch elevation",
    "top of CS1 weir",
    "CS1 rim"
  )
cs1_plot <-
  plot_ts(
    smp_id,
    cs1_suffix,
    eval_start,
    eval_end,
    sys_invert_elev,
    cs1_elevs,
    cs1_elev_descrips,
    key_dates,
    key_date_descrips
  )
ggsave(paste0("63761/output/cs1_ts_", eval_end, ".png"))


# OW1
ow1_suffix <- "OW1"
ow1_elevs <- c(111.98, 119.4)
ow1_elev_descrips <- c("bottom of OW1", "top of OW1")
ow1_plot <-
  plot_ts(
    smp_id,
    ow1_suffix,
    eval_start,
    eval_end,
    sys_invert_elev,
    ow1_elevs,
    ow1_elev_descrips,
    key_dates,
    key_date_descrips
  )
ggsave(paste0("63761/output/ow1_ts_", eval_end, ".png"))
