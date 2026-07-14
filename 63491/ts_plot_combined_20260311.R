#### Script to generate timeseries plots from monitoring data at Summer St 63491 

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
           ow_suffix = 'OW1',
           cs_suffix = 'CS1',
           eval_start,
           eval_end,
           sys_invert_elev,
           ow_invert_elev,
           cs_invert_elev,
           key_elevs,
           key_elev_descrips,
           key_dates = c(),
           key_date_descrips = c(),
           ow_color = 'dodgerblue', 
           cs_color = 'darkorange',
           ow_adj = 0,
           cs_adj = 0,
           ow_min_wl = -100,
           cs_min_wl = -100) {
    
    # Create vector of key depths
    key_depths <- key_elevs - sys_invert_elev
    # Find reference elevations (depth of system invert relative to location invert)
    ow_ref_depth <- sys_invert_elev - ow_invert_elev
    cs_ref_depth <- sys_invert_elev - cs_invert_elev
    
    # Import OW monitoring data 
    
    ow_monitor_data <- marsFetchLevelData(
      mars_con,
      target_id = smp_id,
      ow_suffix = ow_suffix,
      start_date = eval_start,
      end_date = eval_end,
      sump_correct = FALSE) 
    # Adjust for ref depth and adjustment, and filter out water levels below threshold 
    ow_monitor_data <- ow_monitor_data %>%
      mutate(ow_level_ft = level_ft - ow_ref_depth + ow_adj) %>%
      mutate(ow_level_ft = replace(ow_level_ft, ow_level_ft < ow_min_wl, NA))

    # Import CS monitoring data 
    
    cs_monitor_data <- marsFetchLevelData(
      mars_con,
      target_id = smp_id,
      ow_suffix = cs_suffix,
      start_date = eval_start,
      end_date = eval_end,
      sump_correct = FALSE) 
    # Adjust for ref depth and adjustment, and filter out water levels below threshold 
    cs_monitor_data <- cs_monitor_data %>%
      mutate(cs_level_ft = level_ft - cs_ref_depth + cs_adj) %>%
      mutate(cs_level_ft = replace(cs_level_ft, cs_level_ft < cs_min_wl, NA))
    
    # Combine into single df
    monitor_data <- inner_join(ow_monitor_data, cs_monitor_data, by = 'dtime') %>% 
      select('dtime', 'ow_level_ft', 'cs_level_ft')
    rm(ow_monitor_data, cs_monitor_data) 
    
    # Reshape data to make location a second col
    reshaped_data <- monitor_data %>%
      pivot_longer(-dtime, names_to = "location", values_to = "level_ft")  
    
    ts <-
      ggplot(reshaped_data, aes(x = dtime, y = level_ft, color = location)) +
      scale_color_manual(labels = c('CS1', 'OW1'), values = c(cs_color, ow_color)) + 
      geom_point(size = 0.5) + 
      ggtitle(paste0(smp_id, " Combined Response Plot")) +
      ylab("Water Level (ft)") +
      xlab("Date") +
      scale_y_continuous(breaks = scales::breaks_width(1)) + 
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
          ) +
          annotate("text",
                   size = unit(3, 'pt'),
                   x = min(smp_monitor_data$dtime)-months(3),
                   y = key_depths[i] + 0.05,
                   label = key_elev_descrips[i],
                   hjust = 0,
                   vjust = 0,
                   lineheight = 0.8)
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
          ) +
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

############################################################################
smp_id <- '63491'
ow_suffix = 'OW1'
cs_suffix = 'CS1'
eval_start <- '2024-11-01'
eval_end <- '2026-03-11'
sys_invert_elev <- 69.25
ow_invert_elev <- 69.87
cs_invert_elev <- 67.28
key_elevs <- c(67.28, 69.25, 69.87, 71.00, 72.25, 72.75)
key_elev_descrips <- c('Bottom of CS1','Bottom of Stone/ \n 2.125" Orifice Invert','Bottom of OW1','6" Orifice Invert','Top of Weir', 'Top of Stone')
key_dates <- c(0)
key_date_descrips <- c()
ow_color = 'dodgerblue'
cs_color = 'darkorange'
ow_adj = 0
cs_adj = 0
ow_min_wl = 0.7
cs_min_wl = -1


comb_plot <-
  plot_ts(smp_id = smp_id,
          ow_suffix = ow_suffix,
          cs_suffix = cs_suffix,
          eval_start = eval_start,
          eval_end = eval_end,
          sys_invert_elev = sys_invert_elev,
          ow_invert_elev = ow_invert_elev,
          cs_invert_elev = cs_invert_elev,
          key_elevs = key_elevs,
          key_elev_descrips = key_elev_descrips,
          key_dates = key_dates,
          key_date_descrips = key_date_descrips,
          ow_color = ow_color, 
          cs_color = cs_color,
          ow_adj = ow_adj,
          cs_adj = cs_adj,
          ow_min_wl = ow_min_wl,
          cs_min_wl = cs_min_wl
  )
ggsave(paste0(smp_id, '/output/comb_ts_', eval_end, '.png'))

# Close database connection
poolClose(mars_con)

