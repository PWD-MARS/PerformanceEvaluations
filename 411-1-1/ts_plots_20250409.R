#### Script to generate timeseries plots from monitoring data at Ferko 411

#### Setup

# Library necessary packages
library(tidyverse) 
library(ggplot2)
library(pwdgsi)
library(odbc)
library(DBI)
library(lubridate)
library(magrittr)

# Create database connection
mars_con <- odbc::dbConnect(drv  = odbc::odbc(),
                            dsn  = "mars14_data")

# Function to create timeseries plot

plot_ts <- function(smp_id, ow_suffix, eval_start, eval_end, sys_invert_elev, 
                    key_elevs, key_elev_descrips, key_dates, key_date_descrips) {
  
  # Import monitoring data
  smp_monitor_data <- marsFetchLevelData(mars_con,
                                         target_id = smp_id,
                                         ow_suffix = ow_suffix,
                                         start_date = eval_start,
                                         end_date = eval_end,
                                         sump_correct = FALSE)
  
  # Create vector of key depths 
  key_depths <- key_elevs - sys_invert_elev
  
  # Find reference elevation (depth of system invert relative to location invert) 
  ref_depth <- sys_invert_elev - key_elevs[1]
  
  ts <- ggplot(smp_monitor_data, aes(x = dtime_est, y = level_ft - ref_depth)) + 
    geom_line(color = "black") + 
    ggtitle(paste0(smp_id, " ", ow_suffix, " Response Plot")) + 
    ylab("Water Level (ft)") + 
    xlab("Date") + 
    scale_y_continuous(breaks = scales::breaks_width(2))
  scale_x_datetime(date_breaks = "6 months", minor_breaks = "3 months", date_labels = "%D") 
  
  # Add horizontal lines for key depths
  if(length(key_depths) > 0 & length(key_elev_descrips) > 0){
    for(i in 1:length(key_depths)){
      ts <- ts + geom_hline(yintercept = key_depths[i],  color = "orange", size = 0.8, linetype = "dashed") #+
      #annotate("text", x = tail(key_dates, n=1)+months(18), y = key_depths[i] - 0.2, 
      #label = key_elev_descrips[i], hjust = 0)
    }
  }
  
  # Add vertical lines for key dates
  if(length(key_dates) > 0 & length(key_date_descrips) > 0){
    for(i in 1:length(key_dates)){
      ts <- ts + geom_vline(xintercept = key_dates[i], color = "blue", size = 0.8, linetype = "dashed") #+
      #annotate("text", x = key_dates[i]-days(24), y = min(key_depths)+0.2, 
      #angle = 90, label = key_date_descrips[i], hjust = 0)
    }
  }
  print(ts)
}

# Arguments that are the same for all calls to plot_ts
smp_id <- '411-1-1'
sys_invert_elev <- 51.05
eval_start <- '2021-02-23'
eval_end <- '2025-04-09'
key_dates <- c("2021-05-03", "2021-12-01", "2023-02-22") %>% lubridate::as_datetime()
key_date_descrips <- c("trash guard cleaned", "trash guard cleaned", "trash guard removed")
wd <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/Ferko_411/Data Analysis/202403_Perf Eval"

# CS1
cs1_suffix <- 'CS1'
cs1_elevs <- c(47.16, 51.17, 55.12, 59.2)
cs1_elev_descrips <- c("bottom of CS1", "CS1 dist pipe invert", "top of CS1 weir", "top of CS1")
cs1_plot <- plot_ts(smp_id, cs1_suffix, eval_start, eval_end, sys_invert_elev, 
                    cs1_elevs, cs1_elev_descrips, key_dates, key_date_descrips)
ggsave(paste0("output/cs1_ts_", eval_end, ".png"))

# CS2
cs2_suffix <- "CS2"
cs2_elevs <- c(49.92, 51.75, 59.42)
cs2_elev_descrips <- c("bottom of CS2", "bottom of manifold", "top of CS2")
cs2_plot <- plot_ts(smp_id, cs2_suffix, eval_start, eval_end, sys_invert_elev, 
                    cs2_elevs, cs2_elev_descrips, key_dates, key_date_descrips)
ggsave(paste0("output/cs2_ts_", eval_end, ".png"))

# OW2
ow2_suffix <- "OW2"
ow2_elevs <- c(49.64, 51.05, 51.75, 59.33)
ow2_elev_descrips <- c("bottom of OW2", "bottom of stone", "bottom of manifold", "top of OW2")
ow2_plot <- plot_ts(smp_id, ow2_suffix, eval_start, eval_end, sys_invert_elev, 
                    ow2_elevs, ow2_elev_descrips, key_dates, key_date_descrips)
ggsave(paste0("output/ow2_ts_", eval_end, ".png"))

#### 3.0 Disconnect from the Database ####
mars_con <- odbc::dbConnect(drv  = odbc::odbc(),
                            dsn  = "mars14_data")

