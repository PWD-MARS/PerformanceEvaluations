### Script to create plots for each rain event over a monitoring period

#### 0.1 library necessary packages #### 
library(tidyverse); library(ggplot2)
library(pwdgsi); library(odbc)
library(DBI); library(lubridate)
library(magrittr)

#### 0.2 database connection ####
mars_con <- odbc::dbConnect(drv  = odbc::odbc(),
                            dsn  = "mars14_data")

#### 1.0 Pick SMP, OW, Date range, set storage depth ####

smp_id <- '411-1-1'
ow_suffix <- 'CS1'
eval_start <- '2021-02-23'
eval_end <- '2025-04-09'
storage_depth_ft <- 4.07; # Weir depth relative to bottom of stone

# monitoring period
smp_monitor_data <- marsFetchLevelData(mars_con,
                                       target_id = smp_id,
                                       ow_suffix = ow_suffix,
                                       start_date = eval_start,
                                       end_date = eval_end,
                                       sump_correct = TRUE)

# find the dates of the first CWL datapoint and the last CWL datapoint
first_date <- min(smp_monitor_data$dtime_est) %>% as_date()
last_date <- max(smp_monitor_data$dtime_est) %>% as_date()

# get list of events
smp_events <- marsFetchRainEventData(mars_con,
                                     source = 'radar',
                                     target_id = smp_id,
                                     start_date = first_date,
                                     end_date = last_date)

##### 1.1 Set save folder/create save folder ####

## Create a folder if needed
save_dir<- paste0("output/event_plots_", eval_end) 
if(dir.exists(save_dir) == FALSE){
  dir.create(save_dir) 
}

#### 2.0 Loop To Create Graphs ####

for(i in 1:length(smp_events$radar_event_uid)){
  
  # new, truncated functions
  ##### 2.1 Handling potential errors ####
  tryCatch(expr = {plot_x <- marsEventCombinedPlot(con = mars_con,
                                                   event_date = smp_events$eventdatastart_est[i],
                                                   smp_id = smp_id,
                                                   ow_suffix = ow_suffix,
                                                   source = 'radar',
                                                   storage_depth_ft = storage_depth_ft)
  },
  error = function(cond){
    print(paste0('Error found at event: ',smp_events$radar_event_uid[i]))
    print(cond)
  }
  )
  
  #### 2.2 Save Plot ####
  # makes sure event has plot
  if(exists("plot_x")){
    #unique plot file
    plot_file <- paste0(save_dir, "/", smp_id,"_",ow_suffix,"_",smp_events$radar_event_uid[i],".png")
    
    #save file
    marsSavePlot(in_plot = plot_x,
                 plot_type = 'combined',
                 filename = plot_file)
  }
  
  rm(plot_x)
}

#### 3.0 Disconnect from the Database ####
mars_con <- odbc::dbConnect(drv  = odbc::odbc(),
                            dsn  = "mars14_data")

