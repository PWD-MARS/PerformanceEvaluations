library(tidyverse)
library(lubridate)
library(RPostgres)
library(stringr)
library(pwdgsi)
library(padr)

mars <- dbConnect(drv = RPostgres::Postgres(),
                  host = "PWDMARSDBS1",
                  port = 5434,
                  dbname = "mars_prod",
                  user= Sys.getenv("admin_uid"),
                  password = Sys.getenv("admin_pwd"))

# 1.0 Pull Data ----
  ## 1.1 Monitoring Locations ----
  ow_query <- "select ow_uid, smp_id, ow_suffix from fieldwork.tbl_ow where smp_id = '61354'"
  ow <- dbGetQuery(mars, ow_query)
  
  ## 1.2 Level Series ----
  ts_query <-paste("select * from data.tbl_ow_leveldata_raw where ow_uid in (",
                   paste(ow$ow_uid, collapse = ","), ") order by dtime asc")
  ts <- dbGetQuery(mars, ts_query) |>
    left_join(ow) |>
    force_tz(tzone = "America/New_York") |>
    filter(!is.na(dtime)) |>
    filter(ow_suffix != "CO2")
  
  # ###Note: Before august 2019, OW1 and OW2 were mistakenly marked as CO1 and CO2
  #   # And were QAed in the DB as such. Manually recoding that data here:
  # 
  # recoded <- filter(ts, dtime < ymd("2019-08-01", tz = "America/New_York") &
  #                   ow_suffix %in% c("CO1", "CO2")) |>
  #   mutate(new_ow_uid = ifelse(ow_uid == 1030, 1242, 1241),
  #          new_ow_suffix = ifelse(ow_uid == 1030, "OW1", "OW2")) |>
  #   select(ow_leveldata_raw_uid, dtime, level_ft, smp_id, date_added,
  #          ow_suffix = new_ow_suffix, ow_uid = new_ow_uid)
  # 
  # ts_preserved <- filter(ts, !(dtime < ymd("2019-08-01", tz = "America/New_York") &
  #                              ow_suffix %in% c("CO1", "CO2")))
  # 
  # ts <- bind_rows(ts_preserved, recoded) |> arrange(dtime)
  # 
  
  ## 1.3 Raw data ----
  ######### Rich has QAed the missing data
  # rawfolder <- "//pwdoows/oows/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/z_Private Monitoring Sites/Dependable Distrib_61354_2013-SITE-2401-01/Raw Data"
  # files <- list.files(rawfolder, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  # rawfiles <- data.frame(filepath = files) |>
  #   filter(str_detect(filepath, pattern = "[\\(]|SRT|BARO|Archive", negate = TRUE)) |>
  #   mutate(ow_suffix = str_extract(filepath, pattern = "(OW|CS|CO)\\d{1}"))

  # ts_raw <- data.frame(NULL)
  # for(i in 1:nrow(rawfiles)){
  #   file <- read_csv(rawfiles$filepath[i], skip = ifelse(i == 1, 0, 1), show_col_types = FALSE) |>
  #     select(dtime = 2, pres_psi = 3) |>
  #     mutate(dtime = mdy_hms(dtime),
  #            ow_suffix = rawfiles$ow_suffix[i],
  #            file = rawfiles$filepath[i])
  # 
  #   print(head(file))
  # 
  #   ts_raw <- bind_rows(ts_raw, file)
  # }

  ## 1.4 Rainfall data ----
  rain <- marsFetchRainfallData(mars, 
                                target_id = "61354", 
                                source = "gage",
                                start_date = min(min(ts$dtime), min(ts$dtime)),
                                end_date = today()) |>
    mutate(dtime = force_tz(dtime, tzone = "America/New_York"))
  
  ## 1.5 Rain event data ----
  events <- marsFetchRainEventData(mars, target_id = "61354", source = "gage",
                                   start_date = min(min(ts$dtime), min(ts$dtime)),
                                   end_date = today()) |>
    mutate(eventdatastart = force_tz(eventdatastart, tzone = "America/New_York"),
           eventdataend = force_tz(eventdataend, tzone = "America/New_York"))
  
  ## 1.6 Overflow characteristics
  ow1_bottom_elev = 162
  ow1_top_elev = 165.6
  ow1_installheight <- dbGetQuery(mars,
    "SELECT installation_height_ft FROM fieldwork.viw_ow_plus_measurements where ow_uid = 1242") |>
    pull(installation_height_ft)
  ow1_depth <- ow1_top_elev - (ow1_bottom_elev + ow1_installheight)
  
  cs1_bottom_elev = 161.92
  cs1_weir_elev = 165.12
  cs1_installheight <-  dbGetQuery(mars,
      "SELECT installation_height_ft FROM fieldwork.viw_ow_plus_measurements where ow_uid = 1252") |>
    pull(installation_height_ft)
  cs1_depth <- cs1_weir_elev - (cs1_bottom_elev + cs1_installheight)
  
  ow3_bottom_elev = 162
  ow3_top_elev = 165.6
  ow3_installheight <- dbGetQuery(mars,
                                  "SELECT installation_height_ft FROM fieldwork.viw_ow_plus_measurements where ow_uid = 1032") |>
    pull(installation_height_ft)
  ow3_depth <- ow3_top_elev - (ow3_bottom_elev + ow3_installheight)
  
  ow2_bottom_elev = 162
  ow2_top_elev = 165.6
  ow2_installheight <- dbGetQuery(mars,
                                  "SELECT installation_height_ft FROM fieldwork.viw_ow_plus_measurements where ow_uid = 1241") |>
    pull(installation_height_ft)
  ow2_depth <- ow2_top_elev - (ow2_bottom_elev + ow2_installheight)
  
  #Cleanout elbow not called out on plan set. Estimate its elevation by subtracting its well depth from OW1, it's nearest neighbor
  co1_welldepth <- dbGetQuery(mars,
                              "SELECT well_depth_ft FROM fieldwork.viw_ow_plus_measurements where ow_uid = 1030") |>
    pull(well_depth_ft)
  ow1_welldepth <- dbGetQuery(mars,
                              "SELECT well_depth_ft FROM fieldwork.viw_ow_plus_measurements where ow_uid = 1242") |>
    pull(well_depth_ft)
  co1_bottom_elev <- 162 + (ow1_welldepth - co1_welldepth)
  co1_installheight <- dbGetQuery(mars,
                                  "SELECT installation_height_ft FROM fieldwork.viw_ow_plus_measurements where ow_uid = 1030") |>
    pull(installation_height_ft)
  co1_depth <- cs1_weir_elev - (co1_bottom_elev + co1_installheight) # CO is in direct hydraulic connection with CS. imperfect weir proxy but decent enough
  
# 2.0 Data Coverage ----
  ## 2.1 Coverage plot ----
  datesofnote <- data.frame(date = c(ymd("2018-08-01"), 
                                     ymd("2019-08-01")),
                            event = c("Pipe Jetting", 
                                      "Pipe Jetting"))
  
  colors <- c("Pipe Jetting" = "#000000")

  cp <- ggplot() +
    geom_point(data = ts, aes(x = dtime, y = ow_suffix)) +
    geom_vline(data = datesofnote, aes(xintercept = date, col = event)) +
    theme(axis.title.y = element_text(size = rel(1.2), color = "black"),
      axis.text.x = element_text(size = rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
      axis.text.y = element_text(size = rel(1.2), color = "black"), # set font size and color of y axis text
      panel.background =  element_rect(fill = "white", colour = NA), # set white background
      panel.border =      element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
      panel.grid.minor =  element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines)
      legend.position = c(0.8, 0.3),
      legend.background = element_rect(linewidth=0.5,
                         linetype="solid",
                         colour ="gray30")) +
    labs(color = "Maintenance Event") +
    ggtitle("Dependable Distribution 61354 Data Coverage") +
    xlab("Date of Monitoring") +
    ylab("Monitoring Location")

  ggsave(cp, file = "cp2.png", width = 8, height = 6, units = "in")
  # 
  #Count events within coverage periods
  #Use ts_raw since there is more data there than in the DB
  covered_events <- data.frame(NULL)
  for(i in unique(ow$ow_suffix)){
    ow <- filter(ts, ow_suffix == i)
    events_ow <- filter(events, eventdatastart %in% ow$dtime) |>
      mutate(ow_suffix = i)
    
    covered_events <- bind_rows(covered_events, events_ow)
  }
  
  #Table for pre and post jetting
  group_by(covered_events, ow_suffix) |>
    summarize(total_events = n(),
              before_jetting = sum(eventdatastart < ymd("2019-08-01")),
              after_jetting = sum(eventdatastart >= ymd("2019-08-01")))
  
  ##2.2 Full time series plots ----
  ts_plot <- rowwise(ts) |>
    mutate(level_ft_zerofloor = ifelse(level_ft < 0, 0, level_ft))
  fullseries <- ggplot() +
    geom_line(data = ts_plot,
              aes(x = dtime, y = level_ft_zerofloor, color = ow_suffix)) +
    geom_vline(data = datesofnote,
               aes(xintercept = date),
               linewidth = 2) +
    geom_label(data = datesofnote,
               aes(x = date,
                   y = c(2, 3),
                   label = event),
               size = rel(5.5),
               fill = "white") +
    labs(color = "Well Name",
         x = "Sensor Time Series",
         y = "Recorded Water Level (ft)",
         title = "Full Time Series at 61354",
         subtitle = "Negative sensor readings set equal to zero") +
    theme(plot.title = element_text(size = rel(3), color = "black"),
          plot.subtitle = element_text(size = rel(2), color = "black"),
          axis.title = element_text(size = rel(2.2), color = "black"),
          axis.text.x = element_text(size = rel(2), color = "black"), # set font size and color of x axis text #size previously set to 14
          axis.text.y = element_text(size = rel(2), color = "black"), # set font size and color of y axis text
          panel.background =  element_rect(fill = "white", colour = NA), # set white background
          panel.border =      element_rect(fill = NA, colour="black"), # set black border
          panel.grid.major =  element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
          panel.grid.minor =  element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.background = element_rect(linewidth=0.5, linetype="solid", colour ="gray30"),
          legend.title = element_text(size = rel(1.2), color = "black"),
          legend.text = element_text(size = rel(1.2), color = "black")) +
    guides(color = guide_legend(override.aes = list(linewidth = 5)))
  ggsave(filename = "61354_fullseries.png", width = 12, height = 8, units = "in")
  
  # 3.0 Event Plots ----
 
metrics <- data.frame() 
for(i in 1:nrow(events)){
  # for(i in 184:nrow(events)){
    #Event coverage interval: [eventdatastart, eventdataend + 6 hours]
    #Note: 6 hour interevent time defines the start of a new event, so we won't
    #capture rising limb of next event
    eventinterval <- interval(start = events$eventdatastart[i] - hours(1),
                              end = events$eventdataend[i] + hours(6))
    
    #Filter data to only this event, set negative values (dry well) to zero
    ow1data <- filter(ts, ow_suffix == "OW1", dtime %within% eventinterval) |>
      rowwise() |>
      mutate(level_ft = max(0, level_ft))
             
    ow2data <- filter(ts, ow_suffix == "OW2", dtime %within% eventinterval) |>
      rowwise() |>
      mutate(level_ft = max(0, level_ft))
    
    ow3data <- filter(ts, ow_suffix == "OW3", dtime %within% eventinterval) |>
      rowwise() |>
      mutate(level_ft = max(0, level_ft))
    
    co1data <- filter(ts, ow_suffix == "CO1", dtime %within% eventinterval) |>
      rowwise() |>
      mutate(level_ft = max(0, level_ft))
    
    cs1data <- filter(ts, ow_suffix == "CS1", dtime %within% eventinterval) |>
      rowwise() |>
      mutate(level_ft = max(0, level_ft))
    
    
    print(paste("event", i, "of", nrow(events)))
    # if(nrow(cs1data) == 0){
    #   print(paste("No weir data for event", i, "- Skipping!"))
    #   next
    # } else if(nrow(ow1data) == 0 & nrow(ow3data) == 0){
    #   print(paste("No OW data for event", i, "- Skipping!"))
    #   next
    # }
    
    # if(all(eventdata$level_ft <= 0)){
    #   print("No non-negative data for this event. Skipping!")
    #   next
    # }
    
    raindata <- filter(rain, gage_event_uid == events$gage_event_uid[i])
    # how <- marsRainfallPlot(dtime = raindata$dtime,
    #                         rainfall_in = raindata$rainfall_in,
    #                         event = i)
    
  # hmm <- marsWaterLevelPlot(event = i,
  #                           structure_name = "61354 OW3",
  #                           storage_depth_ft = cs1_depth,
  #                           obs_datetime = cs1data$dtime,
  #                           obs_level_ft = cs1data$level_ft,
  #                           datetime_2 = ow1data$dtime,
  #                           level_ft_2 = ow1data$level_ft,
  #                           datetime_3 = ow3data$dtime,
  #                           level_ft_3 = ow3data$level_ft,
  #                           level_names = c("CS1", "OW1", "OW3"),
  #                           label_text = paste("Top of Weir:", round(cs1_depth, 4))) +
  #   geom_vline(xintercept = events$eventdataend[i])

    
    metrics_cs1 <- data.frame(NULL)
    if(nrow(cs1data > 0)){
      metrics_cs1 <- group_by(cs1data, ow_suffix) |>
        summarize(dtime = events$eventdatastart[i],
                  gage_event_uid = events$gage_event_uid[i],
                  eventdepth_in = events$eventdepth_in[i],
                  eventpeakintensity_inhr = events$eventpeakintensity_inhr[i],
                  overtop = round((max(level_ft) - cs1_depth) * 12, 2) > 0,
                  surcharge_in = max(round((max(level_ft) - cs1_depth) * 12, 2), 0),
                  initial_ft = level_ft[1],
                  rpsu = round((max(level_ft) - level_ft[1]) /cs1_depth * 100, 2))
    }
    
    metrics_ow1 <- data.frame(NULL)
    if(nrow(ow1data > 0)){
      metrics_ow1 <- group_by(ow1data, ow_suffix) |>
        summarize(dtime = events$eventdatastart[i],
                  gage_event_uid = events$gage_event_uid[i],
                  eventdepth_in = events$eventdepth_in[i],
                  eventpeakintensity_inhr = events$eventpeakintensity_inhr[i],
                  overtop = round((max(level_ft) - ow1_depth) * 12, 2) > 0,
                  surcharge_in = NA,
                  initial_ft = level_ft[1],
                  rpsu = round((max(level_ft) - level_ft[1]) /ow1_depth * 100, 2))
    }
    
    metrics_ow2 <- data.frame(NULL)
    if(nrow(ow2data > 0)){
      metrics_ow2 <- group_by(ow2data, ow_suffix) |>
        summarize(dtime = events$eventdatastart[i],
                  gage_event_uid = events$gage_event_uid[i],
                  eventdepth_in = events$eventdepth_in[i],
                  eventpeakintensity_inhr = events$eventpeakintensity_inhr[i],
                  overtop = round((max(level_ft) - ow2_depth) * 12, 2) > 0,
                  surcharge_in = NA,
                  initial_ft = level_ft[1],
                  rpsu = round((max(level_ft) - level_ft[1]) /ow2_depth * 100, 2))
    }
    
    metrics_ow3 <- data.frame(NULL)
    if(nrow(ow3data > 0)){
      metrics_ow3 <- group_by(ow3data, ow_suffix) |>
        summarize(dtime = events$eventdatastart[i],
                  gage_event_uid = events$gage_event_uid[i],
                  eventdepth_in = events$eventdepth_in[i],
                  eventpeakintensity_inhr = events$eventpeakintensity_inhr[i],
                  overtop = round((max(level_ft) - ow3_depth) * 12, 2) > 0,
                  surcharge_in = NA,
                  initial_ft = level_ft[1],
                  rpsu = round((max(level_ft) - level_ft[1]) /ow3_depth * 100, 2))
    }
    
    metrics_co1 <- data.frame(NULL)
    if(nrow(co1data > 0)){
      metrics_co1 <- group_by(co1data, ow_suffix) |>
        summarize(dtime = events$eventdatastart[i],
                  gage_event_uid = events$gage_event_uid[i],
                  eventdepth_in = events$eventdepth_in[i],
                  eventpeakintensity_inhr = events$eventpeakintensity_inhr[i],
                  overtop = round((max(level_ft) - co1_depth) * 12, 2) > 0,
                  surcharge_in = NA,
                  initial_ft = level_ft[1],
                  rpsu = round((max(level_ft) - level_ft[1]) /co1_depth * 100, 2))
    }
    
    # if(nrow(cs1data) > 0 & nrow(ow1data) > 0){
    #   test <- paste0("Weir Overtop?: ", metrics_cs1$overtop, "\n",
    #                  "Weir Surcharge: ", metrics_cs1$surcharge_in, " in\n",
    #                  "Weir RPSU: ", metrics_cs1$rpsu, "%\n",
    #                  "Event Depth: ", metrics_cs1$eventdepth_in, " in\n",
    #                  "Peak Intensity: ", metrics_cs1$eventpeakintensity_inhr, " in/hr")
    # 
    #   hmm <- marsCombinedPlot(event = events$gage_event_uid[i],
    #                    structure_name = "Dependable Distrubution 61354",
    #                    storage_depth_ft = cs1_depth,
    #                    obs_datetime = cs1data$dtime,
    #                    obs_level_ft = cs1data$level_ft,
    #                    datetime_2 = ow1data$dtime,
    #                    level_ft_2 = ow1data$level_ft,
    #                    level_names = c("CS1", "OW1"),
    #                    label_text = paste("Top of Weir:", round(cs1_depth, 4)),
    #                    rainfall_datetime = raindata$dtime,
    #                    rainfall_in = raindata$rainfall_in,
    #                    metricslabel = test
    #   )
    # 
    #   ggsave(paste0("plots/", events$gage_event_uid[i], "_all.png"), plot = hmm, width = 12, height = 8, units = "in")
    # }


    
    metrics <- bind_rows(metrics, metrics_cs1, metrics_ow1, metrics_ow2, metrics_ow3, metrics_co1)
}
  
# Event Counts
  event_coverage <- metrics |> 
    group_by(gage_event_uid) |> 
    summarize(hasow = any(ow_suffix %in% c("OW1", "OW2", "OW3")), 
              hasoutflow = any(ow_suffix %in% c("CS1", "CO1"))) |> 
    rowwise() |> 
    mutate(hasboth = hasow & hasoutflow)
  
  coverage_table <- data.frame(hasow = sum(event_coverage$hasow),
                               hasoutflow = sum(event_coverage$hasoutflow),
                               hasboth = sum(event_coverage$hasboth)) |>
    select(`Event has OW?` = hasow,
           `Event has CS or CO?` = hasoutflow,
           `Event has both?` = hasboth)
 
#1. Time series plots for each and all monitoring locations

#2. Plots for individual storms (already done?)
  
#3. Statistics before and after key events
   #a. RPSU boxplots by location & storm size (<=1, >1) (by year?)
  # 4.0 No Inflection ----
  ## 4.1 Scatter plot of all wells ----
  rpsu_scatter <- ggplot() +
    geom_point(data = metrics,
               aes(x = dtime, y = rpsu, shape = ow_suffix, color = ow_suffix, size = overtop)) +
    geom_vline(data = datesofnote,
               aes(xintercept = date)) +
    geom_label(data = datesofnote,
               aes(x = date,
                   y = c(60, 70),
                   label = event),
               size = rel(5.5),
               fill = "white") +
    labs(size = "Overtop?", 
         shape = "Well Name", 
         color = "Well Name",
         x = "Event Date",
         y = "Relative Percent of Storage Used",
         title = "Relative Storage Utilization by Event at 61354",
         subtitle = "CS1 and CO1 relative to weir. OWs relative to top of stone storage.") +
    theme(plot.title =element_text(size = rel(3), color = "black"),
          plot.subtitle = element_text(size = rel(2), color = "black"),
          axis.title = element_text(size = rel(2.2), color = "black"),
          axis.text.x = element_text(size = rel(2), color = "black"), # set font size and color of x axis text #size previously set to 14
          axis.text.y = element_text(size = rel(2), color = "black"), # set font size and color of y axis text
          panel.background =  element_rect(fill = "white", colour = NA), # set white background
          panel.border =      element_rect(fill = NA, colour="black"), # set black border
          panel.grid.major =  element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
          panel.grid.minor =  element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
          legend.position = c(0.7, 0.8),
          legend.direction = "horizontal",
          legend.background = element_rect(linewidth=0.5, linetype="solid", colour ="gray30"),
          legend.title = element_text(size = rel(1.2), color = "black"),
          legend.text = element_text(size = rel(1.2), color = "black")) +
    guides(color = guide_legend(override.aes = list(size = 5)),
           shape = guide_legend(override.aes = list(size = 5)))
  
  ggsave(filename = "rpsu_scatter_all.png", width = 12, height = 8, units = "in")
  
  # 5.0 By storm size ----

  metrics_box <- mutate(metrics, 
                        year = as.factor(year(dtime)), 
                        depthbin = case_when(eventdepth_in <= 0.5 ~ "(0, 0.5] in",
                                             eventdepth_in > 0.5 & eventdepth_in <= 1.0 ~ "(0.5, 1.0] in",
                                             eventdepth_in > 1.0 & eventdepth_in <= 2.0 ~ "(1.0, 2.0] in",
                                             eventdepth_in > 2.0 ~ "(2.0, Inf) in")) |>
    filter(ow_suffix != "CO1")
  
  ## 5.1 RPSU Boxplots ----
  rpsu_box_bysize <- ggplot() +
    geom_boxplot(data = metrics_box,
                 aes(x = ow_suffix, y = rpsu, color = depthbin, fill = depthbin), alpha = 0.25) + 
    labs(color = "Event Depth (inches)", 
         fill = "Event Depth (inches)",
         x = "Well Name",
         y = "Relative Percent of Storage Used",
         title = "Relative Storage Utilization by Event Depth at 61354",
         subtitle = "CS1 relative to weir. OWs relative to top of stone storage") +
    theme(plot.title = element_text(size = rel(3), color = "black"),
          plot.subtitle = element_text(size = rel(2), color = "black"),
          axis.title = element_text(size = rel(2.2), color = "black"),
          axis.text.x = element_text(size = rel(2), color = "black"), # set font size and color of x axis text #size previously set to 14
          axis.text.y = element_text(size = rel(2), color = "black"), # set font size and color of y axis text
          panel.background =  element_rect(fill = "white", colour = NA), # set white background
          panel.border =      element_rect(fill = NA, colour="black"), # set black border
          panel.grid.major =  element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
          panel.grid.minor =  element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
          legend.position = c(0.7, 0.8),
          legend.background = element_rect(linewidth=0.5, linetype="solid", colour ="gray30"),
          legend.title = element_text(size = rel(1.2), color = "black"),
          legend.text = element_text(size = rel(1.2), color = "black"))
  
  ggsave(filename = "rpsu_boxplot_all.png", rpsu_box_bysize, width = 12, height = 8, units = "in")
  
  ## 5.2 CS1 Only ----
  metrics_csonly <- filter(metrics_box, ow_suffix == "CS1")
  
  metrics_cs1_truelabels <- group_by(metrics_csonly, depthbin, overtop) |>
    summarize(count = n()) |>
    filter(overtop == TRUE)
  
  metrics_cs1_falselabels <- group_by(metrics_csonly, depthbin, overtop) |>
    summarize(count = n()) |>
    filter(overtop == FALSE)
  
  rpsu_box_overtop <- ggplot() +
    geom_boxplot(data = metrics_csonly,
                 aes(x = depthbin, y = rpsu, color = overtop, fill = overtop), alpha = 0.25) +
    geom_label(data = metrics_cs1_truelabels,
               aes(x = depthbin, y = 100, label = paste("n =", count), color = overtop), 
               fill = "white", position = position_nudge(x = 0.2), show.legend = FALSE) +
    geom_label(data = metrics_cs1_falselabels,
               aes(x = depthbin, y = 100, label = paste("n =", count), color = overtop), 
               fill = "white", position = position_nudge(x = -0.2), show.legend = FALSE) +
    labs(color = "Weir Overtop?", 
         fill = "Weir Overtop?",
         x = "Event Depth (inches)",
         y = "Relative Percent of Storage Used",
         title = "Overtopping at CS1 Weir by Event Depth at 61354") +
    theme(plot.title = element_text(size = rel(3), color = "black"),
          plot.subtitle = element_text(size = rel(2), color = "black"),
          axis.title = element_text(size = rel(2.2), color = "black"),
          axis.text.x = element_text(size = rel(2), color = "black"), # set font size and color of x axis text #size previously set to 14
          axis.text.y = element_text(size = rel(2), color = "black"), # set font size and color of y axis text
          panel.background =  element_rect(fill = "white", colour = NA), # set white background
          panel.border =      element_rect(fill = NA, colour="black"), # set black border
          panel.grid.major =  element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
          panel.grid.minor =  element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
          legend.position = c(0.8, 0.25),
          legend.background = element_rect(linewidth=0.5, linetype="solid", colour ="gray30"),
          legend.title = element_text(size = rel(1.2), color = "black"),
          legend.text = element_text(size = rel(1.2), color = "black")) +
    scale_fill_manual(values = c("cornflowerblue", "darkred")) +
    scale_color_manual(values = c("cornflowerblue", "darkred"))
  ggsave(filename = "rpsu_boxplot_cs1.png", rpsu_box_overtop, width = 12, height = 8, units = "in")
  
  #6.0 Days since Pipe Jetting  ----
  metrics_boxbyjettingyear <- mutate(metrics,
      dayssince = case_when(dtime < ymd("2018-08-01") ~ NA,
                            dtime >= ymd("2018-08-01") & dtime < ymd("2019-08-01") ~ round(difftime(dtime, ymd("2018-08-01"), units = "days"), 0),
                            dtime >= ymd("2019-08-01") ~ round(difftime(dtime, ymd("2019-08-01"), units = "days"), 0)),
      year = as.factor(year(dtime))) |>
    mutate(yearssince = as.factor(round(dayssince/365, 0))) |>
    filter(ow_suffix == "CS1") |>
    filter(yearssince != 7) # Only one event, makes the plots look weird
  
  metrics_boxbyjettingyear_truelabels <- group_by(metrics_boxbyjettingyear, yearssince, overtop) |>
    summarize(count = n()) |>
    filter(overtop == TRUE) |>
    bind_rows(data.frame(yearssince = factor(1), overtop = TRUE, count = 0))
  
  metrics_boxbyjettingyear_falselabels <- group_by(metrics_boxbyjettingyear, yearssince, overtop) |>
    summarize(count = n()) |>
    filter(overtop == FALSE)  
  
  ## 6.1 RPSU Boxplots ----
  rpsu_boxsincejetting <- ggplot() +
    geom_boxplot(data = metrics_boxbyjettingyear,
                 aes(x = yearssince,
                     y = rpsu, color = overtop, fill = overtop), alpha = 0.25) +
    geom_label(data = metrics_boxbyjettingyear_truelabels,
               aes(x = yearssince, y = 100, label = paste("n =", count), color = overtop), 
               fill = "white", position = position_nudge(x = 0.2), show.legend = FALSE) +
    geom_label(data = metrics_boxbyjettingyear_falselabels,
               aes(x = yearssince, y = 100, label = paste("n =", count), color = overtop), 
               fill = "white", position = position_nudge(x = -0.2), show.legend = FALSE) +
    labs(color = "Weir Overtop?", 
         fill = "Weir Overtop?",
         x = "Years Since Last Pipe Jetting",
         y = "Relative Percent of Storage Used",
         title = "Years Since Pipe Jetting at 61354") +
    theme(plot.title = element_text(size = rel(3), color = "black"),
          plot.subtitle = element_text(size = rel(2), color = "black"),
          axis.title = element_text(size = rel(2.2), color = "black"),
          axis.text.x = element_text(size = rel(2), color = "black"), # set font size and color of x axis text #size previously set to 14
          axis.text.y = element_text(size = rel(2), color = "black"), # set font size and color of y axis text
          panel.background =  element_rect(fill = "white", colour = NA), # set white background
          panel.border =      element_rect(fill = NA, colour="black"), # set black border
          panel.grid.major =  element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
          panel.grid.minor =  element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
          legend.position = c(0.8, 0.25),
          legend.background = element_rect(linewidth=0.5, linetype="solid", colour ="gray30"),
          legend.title = element_text(size = rel(1.2), color = "black"),
          legend.text = element_text(size = rel(1.2), color = "black")) +
    scale_fill_manual(values = c("cornflowerblue", "darkred")) +
    scale_color_manual(values = c("cornflowerblue", "darkred"))
  
  ggsave(filename = "rpsu_boxplot_years.png", width = 12, height = 8, units = "in")
  
  ## 6.2 RPSU Scatterplot ----
  rpsu_scatter_sincejetting <- ggplot() +
    geom_point(data = metrics_boxbyjettingyear,
               aes(x = dayssince, y = rpsu, color = overtop)) +
    geom_smooth(data = metrics_boxbyjettingyear,
                aes(x = dayssince, y = rpsu),
                method = lm) +    
    labs(color = "Weir Overtop?", ,
          x = "Days Since Last Pipe Jetting",
          y = "Relative Percent of Storage Used",
          title = "Days Since Pipe Jetting at 61354") +
    theme(plot.title = element_text(size = rel(3), color = "black"),
          plot.subtitle = element_text(size = rel(2), color = "black"),
          axis.title = element_text(size = rel(2.2), color = "black"),
          axis.text.x = element_text(size = rel(2), color = "black"), # set font size and color of x axis text #size previously set to 14
          axis.text.y = element_text(size = rel(2), color = "black"), # set font size and color of y axis text
          panel.background =  element_rect(fill = "white", colour = NA), # set white background
          panel.border =      element_rect(fill = NA, colour="black"), # set black border
          panel.grid.major =  element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
          panel.grid.minor =  element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
          legend.position = c(0.8, 0.25),
          legend.background = element_rect(linewidth=0.5, linetype="solid", colour ="gray30"),
          legend.title = element_text(size = rel(1.2), color = "black"),
          legend.text = element_text(size = rel(1.2), color = "black")) +
    scale_color_manual(values = c("cornflowerblue", "darkred")) +
    xlim(0, 2510) +
    guides(color = guide_legend(override.aes = list(size = 5)))
           
  ggsave(filename = "rpsu_scatter_pipejetting.png", width = 12, height = 8, units = "in")
  