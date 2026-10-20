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
ow_query <- "select ow_uid, smp_id, ow_suffix from fieldwork.tbl_ow where smp_id = '61344'"
ow <- dbGetQuery(mars, ow_query)

## 1.2 Level Series ----
ts_query <-paste("select * from data.tbl_ow_leveldata_raw where ow_uid in (",
                 paste(ow$ow_uid, collapse = ","), ") order by dtime asc")
ts <- dbGetQuery(mars, ts_query) |>
  left_join(ow) |>
  force_tz(tzone = "America/New_York") |>
  filter(!is.na(dtime))

## 1.4 Rainfall data ----
rain <- marsFetchRainfallData(mars, 
                              target_id = "61344", 
                              source = "gage",
                              start_date = min(min(ts$dtime), min(ts$dtime)),
                              end_date = today()) |>
  mutate(dtime = force_tz(dtime, tzone = "America/New_York"))
## 1.5 Rain event data ----
events <- marsFetchRainEventData(mars, target_id = "61344", source = "gage",
                                 start_date = min(min(ts$dtime), min(ts$dtime)),
                                 end_date = today()) |>
  mutate(eventdatastart = force_tz(eventdatastart, tzone = "America/New_York"),
         eventdataend = force_tz(eventdataend, tzone = "America/New_York"))


# ## 1.5 Raw data ----
# rawfolder <- "//pwdoows/oows/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/z_Private Monitoring Sites/Dependable Distrib_61344_2013-SITE-2387-01/Raw Data"
# files <- list.files(rawfolder, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
# rawfiles <- data.frame(filepath = files) |>
#   filter(str_detect(filepath, pattern = "[\\(]|SRT|BARO|Archive|_0\\.csv|V2", negate = TRUE)) |>
#   mutate(ow_suffix = str_extract(filepath, pattern = "(OW|CS|CO)\\d{1}"))
# 
# ts_raw <- data.frame(NULL)
# for(i in 1:(nrow(rawfiles) - 1)){ #The last file is messed up
#   file <- read_csv(rawfiles$filepath[i], skip = 1, show_col_types = FALSE) |>
#     select(dtime = 2, pres_psi = 3) |>
#     mutate(dtime = mdy_hms(dtime),
#            ow_suffix = rawfiles$ow_suffix[i],
#            file = rawfiles$filepath[i])
# 
#   if(i == 69){
#     file <- read_csv(rawfiles$filepath[i], skip = 1, show_col_types = FALSE) |>
#       select(dtime = 2, pres_psi = 3) |>
#       mutate(dtime = mdy_hm(dtime),
#     ow_suffix = rawfiles$ow_suffix[i],
#     file = rawfiles$filepath[i])
#  }
#   
#   print(head(file))
#   
#   ts_raw <- bind_rows(ts_raw, file)
# }
## 1.6 Overflow characteristics
  ow1_bottom_elev = 160.62
  ow1_top_elev = 164.12
  ow1_installheight <- dbGetQuery(mars,
                                  "SELECT installation_height_ft FROM fieldwork.viw_ow_plus_measurements where ow_uid = 1027") |>
    pull(installation_height_ft)
  ow1_depth <- ow1_top_elev - (ow1_bottom_elev + ow1_installheight)
  
  cs1_bottom_elev = 159.69
  cs1_weir_elev = 163.62
  # cs1_installheight <-  dbGetQuery(mars,
  #                                  "SELECT installation_height_ft FROM fieldwork.viw_ow_plus_measurements where ow_uid = 1251") |>
  #   pull(installation_height_ft)
  cs1_installheight <- (9 - 6.4 - 2.55)
  cs1_depth <- cs1_weir_elev - (cs1_bottom_elev + cs1_installheight)
  
  ow3_bottom_elev = 160.62
  ow3_top_elev = 164.12
  ow3_installheight <- dbGetQuery(mars,
                                  "SELECT installation_height_ft FROM fieldwork.viw_ow_plus_measurements where ow_uid = 1029") |>
    pull(installation_height_ft)
  ow3_depth <- ow3_top_elev - (ow3_bottom_elev + ow3_installheight)
  
  ow2_bottom_elev = 160.62
  ow2_top_elev = 164.12
  ow2_installheight <- dbGetQuery(mars,
                                  "SELECT installation_height_ft FROM fieldwork.viw_ow_plus_measurements where ow_uid = 1250") |>
    pull(installation_height_ft)
  ow2_depth <- ow2_top_elev - (ow2_bottom_elev + ow2_installheight)
  
# 2.0 Data Coverage ----
#Coverage plot
colors <- c("Raw (in files only)" = "#b24745", # red
            "QAQC (in DB)" = "#74aaff", # blue
            "Pipe Jetting" = "#000000",
            "Disconnected From Sewer" = "#ee47aa",
            "Filter Bags Installed" = "#ffd966",
            "Wetland Piping Reconstructed" = "#b247aa",
            "Parged Pipe Connections" = "#99dd99")

datesofnote <- data.frame(date = c(ymd("2018-08-01"), ymd("2020-10-01"), ymd("2020-03-01"), ymd("2022-03-01"), ymd("2022-08-01")),
                          event = c("Pipe Jetting", "Disconnected From Sewer", "Filter Bags Installed", "Wetland Piping Reconstructed", "Parged Pipe Connections"))

cp <- ggplot() + 
  # geom_point(data = ts_raw, aes(x = dtime, y = ow_suffix, col = "Raw (in files only)")) +
  geom_point(data = ts, aes(x = dtime, y = ow_suffix)) +
  geom_vline(data = datesofnote, aes(xintercept = date, col = event)) +
  scale_color_manual(values = colors) +
  theme(axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
        axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
        axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
        panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
        panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
        panel.grid.major =  ggplot2::element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
        panel.grid.minor =  ggplot2::element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines)
        legend.position = c(0.8, 0.2),
        legend.background = element_rect(linewidth=0.5,
                                         linetype="solid",
                                         colour ="gray30")) +
  labs(color = "Maintenance Event") +
  ggtitle("Dependable Distribution 61344 Data Coverage") +
  xlab("Date of Monitoring") +
  ylab("Monitoring Location")

ggsave(cp, file = "cp3.png", width = 8, height = 6, units = "in")

#TODO
# More points of interest on 61344
# Count events before and after points of interest

# #Count events within coverage periods
# #Use ts_raw since there is more data there than in the DB
# covered_events <- data.frame(NULL)
# for(i in unique(ow$ow_suffix)){
#   ow_raw <- filter(ts_raw, ow_suffix == i)
#   events_ow <- filter(events, eventdatastart %in% ow_raw$dtime) |>
#     mutate(ow_suffix = i)
#   
#   covered_events <- bind_rows(covered_events, events_ow)
# }
# 
# #Table for pre and post jetting
#   group_by(covered_events, ow_suffix) |>
#     summarize(total_events = n(),
#               `Befr Jetting` = sum(eventdatastart < ymd("2018-08-01")),
#               `Btwn Jetting and Bags` = sum(eventdatastart >= ymd("2018-08-01") & eventdatastart < ymd("2020-03-01")),
#               `Btwn Bags and Disconnection` = sum(eventdatastart >= ymd("2020-03-01") & eventdatastart < ymd("2020-08-01")),
#               `Btwn Disconnection and Piping` = sum(eventdatastart >= ymd("2020-08-01") & eventdatastart < ymd("2022-03-01")),
#               `Btwn Piping and Parging` = sum(eventdatastart >= ymd("2022-03-01") & eventdatastart < ymd("2022-08-01")),
#               `After Parging` = sum(eventdatastart >= ymd("2022-08-01")))


#TODO

ts_plot <- rowwise(ts) |>
  mutate(level_ft_zerofloor = ifelse(level_ft < 0, 0, level_ft))
for(i in unique(ts_plot$ow_suffix)){
  ts_singleton <- filter(ts_plot, ow_suffix == i)
  fullseries <- ggplot() +
    geom_line(data = ts_singleton,
              aes(x = dtime, y = level_ft_zerofloor, color = ow_suffix)) +
    # geom_line(data = ts_plot,
    #           aes(x = dtime, y = level_ft_zerofloor, color = ow_suffix)) +
    geom_vline(data = datesofnote,
               aes(xintercept = date),
               linewidth = 2) +
    geom_label(data = datesofnote,
               aes(x = date,
                   y = c(4.2, 4.5, 5, 5.5, 6),
                   label = event),
               size = rel(5.5),
               fill = "white") +
    geom_hline(aes(yintercept = cs1_depth),
               linewidth = 2,
               color = "orange") +
    geom_label(aes(x = ymd("2019-01-01"),
                   y = cs1_depth,
                   label = "Weir"),
               size = rel(5.5),
               fill = "darksalmon") +    
    labs(color = "Well Name",
         x = "Sensor Time Series",
         y = "Recorded Water Level (ft)",
         title = paste("Full", i, "Time Series at 61344"),
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
      scale_x_datetime(limits = c(ymd_hms('2018-03-01 00:00:00'), ymd_hms('2026-02-01 00:00:00'))) +
    guides(color = guide_legend(override.aes = list(linewidth = 5)))
  ggsave(filename = paste0("61344_", i, "fullseries.png"), fullseries, width = 12, height = 8, units = "in")
}

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

  
  if(nrow(cs1data) > 0){#& nrow(ow1data) > 0){
    if(nrow(ow2data) >= nrow(ow1data)){
      owdata <- ow2data
      owlabel = "OW2"
    } else if(nrow(ow2data) == 0 & nrow(ow1data) == 0){
      next
    } else{
      owdata <- ow1data
      owlabel = "OW1"
    }
    
    test <- paste0("Weir Overtop?: ", metrics_cs1$overtop, "\n",
                   "Weir Surcharge: ", metrics_cs1$surcharge_in, " in\n",
                   "Weir RPSU: ", metrics_cs1$rpsu, "%\n",
                   "Event Depth: ", metrics_cs1$eventdepth_in, " in\n",
                   "Peak Intensity: ", metrics_cs1$eventpeakintensity_inhr, " in/hr")

    hmm <- marsCombinedPlot(event = events$gage_event_uid[i],
                     structure_name = "Dependable Distrubution 61344",
                     storage_depth_ft = cs1_depth,
                     obs_datetime = cs1data$dtime,
                     obs_level_ft = cs1data$level_ft,
                     datetime_2 = owdata$dtime,
                     level_ft_2 = owdata$level_ft,
                     level_names = c("CS1", owlabel),
                     label_text = paste("Top of Weir:", round(cs1_depth, 4)),
                     rainfall_datetime = raindata$dtime,
                     rainfall_in = raindata$rainfall_in,
                     metricslabel = test
    )

    ggsave(paste0("plots/", events$gage_event_uid[i], "_all.png"), plot = hmm, width = 12, height = 8, units = "in")
  }
  
  
  
  metrics <- bind_rows(metrics, metrics_cs1, metrics_ow1, metrics_ow2, metrics_ow3)
}


# 4.0 No Inflection ----
## 4.1 Scatter plot of all wells ----
rpsu_scatter <- ggplot() +
  geom_point(data = metrics,
             aes(x = dtime, y = rpsu, shape = ow_suffix, color = ow_suffix, size = overtop)) +
  geom_vline(data = datesofnote,
             aes(xintercept = date),
             linewidth = 2) +
  geom_label(data = datesofnote,
             aes(x = date,
                 y = seq(from = 10, to = 50, by = 10),
                 label = event),
             size = rel(5),
             fill = "white") +
  labs(size = "Overtop?", 
       shape = "Well Name", 
       color = "Well Name",
       x = "Event Date",
       y = "Relative Percent of Storage Used",
       title = "Relative Storage Utilization by Event at 61344",
       subtitle = "CS1 relative to weir. OWs relative to top of stone storage.") +
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
       title = "Relative Storage Utilization by Event Depth at 61344",
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
       title = "Overtopping at CS1 Weir by Event Depth at 61344") +
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

## 5.3 OW2 Only ----
metrics_owonly <- filter(metrics_box, ow_suffix == "OW2")

metrics_ow2_truelabels <- group_by(metrics_owonly, depthbin, overtop) |>
  summarize(count = n()) |>
  filter(overtop == TRUE)

metrics_ow2_falselabels <- group_by(metrics_owonly, depthbin, overtop) |>
  summarize(count = n()) |>
  filter(overtop == FALSE)

rpsu_box_overtop_ow2 <- ggplot() +
  geom_boxplot(data = metrics_owonly,
               aes(x = depthbin, y = rpsu, color = overtop, fill = overtop), alpha = 0.25) +
  geom_label(data = metrics_ow2_truelabels,
             aes(x = depthbin, y = 110, label = paste("n =", count), color = overtop), 
             fill = "white", position = position_nudge(x = 0.2), show.legend = FALSE) +
  geom_label(data = metrics_ow2_falselabels,
             aes(x = depthbin, y = 110, label = paste("n =", count), color = overtop), 
             fill = "white", position = position_nudge(x = -0.2), show.legend = FALSE) +
  labs(color = "Well Overtop?", 
       fill = "Well Overtop?",
       x = "Event Depth (inches)",
       y = "Relative Percent of Storage Used",
       title = "RPSU at OW2 by Event Depth at 61344") +
  theme(plot.title = element_text(size = rel(3), color = "black"),
        plot.subtitle = element_text(size = rel(2), color = "black"),
        axis.title = element_text(size = rel(2.2), color = "black"),
        axis.text.x = element_text(size = rel(2), color = "black"), # set font size and color of x axis text #size previously set to 14
        axis.text.y = element_text(size = rel(2), color = "black"), # set font size and color of y axis text
        panel.background =  element_rect(fill = "white", colour = NA), # set white background
        panel.border =      element_rect(fill = NA, colour="black"), # set black border
        panel.grid.major =  element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
        panel.grid.minor =  element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
        legend.position = c(0.8, 0.40),
        legend.background = element_rect(linewidth=0.5, linetype="solid", colour ="gray30"),
        legend.title = element_text(size = rel(1.2), color = "black"),
        legend.text = element_text(size = rel(1.2), color = "black")) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_fill_manual(values = c("darkolivegreen", "darkmagenta")) +
  scale_color_manual(values = c("darkolivegreen", "darkmagenta"))
ggsave(filename = "rpsu_boxplot_ow2.png", rpsu_box_overtop_ow2, width = 12, height = 8, units = "in")

#6.0 Days since Pipe Jetting  ----
metrics_boxbyjettingyear <- mutate(metrics,
                                   dayssince = case_when(dtime < ymd("2018-08-01") ~ NA,
                                                         dtime >= ymd("2018-08-01") ~ round(difftime(dtime, ymd("2018-08-01"), units = "days"), 0)),
                                   year = as.factor(year(dtime))) |>
  mutate(yearssince = as.factor(round(dayssince/365, 0))) |>
  filter(ow_suffix == "CS1") #|>
 # filter(yearssince != 7) # Only one event, makes the plots look weird

metrics_boxbyjettingyear_truelabels <- group_by(metrics_boxbyjettingyear, yearssince, overtop) |>
  summarize(count = n()) |>
  filter(overtop == TRUE)

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
       title = "Years Since Pipe Jetting at 61344") +
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
       title = "Days Since Pipe Jetting at 61344") +
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
