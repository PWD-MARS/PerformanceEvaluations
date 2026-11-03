library(tidyverse)
library(lubridate)
library(RPostgres)
library(pwdgsi)


mars <- dbConnect(drv = RPostgres::Postgres(),
                  host = "PWDMARSDBS1",
                  port = 5434,
                  dbname = "mars_prod",
                  user= Sys.getenv("admin_uid"),
                  password = Sys.getenv("admin_pwd"))
#Plot full time series of BLS 20-4-1 OW1 and 20-8-1 OW1

  #Gather appropriate data for plotting
    bls_4 <- marsFetchMonitoringData(mars, target_id = "20-4-1",
                                     ow_suffix = "OW1",
                                     source = "gage", start_date = '2000-01-01',
                                     end_date = '2030-01-01',
                                     sump_correct = FALSE)

    bls_8 <- marsFetchMonitoringData(mars, target_id = "20-8-1",
                                     ow_suffix = "OW1",
                                     source = "gage", start_date = '2000-01-01',
                                     end_date = '2030-01-01',
                                     sump_correct = FALSE)

  #Write local copies for latency reasons
    write.csv(bls_4$`Rain Event Data`,
              file = "20-4-1_ow1_events.csv", row.names=FALSE)
    write.csv(bls_4$`Rainfall Data`,
              file = "20-4-1_ow1_rain.csv", row.names=FALSE)
    write.csv(bls_4$`Level Data`,
              file = "20-4-1_ow1_level.csv", row.names=FALSE)

    write.csv(bls_8$`Rain Event Data`,
              file = "20-8-1_ow1_events.csv", row.names=FALSE)
    write.csv(bls_8$`Rainfall Data`,
              file = "20-8-1_ow1_rain.csv", row.names=FALSE)
    write.csv(bls_8$`Level Data`,
              file = "20-8-1_ow1_level.csv", row.names=FALSE)

  #import from local copies
  rain_4 <- read_csv(file = "20-4-1_ow1_rain.csv")
  events_4 <- read_csv(file = "20-4-1_ow1_events.csv")
  level_4 <- read_csv(file = "20-4-1_ow1_level.csv")
  
  rain_8 <- read_csv(file = "20-8-1_ow1_rain.csv")
  events_8 <- read_csv(file = "20-8-1_ow1_events.csv")
  level_8 <- read_csv(file = "20-8-1_ow1_level.csv") 

    
  #Gather appropriate data from survey spreadsheet
    #20-4-1
    deploydepth_4_ft <- 0
    sumpdepth_4_ft <- 0.6267
    storage_depth_4_ft <- 2 + sumpdepth_4_ft
    soil_depth_4_ft <- 4 + sumpdepth_4_ft
    ow_rim_4_ft <- 33.18 - 28.01 #sanity check

    #20-8-1
    deploydepth_8_ft <- 1.25/12 #1.25 inches
    sumpdepth_8_ft <- 25.5-25.08
    storage_depth_8_ft <- 2
    ow_rim_8_ft <- 29.37 - 25.08
    
    #Add deployment depth to level in 20-8
    level_8 <- mutate(level_8, level_ft = level_ft + deploydepth_8_ft)
    
  
  #Plot data
    plot_level_4 <- ggplot(data = level_4) +
      geom_line(aes(x = dtime, y = level_ft)) + theme(
            axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
            axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
            axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
            panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
            panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
            panel.grid.major =  ggplot2::element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
            panel.grid.minor =  ggplot2::element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
            legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
            legend.text = ggplot2::element_text(size = ggplot2::rel(.9)),
            legend.title=ggplot2::element_blank()) + 
       geom_hline(yintercept = storage_depth_4_ft, color = "orange", linewidth = 1.2) +
       geom_hline(yintercept = sumpdepth_4_ft, color = "deepskyblue", linewidth = 1.2) +
      geom_hline(yintercept = soil_depth_4_ft, color = "forestgreen", linewidth = 1.2) +
      geom_hline(yintercept = ow_rim_4_ft, color = "red", linewidth = 1.2) + 
      annotate(geom = "label",
               x = ymd("2014-08-01"),
               y = sumpdepth_4_ft,
               label = "Top of Sump",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2014-08-01"),
               y = storage_depth_4_ft,
               label = "Top of Stone",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2014-08-01"),
               y = storage_depth_4_ft,
               label = "Top of Stone",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2014-08-01"),
               y = soil_depth_4_ft,
               label = "Top of Soil",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2014-08-01"),
               y = ow_rim_4_ft,
               label = "Top of OW Rim",
               fill = "white") +
      scale_y_continuous(name = "OW1 Water Level (ft)",
                         n.breaks = 6,
                         limits = c(0, 5.5),
                         sec.axis = sec_axis(transform = (~ . + 28.01),
                                             name = "Elevation")) +
      xlab("Datetime") + ggtitle("BLS 20-4-1 OW1 Period of Record")
    
      
    
    plot_level_8 <- ggplot(data = level_8) +
      geom_line(aes(x = dtime, y = level_ft)) + 
      geom_rect(xmin = min(level_8$dtime), xmax = max(level_8$dtime), 
                ymin = 0, ymax = deploydepth_8_ft, fill = "grey70") + theme(
        axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
        axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
        axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
        panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
        panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
        panel.grid.major =  ggplot2::element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
        panel.grid.minor =  ggplot2::element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
        legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
        legend.text = ggplot2::element_text(size = ggplot2::rel(.9)),
        legend.title=ggplot2::element_blank()) + 
      geom_hline(yintercept = storage_depth_8_ft, color = "orange", linewidth = 1.2) +
      geom_hline(yintercept = sumpdepth_8_ft, color = "deepskyblue", linewidth = 1.2) +
      geom_hline(yintercept = ow_rim_8_ft, color = "red", linewidth = 1.2) +
      annotate(geom = "label",
               x = ymd("2020-01-01"),
               y = sumpdepth_8_ft,
               label = "Top of Sump",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2020-01-01"),
               y = storage_depth_8_ft,
               label = "Top of Stone",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2020-01-01"),
               y = ow_rim_8_ft,
               label = "Top of OW Rim",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2020-01-01"),
               y = mean(c(0, deploydepth_8_ft)),
               label = "Sensor Installation Height",
               fill = "white") +
      scale_y_continuous(name = "OW1 Water Level (ft)",
                         n.breaks = 6,
                         limits = c(0, 4.5),
                         sec.axis = sec_axis(transform = (~ . + 25.08),
                                             name = "Elevation")) +
      xlab("Datetime") + ggtitle("BLS 20-8-1 OW1 Period of Record")
    
      
    
    ggsave(filename = "bls4_all.png", plot_level_4, width = 8, height = 6, units = "in")
    ggsave(filename = "bls8_all.png", plot_level_8, width = 8, height = 6, units = "in")
    

  #Dry weather baselines?
    #Trim some rain events manually
    bls8_dry <- filter(level_8, is.na(gage_event_uid),
                  !(dtime >= ymd("2015-11-03") & dtime <= ymd("2015-11-04")),
                  !(dtime >= ymd("2024-05-23") & dtime <= ymd("2024-05-27" )),
                  !(dtime >= ymd("2025-02-06") & dtime <= ymd("2025-02-15" )),
                  dtime <= ymd("2026-01-01"))
    bls8_dry$level_ft[bls8_dry$level_ft < 0.5] <- NA
    
    bls8_dryplot <- ggplot(data = bls8_dry) +
      geom_line(aes(x = dtime, y = level_ft)) + 
      geom_rect(xmin = min(bls8_dry$dtime), xmax = max(bls8_dry$dtime), 
                ymin = 0, ymax = deploydepth_8_ft, fill = "grey70") + theme(
                  axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                  axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
                  axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
                  panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
                  panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
                  panel.grid.major =  ggplot2::element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
                  panel.grid.minor =  ggplot2::element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
                  legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
                  legend.text = ggplot2::element_text(size = ggplot2::rel(.9)),
                  legend.title=ggplot2::element_blank()) + 
      geom_hline(yintercept = storage_depth_8_ft, color = "orange", linewidth = 1.2) +
      geom_hline(yintercept = sumpdepth_8_ft, color = "deepskyblue", linewidth = 1.2) +
      geom_hline(yintercept = ow_rim_8_ft, color = "red", linewidth = 1.2) +
      annotate(geom = "label",
               x = ymd("2020-01-01"),
               y = sumpdepth_8_ft,
               label = "Top of OW Sump",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2020-01-01"),
               y = storage_depth_8_ft,
               label = "Top of Stone Storage",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2020-01-01"),
               y = mean(c(0, deploydepth_8_ft)),
               label = "Sensor Installation Height",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2020-01-01"),
               y = ow_rim_8_ft,
               label = "Top of OW Rim (at grade)",
               fill = "white") +
      scale_y_continuous(name = "OW1 Water Level (ft)",
                         n.breaks = 6,
                         limits = c(0, 4.5),
                         sec.axis = sec_axis(transform = (~ . + 25.08),
                                             name = "Elevation")) +
      xlab("Datetime") + ggtitle("BLS 20-8-1 OW1 Dry Weather Water Level 2014-2026")
    ggsave(bls8_dryplot, filename = paste0("20-8-1_dry_full.png"),
           width = 8, height = 6, units = "in")    
    
    #Dry weather baselines?
    bls4_dry <- filter(level_4, is.na(gage_event_uid))
    bls4_dry$level_ft[bls4_dry$dtime > ymd("2015-11-03") & bls4_dry$dtime < ymd("2015-11-05")] <- NA #Cleaning up outliers

    
    bls4_dryplot <- ggplot(data = bls4_dry) +
      geom_line(aes(x = dtime, y = level_ft)) + theme(
        axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
        axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
        axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
        panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
        panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
        panel.grid.major =  ggplot2::element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
        panel.grid.minor =  ggplot2::element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
        legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
        legend.text = ggplot2::element_text(size = ggplot2::rel(.9)),
        legend.title=ggplot2::element_blank()) + 
      geom_hline(yintercept = storage_depth_4_ft, color = "orange", linewidth = 1.2) +
      geom_hline(yintercept = sumpdepth_4_ft, color = "deepskyblue", linewidth = 1.2) +
      geom_hline(yintercept = soil_depth_4_ft, color = "forestgreen", linewidth = 1.2) +
      geom_hline(yintercept = ow_rim_4_ft, color = "red", linewidth = 1.2) + 
      annotate(geom = "label",
               x = ymd("2015-03-01"),
               y = sumpdepth_4_ft,
               label = "Top of OW Sump",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2015-03-01"),
               y = storage_depth_4_ft,
               label = "Top of Stone Storage",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2015-03-01"),
               y = soil_depth_4_ft,
               label = "Top of Planter Soil",
               fill = "white") +
      annotate(geom = "label",
               x = ymd("2015-03-01"),
               y = ow_rim_4_ft,
               label = "Top of OW Rim (at grade)",
               fill = "white") +
      scale_y_continuous(name = "OW1 Water Level (ft)",
                         n.breaks = 6,
                         limits = c(0, 5.5),
                         sec.axis = sec_axis(transform = (~ . + 28.01),
                                             name = "Elevation")) +
      xlab("Datetime") + ggtitle("BLS 20-4-1 OW1 Dry Weather Water Level 2014-2016")
    ggsave(bls4_dryplot, filename = paste0("20-4-1_dry.png"),
           width = 8, height = 6, units = "in")    
    
    
#Seasonal BLS dry weather highs and lows
    bls8_years <- mutate(bls8_dry, year = year(dtime), month = month(dtime)) %>%
      filter(year != 2026)
    
    bls8_peaks <- group_by(bls8_years, year) %>%
      summarize(highwater_level = max(level_ft, na.rm = TRUE), lowwater_level = min(level_ft, na.rm = TRUE),
                highwater_month = month[which.max(level_ft)],
                lowwater_month = month[which.min(level_ft)])
    
    seasons <- data.frame(season = c(rep("Spring", 3), rep("Summer", 3), rep("Autumn", 3), rep("Winter", 3)),
                          month = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2))
    
    bls8_seasons <- left_join(bls8_peaks, seasons, by = c("highwater_month" = "month"), suffix = c("", "_high")) %>%
      left_join(seasons, by = c("lowwater_month" = "month"), suffix = c("", "_low")) %>%
      mutate(season_high = season) %>%
      select(-season)
    
    roosevelt_totals <- group_by(roosevelt_clean, year, month) %>% summarize(n = n())
    