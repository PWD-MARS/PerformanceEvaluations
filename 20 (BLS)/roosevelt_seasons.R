roosevelt_level <- marsFetchLevelData(mars, target_id = "274-4-1",
                                      ow_suffix = "CW1",start_date = '2000-01-01',
                                      end_date = '2030-01-01',
                                      sump_correct = FALSE)

roosevelt_years <- mutate(roosevelt_level, year = year(dtime), month = month(dtime))


  
  
roosevelt_all <- ggplot() + 
  geom_line(data = roosevelt_clean, aes(x = dtime, y = depth_ft)) + theme(
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
  geom_hline(yintercept = 0, color = "orange", linewidth = 1.2) +
  annotate(geom = "label",
           x = ymd("2020-01-01"),
           y = 0,
           label = "Surface Elevation (approx 40.5)",
           fill = "white") +
  scale_y_reverse(limits = c(28, 0), minor_breaks = c(0:28), name = "Depth to Water (ft)") + 
  scale_x_datetime(breaks = "1 year", date_labels = "%Y", name = "Datetime") +
  ggtitle("Roosevelt Playground 274-4-1 CW1 Groundwater Level") 
ggsave(filename = "roosevelt_all.png", roosevelt_all, width = 8, height = 6, units = "in")

roosevelt_clean <- roosevelt_years |> 
  filter(depth_ft > 0, depth_ft < 26) |> #clean up anomalous singleton data points
  filter(dtime < ymd("2016-04-01") | dtime > ymd("2016-04-08")) |> #Sharp anomalous drop and return
  filter(!dtime %in% c("2014-08-14 15:00:00", #Manual list of single point outliers, ick
                       "2014-09-24 18:15:00",
                       "2015-11-17 18:00:00",
                       "2016-05-13 15:55:00",
                       "2016-10-24 18:45:00",
                       "2016-11-14 15:45:00",
                       "2017-02-14 16:15:00",
                       "2017-04-14 14:35:00",
                       "2018-01-16 15:35:00",
                       "2018-05-15 15:25:00",
                       "2018-08-16 15:05:00",
                       "2020-01-27 17:50:00",
                       "2020-11-18 17:30:00",
                       "2022-05-12 16:15:00",
                       "2022-12-15 17:45:00",
                       "2023-06-09 17:00:00"))

roosevelt_peaks <- group_by(roosevelt_clean, year) %>%
  summarize(highwater_elev = min(depth_ft), lowwater_elev = max(depth_ft),
            highwater_month = month[which(min(depth_ft))],
            lowwater_month = month[which(max(depth_ft))])
