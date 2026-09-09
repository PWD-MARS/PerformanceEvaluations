library(tidyverse)
library(lubridate)
library(RPostgres)
library(padr)
library(pwdgsi)

mars <- dbConnect(drv = RPostgres::Postgres(),
                  host = "PWDMARSDBS1",
                  port = 5434,
                  dbname = "mars_prod",
                  user= Sys.getenv("admin_uid"),
                  password = Sys.getenv("admin_pwd"))

roosevelt_level <- marsFetchLevelData(mars, target_id = "274-4-1",
                                    ow_suffix = "CW1",start_date = '2000-01-01',
                                    end_date = '2030-01-01',
                                    sump_correct = FALSE)

roosevelt_all <- ggplot() + 
  geom_line(data = roosevelt_level, aes(x = dtime, y = depth_ft)) + theme(
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
 scale_y_reverse(limits = c(22.5, 0), minor_breaks = c(0:25), name = "Depth to Water (ft)") + 
  scale_x_datetime(breaks = "1 year", date_labels = "%Y", name = "Datetime") +
  ggtitle("Roosevelt Playground 274-4-1 CW1 Groundwater Level") 
ggsave(filename = "roosevelt_all.png", roosevelt_all, width = 8, height = 6, units = "in")

#Convert to water level for the sake of comparison to 20-8-1 Dry Weather
roosevelt_elev <- mutate(hey, depth_ft = 40.5 - depth_ft) %>%
  filter(depth_ft < 25, depth_ft > 17) #Clean up outliers

roosevelt_inverted <- ggplot() + 
  geom_line(data = roosevelt_elev, aes(x = dtime, y = depth_ft, color = "Roosevelt 274-4-1 CW1 (Hellerman and Cottage)")) + 
  theme(
    axis.title.y = ggplot2::element_blank(), #Remove axis label to show levels are not actually comparable
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
    axis.text.y = ggplot2::element_blank(), # set font size and color of y axis text
    panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
    panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  ggplot2::element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
    panel.grid.minor =  ggplot2::element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
    legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
    legend.text = ggplot2::element_text(size = ggplot2::rel(.9)),
    legend.direction = "vertical") +
  scale_x_datetime(breaks = "1 year", date_labels = "%Y", name = "Datetime") +
  ggtitle("Roosevelt Playground 274-4-1 CW1 Groundwater vs 20-8-1 OW1 Dry Weather",
          subtitle = "Plots overlaid for conceptual visualization - elevations not actually comparable") 

bls8_dryadjusted <- mutate(bls8_dry, elev_ft = level_ft + 18.58) #From the other file
  #Adjusted to line up with the Roosevelt line

roosevelt_overlay <- roosevelt_inverted + geom_line(data = bls8_dryadjusted, aes(x = dtime, y = elev_ft, color = "BLS 20-8-1 OW1 Dry Weather (Hunting Park and O)")) + 
  scale_y_continuous(limits = c(18, 23)) + scale_colour_manual(name = "Sensors",
                                                               values = c("Roosevelt 274-4-1 CW1 (Hellerman and Cottage)" = "black",
                                                                          "BLS 20-8-1 OW1 Dry Weather (Hunting Park and O)" = "red"))
#  guides(color = guide_legend())
ggsave(filename = "roosevelt_bls.png", roosevelt_overlay, width = 8, height = 6, units = "in")
