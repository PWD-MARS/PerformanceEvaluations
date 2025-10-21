### Script to create plots of rain event depth or intensity vs. overtopping at Ferko 411-1-1 for full monitoring period

# Library necessary packages 
library(tidyverse)
library(lubridate)

# Set event info and design targets
smp_id = '411-1-1'
end_date = "2025-04-09"
event_dates <- c("2021-05-03", "2021-12-01", "2023-02-22") %>% 
  lubridate::as_datetime()
event_descriptions <- c("trash guard cleaned", "trash guard cleaned", "trash guard removed") 
peak_intensity <- 2.5 # Philadelphia 1-year, 15-minute peak intensity, in/hr
design_depth <- 1.6 # Design storm depth, in

# Read in metrics file
smp_metrics <- read.csv(paste0(smp_id, "/output/metrics_", end_date, ".csv")) %>%
  mutate(eventdatastart = ymd_hms(eventdatastart),
         eventdataend = ymd_hms(eventdataend))
  
 
# Find some min/max values to help with setting axes
# May be able to delete this in future with smart plot settings
date_range <- range(smp_metrics$eventdatastart)
max_obs_intensity <- max(smp_metrics$eventpeakintensity_inhr)
intensity_plot_max <- max(peak_intensity, max_obs_intensity)
max_obs_depth <- max(smp_metrics$eventdepth_in)
depth_plot_max <- max(design_storm, max_obs_depth)

# Create intensity vs. overtopping plot
intensity_plot <- ggplot(smp_metrics,
                         aes(x = eventdatastart, 
                             y = eventpeakintensity_inhr)) + 
  geom_point(aes(color = overtop, shape = overtop), size=2) + 
  scale_color_manual(values = c('blue', 'red')) + 
  geom_hline(yintercept = peak_intensity, color = "black", size = 1, linetype = "dashed") + 
  scale_y_continuous(limits = c(0,intensity_plot_max), minor_breaks = seq(0, intensity_plot_max, 0.2)) +
  scale_x_datetime(limits = date_range, date_minor_breaks = "3 months") + 
  ylab("Rain Event Peak Intensity (in/hr)") + 
  xlab("Event Date") + 
  geom_text(label = paste("Phila 1-yr, 15-min peak: ", peak_intensity, " in/hr"),
            y = peak_intensity -.05, color = "black", size = 10 / .pt, hjust = 0, 
            x = event_dates[3]+days(60), check_overlap = TRUE) + 
  ggtitle(paste0("Event Peak Intensity and Overtopping vs. Date for ",smp_metrics$smp_id[1])) + 
  labs(color = "Overtopping", shape = "Overtopping")  

if(length(event_dates) > 0 & length(event_descriptions) > 0){
  for(i in 1:length(event_dates)){
    intensity_plot <- intensity_plot + geom_vline(xintercept = event_dates[i], color = "purple", size = 1, 
                                                  linetype = "dashed") +
      geom_text(label = event_descriptions[i], angle = 90,
                y = peak_intensity-0.05, color = "black", size = 10 / .pt, hjust = 1,
                x = as.numeric(event_dates[i] + days(15)), check_overlap = TRUE) +
      geom_text(label = event_dates[i], angle = 90,
                y = peak_intensity+0.051, color = "black", size = 10 / .pt, hjust = 0,
                x = as.numeric(event_dates[i] + days(15)), check_overlap = TRUE)
  }
}

intensity_plot
ggsave(paste0(smp_id, "/output/intensity_overtopping_plot_", end_date, ".png"))

# Create intensity vs. overtopping plot
depth_plot <- ggplot(smp_metrics,
                         aes(x = eventdatastart, 
                             y = eventdepth_in)) + 
  geom_point(aes(color = overtop, shape = overtop), size=2) + 
  scale_color_manual(values = c('blue', 'red')) + 
  geom_hline(yintercept = design_depth, color = "black", size = 1, linetype = "dashed") + 
  scale_y_continuous(limits = c(0,depth_plot_max), minor_breaks = seq(0, depth_plot_max, 0.2)) +
  scale_x_datetime(limits = date_range, date_minor_breaks = "3 months") + 
  ylab("Rain Event Depth (in)") + 
  xlab("Event Date") + 
  geom_text(label = paste("Design Storm Depth: ", design_depth, " in"),
            y = design_depth -.05, color = "black", size = 10 / .pt, hjust = 0, 
            x = event_dates[3]+days(60), check_overlap = TRUE) + 
  ggtitle(paste0("Rain Event Depth and Overtopping vs. Date for ",smp_metrics$smp_id[1])) + 
  labs(color = "Overtopping", shape = "Overtopping")  

if(length(event_dates) > 0 & length(event_descriptions) > 0){
  for(i in 1:length(event_dates)){
    depth_plot <- depth_plot + geom_vline(xintercept = event_dates[i], color = "purple", size = 1, 
                                                  linetype = "dashed") +
      geom_text(label = event_descriptions[i], angle = 90,
                y = design_depth-0.05, color = "black", size = 10 / .pt, hjust = 1,
                x = as.numeric(event_dates[i] + days(15)), check_overlap = TRUE) +
      geom_text(label = event_dates[i], angle = 90,
                y = design_depth+0.051, color = "black", size = 10 / .pt, hjust = 0,
                x = as.numeric(event_dates[i] + days(15)), check_overlap = TRUE)
  }
}

depth_plot
ggsave(paste0(smp_id, "/output/depth_overtopping_plot_", end_date, ".png"))
