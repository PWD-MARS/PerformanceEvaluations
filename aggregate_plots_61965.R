### Script to generate visuals of water level at CS1, OW1 and OW2, SMP-ID = 61965
# 0.0 SETUP----

library(pwdgsi)
library(odbc)
library(lubridate)
library(tidyverse)
library(stats)
library(gridExtra)
library(grid)
library(gtable)
library(ggtext)
library(dplyr)
library(ggplot2)
library(pool)


#read data 
parent_df <- read.csv("//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/59 Performance Evaluation of Private Sites/61965/Metrics/April_26_2024/overtop.csv")


# aggregate plots of overtopping for CS1

rainfall_overtopping_cs1 <- parent_df %>%
  filter(ow_suffix == "CS1") %>%
  filter(eventdatastart_edt > as.Date('2020-01-01'))

rainfall_overtopping_cs1$eventdatastart_edt <-as.Date(rainfall_overtopping_cs1$eventdatastart_edt)



# Calculate overtopping rate at each corner of the plot

larger_than_design_before_jet <- rainfall_overtopping_cs1 %>%
  filter(eventdepth_in > 1 & eventdatastart_edt < as.Date('2023-07-20') & overtopping == "TRUE") %>%
  nrow()

larger_than_design_before_jet_all <- rainfall_overtopping_cs1 %>%
  filter(eventdepth_in > 1 & eventdatastart_edt < as.Date('2023-07-20')) %>%
  nrow()

larger_than_design_before_jet_p <- larger_than_design_before_jet/larger_than_design_before_jet_all


smaller_than_design_before_jet <- rainfall_overtopping_cs1 %>%
  filter(eventdepth_in <= 1 & eventdatastart_edt <= as.Date('2023-07-20') & overtopping == "TRUE") %>%
  nrow()

smaller_than_design_before_jet_all <- rainfall_overtopping_cs1 %>%
  filter(eventdepth_in <= 1 & eventdatastart_edt <= as.Date('2023-07-20')) %>%
  nrow()

smaller_than_design_before_jet_p <- smaller_than_design_before_jet/smaller_than_design_before_jet_all




larger_than_design_after_jet <- rainfall_overtopping_cs1 %>%
  filter(eventdepth_in > 1 & eventdatastart_edt > as.Date('2023-07-20') & overtopping == "TRUE") %>%
  nrow()

larger_than_design_after_jet_all <- rainfall_overtopping_cs1 %>%
  filter(eventdepth_in > 1 & eventdatastart_edt > as.Date('2023-07-20')) %>%
  nrow()

larger_than_design_after_jet_p <- larger_than_design_after_jet/larger_than_design_after_jet_all


smaller_than_design_after_jet <- rainfall_overtopping_cs1 %>%
  filter(eventdepth_in < 1 & eventdatastart_edt >= as.Date('2023-07-20')& overtopping == "TRUE") %>%
  nrow()

smaller_than_design_after_jet_all <- rainfall_overtopping_cs1 %>%
  filter(eventdepth_in < 1 & eventdatastart_edt >= as.Date('2023-07-20')) %>%
  nrow()

smaller_than_design_after_jet_p <- smaller_than_design_after_jet/smaller_than_design_after_jet_all



# percents
larger_than_design_before_jet_p
smaller_than_design_before_jet_p
larger_than_design_after_jet_p
smaller_than_design_after_jet_p


#plot


theilsen_plot <- ggplot(rainfall_overtopping_cs1, aes(x = eventdatastart_edt, y= eventdepth_in)) +
  geom_point(aes(color = overtopping, shape = overtopping), size = 5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
  labs(x = "Date of Event", y = "Rain Event Depth (in)") + 
  geom_vline(xintercept= as.Date('2023-07-20'), color = "purple", size=2, linetype="dashed") +
  geom_hline(yintercept= 1, color = "black", size=2, linetype="dashed") +
  scale_x_date(date_labels="%b-%y",date_breaks  ="6 month") +
  ggtitle("CS1 Overtopping at Cardone 2015-SITE-2809-01 SB-2 (61965)") +
  annotate("text", x = as.Date('2020-03-20'), y = 1.2, label = "Design Storm: 1 in", size = 20/.pt) +
  annotate("text", x = as.Date('2023-05-01'), y = 3, label = "Pipe Jetting Occured on: July 20th 2023", size = 20/.pt) +
  annotate("text", x = as.Date('2021-04-01'), y = 2.8, label = paste("Overtopping Rate for Storms > 1 in before Pipe Jetting :", as.character(round(larger_than_design_before_jet_p*100)),"%"), size = 18/.pt, color = "darkgreen") +
  annotate("text", x = as.Date('2023-07-05'), y = 2.6, label = paste("Overtopping Rate for Storms > 1 in after Pipe Jetting :", as.character(round(larger_than_design_after_jet_p*100)),"%"), size = 18/.pt, color = "darkgreen") +
  annotate("text", x = as.Date('2021-04-01'), y = 0, label = paste("Overtopping Rate for Storms < 1 in before Pipe Jetting :", as.character(round(smaller_than_design_before_jet_p*100)),"%"), size = 18/.pt, color = "darkgreen") +
  annotate("text", x = as.Date('2023-07-05'), y = 0, label = paste("Overtopping Rate for Storms < 1 in after Pipe Jetting :", as.character(round(smaller_than_design_after_jet_p*100)),"%"), size = 18/.pt, color = "darkgreen") 
  

