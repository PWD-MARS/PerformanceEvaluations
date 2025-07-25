#### Script to generate summary stats based on event metrics

#### Setup

# Library necessary packages
library(tidyverse) 
library(ggplot2)
library(pwdgsi)
library(odbc)
library(DBI)
library(lubridate)
library(magrittr)
library(formattable)
library(knitr)


# Round values in tables to 2 digits
kable <- function(data) {
  knitr::kable(data, digits = 2)
}

metrics_file = "output/metrics_2025-04-09.csv"

# Read in metrics data and add interval marker
smp_metrics <- read.csv(metrics_file)
key_dates <- c("2021-05-03", "2021-12-01", "2023-02-22") %>% lubridate::as_datetime()
key_dates_3 <- append(key_dates, max(lubridate::as_datetime(smp_metrics$eventdatastart_est)))
date_lengths_3 <- diff(key_dates_3)/7 
key_dates_2 <- key_dates_3[-c(2)]
date_lengths_2 <- diff(key_dates_2)/7
smp_metrics <- smp_metrics %>% 
  select(eventdepth_in, eventpeakintensity_inhr, eventdatastart_est, overtop) %>% 
  mutate(int_3 = cut(lubridate::as_datetime(eventdatastart_est), key_dates_3, right = TRUE,
                           labels = c("After First Cleaning", "After Second Cleaning", "After Removal"))) %>% 
  mutate(int_length_3 = as.numeric(as.character(factor(int_3, levels = c("After First Cleaning", "After Second Cleaning", "After Removal"),
                            labels = date_lengths_3)))) %>% 
  mutate(int_2 = cut(lubridate::as_datetime(eventdatastart_est), key_dates_2, right = TRUE,
                           labels = c("Before Removal", "After Removal"))) %>%
  mutate(int_length_2 = as.numeric(as.character(factor(int_2, levels = c("Before Removal", "After Removal"),
                            labels = date_lengths_2)))) %>%   
  mutate(depth_type = cut(eventdepth_in, c(0, 1, 1.6, max(eventdepth_in)), labels = c("Small", "Medium", "Large"))) %>%
  mutate(intensity_type = cut(eventpeakintensity_inhr, c(0, 1, 2.5, max(eventpeakintensity_inhr)), labels = c("Small", "Medium", "Large")))

kable(
  smp_metrics %>% 
    group_by(int_3) %>%
    summarize(num_ot=sum(overtop), ot_freq = sum(overtop/int_length_3), 
              pc_ot_depth_s = percent(sum(overtop[depth_type == "Small"])/sum(depth_type == "Small"),0), 
              pc_ot_depth_m = percent(sum(overtop[depth_type == "Medium"])/sum(depth_type == "Medium"),0), 
              pc_ot_depth_l = percent(sum(overtop[depth_type == "Large"])/sum(depth_type == "Large"),0),
              pc_ot_int_S = percent(sum(overtop[intensity_type == "Small"])/sum(intensity_type == "Small"),0), 
              pc_ot_int_m = percent(sum(overtop[intensity_type == "Medium"])/sum(intensity_type == "Medium"),0), 
              pc_ot_int_l = percent(sum(overtop[intensity_type == "Large"])/sum(intensity_type == "Large"),0),
              )
)

kable(
  smp_metrics %>% 
    group_by(int_2) %>%
    summarize(num_ot=sum(overtop), ot_freq = sum(overtop/int_length_2), 
              pc_ot_depth_s = percent(sum(overtop[depth_type == "Small"])/sum(depth_type == "Small"),0), 
              pc_ot_depth_m = percent(sum(overtop[depth_type == "Medium"])/sum(depth_type == "Medium"),0), 
              pc_ot_depth_l = percent(sum(overtop[depth_type == "Large"])/sum(depth_type == "Large"),0),
              pc_ot_int_S = percent(sum(overtop[intensity_type == "Small"])/sum(intensity_type == "Small"),0), 
              pc_ot_int_m = percent(sum(overtop[intensity_type == "Medium"])/sum(intensity_type == "Medium"),0), 
              pc_ot_int_l = percent(sum(overtop[intensity_type == "Large"])/sum(intensity_type == "Large"),0),
    )
)


  


