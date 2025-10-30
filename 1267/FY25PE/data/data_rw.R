library(tidyverse)
library(readxl)

# Function for reading QAQC files
read_qaqc <- function(file_path) {
  # Read Final_Import_Site sheet
  qaqc <- readxl::read_xlsx(file_path,
                            # Skip first header column
                            #skip = 1,
                            range = "Data!A2:J8832",
                            col_types = c("date", "date", "numeric",
                                          "date", "numeric", 
                                          "numeric","numeric","numeric",
                                          "numeric", "numeric"
                            )) |>
    # Not using `Standard Dtime` due to Excel rounding issue
    rename(dtime = `Dtime Baro`,
           water_level = `Corrected Water Depth (ft)`) |>
    # Select only dtime and water level
    select(dtime, water_level) |>
    # Make sure tz is correct
    mutate(dtime = force_tz(dtime, tz = "EST"))
}

# Paths to files
dash2_main <- "\\\\pwdoows\\OOWS\\Watershed Sciences\\GSI Monitoring\\02 GSI Monitoring Sites\\Wissinoming Park_1267\\QAQC\\1267-2-1"

dash2_cs1Files <- list.files(paste(dash2_main, "\\CS1", sep = ""))
dash2_cs1paths <- map(dash2_main, paste, "\\CS1\\", dash2_cs1Files, sep = "") |> unlist()

dash2_cs2Files <-list.files(paste(dash2_main, "\\CS2", sep = ""))
dash2_cs2paths <- map(dash2_main, paste,"\\CS2\\", dash2_cs2Files, sep = "") |> unlist()

# Remove missing data in QAQC sheet
dash2_cs1 <- map_dfr(dash2_cs1paths, read_qaqc) |> filter(dtime >= lubridate::ymd_hms("2024-03-22 10:15:00", tz = "EST"))
dash2_cs2 <- map_dfr(dash2_cs2paths, read_qaqc) |> filter(dtime >= lubridate::ymd_hms("2024-03-22 10:15:00", tz = "EST"))

# Export to RDS files
readr::write_rds(dash2_cs1, "1267-2_CS1_level.rds", "xz", compression = 9L)
readr::write_rds(dash2_cs2, "1267-2_CS2_level.rds", "xz", compression = 9L)