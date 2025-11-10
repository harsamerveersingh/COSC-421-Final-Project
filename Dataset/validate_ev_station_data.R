# validate_ev_station_data.r
# Purpose: Validate the data pulled using the extract_data_via_public_api.R

# libraries
library(dplyr)
library(readr)
library(ggplot2)
library(leaflet)
library(stringr)

# load data
csv_path = "./Dataset/bc_electric_charging_stations.csv"
bc_stations <- read_csv(csv_path, show_col_types = FALSE)
cat("Raw data has", nrow(bc_stations), "stations with", ncol(bc_stations), "columns.\n\n")

# identify missing critical fields, duplicate stations, and stations outside of BC
missing_geo <- sum(is.na(bc_stations$latitude) | is.na(bc_stations$longitude))
missing_name <- sum(is.na(bc_stations$station_name))
dup_ids <- bc_stations %>% filter(duplicated(id)) %>% nrow()
out_of_bounds <- bc_stations %>%
  filter(latitude < 48 | latitude > 60 | longitude < -139 | longitude > -114) %>%
  nrow()

# clean data
bc_clean <- bc_stations %>%
  # remove duplicates
  distinct(id, .keep_all = TRUE) %>%
  # filter valid BC coordinates
  filter(between(latitude, 48, 60), between(longitude, -139, -114)) %>%
  # clean port counts
  mutate(
    ev_level2_evse_num = pmax(ev_level2_evse_num, 0, na.rm = TRUE),
    ev_dc_fast_num = pmax(ev_dc_fast_num, 0, na.rm = TRUE),
    # normalize access
    access = case_when(
      str_to_lower(access_code) %in% c("public", "pub") ~ "public",
      str_to_lower(access_code) %in% c("private", "priv") ~ "private",
      TRUE ~ "unknown"
    ),
    # derive features
    has_level2 = ev_level2_evse_num > 0,
    has_dc_fast = ev_dc_fast_num > 0,
    total_ports = ev_level2_evse_num + ev_dc_fast_num,
    charging_type = case_when(
      has_dc_fast & has_level2 ~ "Level 2 + DC Fast",
      has_dc_fast ~ "DC Fast Only",
      has_level2 ~ "Level 2 Only",
      TRUE ~ "No Ports"
    )
  )
cat("Cleaned data has", nrow(bc_clean), "stations with", ncol(bc_clean), "columns.\n\n")


# save cleaned data to csv
csv_path = "./Dataset/bc_electric_charging_stations_CLEAN.csv"
write.csv(bc_clean, csv_path, row.names = FALSE)
