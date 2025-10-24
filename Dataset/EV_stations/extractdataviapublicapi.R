install.packages(c("httr", "jsonlite", "dplyr"))
# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)

# Step 4.1: Define API parameters
api_key <- "DKGxjdCMg8axymiGPJfJ1oGaiWimbFTVJXuEQUEu"  # Replace with your NREL API key
base_url <- "https://developer.nrel.gov/api/alt-fuel-stations/v1.json"

# Parameters for ALL electric charging stations in BC, Canada
# - fuel_type=ELEC: Electric only
# - country=CA: Canada
# - state=BC: British Columbia
# - limit: None (fetches all; API caps at ~10k results)
params <- list(
  api_key = api_key,
  fuel_type = "ELEC",
  country = "CA",
  state = "BC",
  limit = NULL  # Fetches all results
)

# === MAKE REQUEST ===
cat("Making API request...\n")
response <- GET(base_url, query = params)

# === CHECK STATUS ===
cat("HTTP Status:", status_code(response), "\n")

if (status_code(response) != 200) {
  cat("ERROR: API request failed!\n")
  cat("Response body:\n")
  print(content(response, "text"))
  stop("Stopping due to API error.")
}

# === PARSE JSON ===
cat("Parsing JSON response...\n")
raw_data <- content(response, "text", encoding = "UTF-8")
data <- fromJSON(raw_data, flatten = TRUE)

# === DEBUG: SHOW STRUCTURE ===
cat("Total results reported by API:", data$total_results, "\n")
cat("Structure of data:\n")
str(data, max.level = 1)

# === EXTRACT STATIONS ===
stations <- data$fuel_stations

if (length(stations) == 0) {
  cat("No stations returned. Check API key, parameters, or internet.\n")
  stop("No data to save.")
}

# === CONVERT TO DATA FRAME ===
cat("Converting to data frame...\n")
bc_stations_df <- bind_rows(stations)

cat("Final data frame dimensions:", dim(bc_stations_df), "\n")
cat("First few station names:\n")
print(head(bc_stations_df$station_name, 5))

# === SET DIRECTORY & SAVE ===
target_dir <- "C:/Users/hsingh77.stu/Downloads/COSC-421-Final-Project/Dataset/EV_stations"

if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
  cat("Created folder:", target_dir, "\n")
}

setwd(target_dir)
cat("Working directory:", getwd(), "\n")

csv_path <- "bc_electric_charging_stations.csv"
# After you have: bc_stations_df <- bind_rows(stations)

# CLEAN: Convert list columns to strings
bc_stations_df_clean <- bc_stations_df %>%
  mutate(across(where(is.list), ~ sapply(., paste, collapse = ", ")))

# SET PATH
csv_path <- "C:/Users/hsingh77.stu/Downloads/COSC-421-Final-Project/Dataset/EV_stations/bc_electric_charging_stations.csv"

# SAVE
write.csv(bc_stations_df_clean, csv_path, row.names = FALSE)

cat("Done! Saved", nrow(bc_stations_df_clean), "stations to CSV.\n")
# === FINAL VERIFICATION ===
if (file.info(csv_path)$size > 100) {
  cat("File is NOT empty. Success!\n")
} else {
  cat("File is empty or too small. Something went wrong.\n")
}



