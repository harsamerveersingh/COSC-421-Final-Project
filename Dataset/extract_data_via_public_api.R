# extract_data_via_public_api.R
# Purpose: Extract available data using a public API

# libraries
install.packages(c("httr", "jsonlite", "dplyr"))
library(httr)
library(jsonlite)
library(dplyr)

# variables
api_key <- "DKGxjdCMg8axymiGPJfJ1oGaiWimbFTVJXuEQUEu"  # Replace with your NREL API key
base_url <- "https://developer.nrel.gov/api/alt-fuel-stations/v1.json"
params <- list(
  api_key = api_key,
  fuel_type = "ELEC",
  country = "CA",
  state = "BC",
  limit = NULL
)

# make request
response <- GET(base_url, query = params)

# check status
cat("HTTP Status:", status_code(response), "\n")
if (status_code(response) != 200) {
  cat("ERROR: API request failed!\n")
  cat("Response body:\n")
  print(content(response, "text"))
  stop("Stopping due to API error.")
}

# parse json response
raw_data <- content(response, "text", encoding = "UTF-8")
data <- fromJSON(raw_data, flatten = TRUE)

# show structure of data
cat("Total results reported by API:", data$total_results, "\n")
cat("Structure of data:\n")
str(data, max.level = 1)

# extract stations
stations <- data$fuel_stations
if (length(stations) == 0) {
  cat("No stations returned. Check API key, parameters, or internet.\n")
  stop("No data to save.")
}

# save data into df
bc_stations_df <- bind_rows(stations)
csv_path <- "bc_electric_charging_stations.csv"

# clean data
bc_stations_df_clean <- bc_stations_df %>%
  mutate(across(where(is.list), ~ sapply(., paste, collapse = ", ")))

# save data as csv
csv_path <- "./Dataset/bc_electric_charging_stations.csv"
write.csv(bc_stations_df_clean, csv_path, row.names = FALSE)
cat("Done! Saved", nrow(bc_stations_df_clean), "stations to CSV.\n")


