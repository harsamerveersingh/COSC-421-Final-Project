# =========================================
# EV CHARGING STATIONS IN BC: FULL ANALYSIS
# =========================================
library(dplyr)
library(readr)
library(ggplot2)
library(leaflet)
library(stringr)

# --- 1. LOAD DATA ---
csv_path <- "C:/Users/hsingh77.stu/Downloads/COSC-421-Final-Project/Dataset/EV_stations/bc_electric_charging_stations.csv"
cat("Loading data...\n")
bc_stations <- read_csv(csv_path, show_col_types = FALSE)

cat("Loaded", nrow(bc_stations), "stations with", ncol(bc_stations), "columns.\n\n")

# --- 3. VALIDATE DATA ---
cat("\n=== VALIDATION ===\n")

# Missing critical fields
missing_geo <- sum(is.na(bc_stations$latitude) | is.na(bc_stations$longitude))
missing_name <- sum(is.na(bc_stations$station_name))
cat("Missing latitude/longitude:", missing_geo, "\n")
cat("Missing station name:", missing_name, "\n")

# Duplicates
dup_ids <- bc_stations %>% filter(duplicated(id)) %>% nrow()
cat("Duplicate IDs:", dup_ids, "\n")

# Out of BC bounds
out_of_bounds <- bc_stations %>%
  filter(latitude < 48 | latitude > 60 | longitude < -139 | longitude > -114) %>%
  nrow()
cat("Outside BC bounds:", out_of_bounds, "\n")

# --- 4. CLEAN DATA ---
cat("\n=== CLEANING DATA ===\n")

bc_clean <- bc_stations %>%
  # Remove duplicates
  distinct(id, .keep_all = TRUE) %>%
  
  # Filter valid BC coordinates
  filter(between(latitude, 48, 60), between(longitude, -139, -114)) %>%
  
  # Clean port counts
  mutate(
    ev_level2_evse_num = pmax(ev_level2_evse_num, 0, na.rm = TRUE),
    ev_dc_fast_num = pmax(ev_dc_fast_num, 0, na.rm = TRUE),
    
    # Standardize access
    access = case_when(
      str_to_lower(access_code) %in% c("public", "pub") ~ "public",
      str_to_lower(access_code) %in% c("private", "priv") ~ "private",
      TRUE ~ "unknown"
    ),
    
    # Derived features
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

cat("After cleaning:", nrow(bc_clean), "valid stations\n")


# --- 7. PLOT: Charging Type by Access ---
cat("\nGenerating bar plot...\n")
p <- ggplot(bc_clean, aes(x = charging_type, fill = access)) +
  geom_bar(position = "dodge") +
  labs(
    title = "EV Charging Stations in BC by Type and Access",
    x = "Charging Type",
    y = "Number of Stations",
    fill = "Access"
  ) +
  scale_fill_manual(values = c("public" = "#1f78b4", "private" = "#e31a1c", "unknown" = "gray")) +
  theme_minimal(base_size = 12)

ggsave("bc_charging_type_plot.png", p, width = 11, height = 7, dpi = 150)
cat("Plot saved: bc_charging_type_plot.png\n")

# --- 8. INTERACTIVE MAP ---
cat("\nGenerating interactive map...\n")
map <- leaflet(bc_clean) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = ~ifelse(has_dc_fast, 7, 5),
    color = ~ifelse(access == "public", "#1f78b4", "#e31a1c"),
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~paste0(
      "<b>", station_name, "</b><br>",
      street_address, "<br>",
      city, ", BC<br>",
      "Level 2 Ports: ", ev_level2_evse_num, "<br>",
      "DC Fast Ports: ", ev_dc_fast_num, "<br>",
      "Access: ", access
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("#1f78b4", "#e31a1c", "gray"),
    labels = c("Public", "Private", "Unknown"),
    title = "Access Type"
  )

htmlwidgets::saveWidget(map, "bc_ev_stations_map.html", selfcontained = TRUE)
cat("Interactive map saved: bc_ev_stations_map.html\n")

# --- 9. EXPORT CLEANED DATA ---
write.csv(bc_clean, "bc_electric_charging_stations_CLEAN.csv", row.names = FALSE)
