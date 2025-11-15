# Title: EV Underserved Priority Analysis
# Author: Harsamerveer Singh
# Date: 11/14/2025
# Goal: Identify areas for prioritizing backup EV station placement based on 
#       Population, Network Accessibility, and Existing EV Stations.

# --- SETUP AND DATA LOADING ---
install.packages("igraph")
install.packages("dplyr") 
library(igraph)
library(dplyr)

# setwd("H:/COSC-421-Final-Project/Dataset/EV_stations/learning_purpose/learning_purpose")
print(paste("Current Working Directory:", getwd()))

# 1. Load Area Population Data
area_df <- read.csv("bc_area_population.csv", 
                    header = TRUE, stringsAsFactors = FALSE)
area_df$Population <- as.numeric(area_df$Population)

# Store the list of valid AreaIDs (Cities) to filter the EV station data
valid_area_ids <- area_df$AreaID
rownames(area_df) <- area_df$AreaID # Set rownames for robust graph attribute lookup

# --- DEBUG STEP 1: INITIAL DATA AND ENVIRONMENT CHECK ---
# Checks for missing or invalid population data
if(sum(is.na(area_df$Population)) > 0) {
  warning("NA values found in Population after conversion. Check input data.")
}
print(head(area_df[, c("AreaID", "Population")], 5))
# --------------------------------------------------------


# 2. Load and Aggregate EV Station Data
# NOTE: Assumes EV stations file is named 'bc_electric_charging_stations_CLEAN.csv'
# and the city column is named 'city' (lowercase).
ev_stations_df <- read.csv("bc_electric_charging_stations_CLEAN.csv", 
                           header = TRUE, stringsAsFactors = FALSE)

# Crucial Step: Filter EV stations to ONLY include cities (AreaIDs) present in area_df
filtered_stations_df <- ev_stations_df %>% 
  filter(city %in% valid_area_ids)

# 3. Aggregate: Calculate the total number of stations per AreaID (City)
ev_station_counts <- filtered_stations_df %>% 
  group_by(AreaID = city) %>% # Group by 'city', rename column to 'AreaID' for join
  summarise(EV_Stations = n())

# --- DEBUG STEP 2: EV STATION INTEGRATION CHECK ---
if(nrow(filtered_stations_df) == 0) {
  stop("No EV stations found after filtering. Check if 'city' column names match 'AreaID' names (case sensitive).")
}
print(head(ev_station_counts, 5))
# --------------------------------------------------------


# 4. Merge the station counts into the main area_df
area_df <- area_df %>% 
  left_join(ev_station_counts, by = "AreaID")

# Replace NAs (areas in network with no stations) with 0
area_df$EV_Stations[is.na(area_df$EV_Stations)] <- 0
rownames(area_df) <- area_df$AreaID # Re-set rownames after join


# 5. Load Network Edges and Build Graph
network_edges_data <- read.csv("bc_network_edges.csv", 
                               header = TRUE, 
                               stringsAsFactors = FALSE)

network_graph <- graph_from_data_frame(d = network_edges_data,
                                       vertices = area_df$AreaID,
                                       directed = FALSE)

# 6. Assign Attributes to Graph Vertices (Using rownames for robust indexing)
V(network_graph)$Population <- area_df[V(network_graph)$name, "Population"]
V(network_graph)$EV_Stations <- area_df[V(network_graph)$name, "EV_Stations"] 

# --- DEBUG STEP 3: GRAPH ATTRIBUTE CHECK ---
if(any(is.na(V(network_graph)$Population))) {
  stop("NA values found when assigning Population to graph. Check AreaID/rownames correspondence.")
}
# --------------------------------------------------------

# 7. Closeness Centrality Calculation
closeness_scores <- closeness(network_graph, normalized = TRUE)

results_df <- data.frame(
  AreaID = V(network_graph)$name,
  Population = V(network_graph)$Population,
  EV_Stations = V(network_graph)$EV_Stations,
  ClosenessCentrality = closeness_scores
)

# Handle zero closeness centrality: replace 0 with a small fraction of the minimum non-zero value
min_non_zero_closeness <- min(results_df$ClosenessCentrality[results_df$ClosenessCentrality > 0])
results_df$ClosenessCentrality[results_df$ClosenessCentrality == 0] <- min_non_zero_closeness / 10

# 8. Calculate New EV_UnderservedScore
# Proposed Metric: (Population * (1 + Mean Stations)) / (Closeness Centrality * (Existing EV Stations per Area + 1))
mean_stations <- mean(results_df$EV_Stations)
results_df$EV_UnderservedScore <- (results_df$Population * (1 + mean_stations)) / 
  (results_df$ClosenessCentrality * (results_df$EV_Stations + 1))
results_df$PriorityRank <- rank(-results_df$EV_UnderservedScore)


# 9. Prioritization and Output
print("--- BC Cities EV Underserved Score Analysis (Higher Score = Higher Priority) ---")
top_results <- results_df[order(results_df$EV_UnderservedScore, decreasing = TRUE), ]
print(head(top_results, 15)) # Print top 15 results

write.csv(top_results, "BC_Cities_EV_Underserved_Score.csv", row.names = FALSE)


# 10. Visualization 
# Use the 'match' function for reliable indexing of the score, preventing 'invalid indexing' error
V(network_graph)$score <- results_df[match(V(network_graph)$name, results_df$AreaID), "EV_UnderservedScore"]
V(network_graph)$size <- V(network_graph)$score / max(V(network_graph)$score, na.rm=TRUE) * 15 + 5 


# Plot the network
plot(network_graph,
     vertex.label = V(network_graph)$name,
     vertex.label.cex = 0.8,
     main = "BC EV Backup Station Priority Analysis (Redder/Larger = Higher Priority)"
)

