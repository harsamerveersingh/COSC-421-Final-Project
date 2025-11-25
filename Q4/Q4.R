library(tidyverse)
library(igraph)
library(geosphere)

# Loading and cleaning the stations list
stations <- read.csv("bc_electric_charging_stations.csv")
stations_clean <- stations[!is.na(stations$latitude) & !is.na(stations$longitude), ]
stations_clean <- stations_clean[, c("id", "station_name", "city",
                                     "latitude", "longitude",
                                     "ev_network", "access_code",
                                     "ev_level2_evse_num", "ev_dc_fast_num")]

# getting lat/long coordinates and putting it in a distance matrix
coordinates <- stations_clean[, c("longitude", "latitude")]
dist_matrix <- distm(coordinates, fun = distHaversine)

threshold <- 10000 # Within 10km counts as an edge

# creating adjacency matrix for nodes & ensuring nodes don't refer to self
adj_matrix <- dist_matrix < threshold
diag(adj_matrix) <- FALSE

# Initial graph creation
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected")
V(g)$name <- stations_clean$station_name
V(g)$city <- stations_clean$city

# centralities per node. Only using betweenness as of now though
V(g)$degree <- degree(g)
V(g)$betweenness <- betweenness(g)
V(g)$closeness <- closeness(g)

# Simplified vectors with degree and name
betweenness_vals <- setNames(V(g)$betweenness, V(g)$name)

# most critical stations
top_betweenness <- sort(betweenness_vals, decreasing = TRUE)[1:10]
top_betweenness_df <- data.frame(
  station_name = names(top_betweenness),
  city = V(g)[names(top_betweenness)]$city,
  betweenness = top_betweenness
)
print(top_betweenness_df)

# ranked list of highest betweenness nodes
removal_order <- names(sort(betweenness_vals, decreasing = TRUE))

# Storage for proportion of nodes in largest component
largest_component_sizes <- numeric(length(removal_order))

# Storage for dropoff magnitudes
dropoffs <- numeric(length(removal_order))

# Clone of g to test for failure
g_fail <- g 

for (i in seq_along(removal_order)) {
  
  # Remove the station
  g_fail <- delete_vertices(g_fail, removal_order[i])
  
  # Measure connectedness
  comp <- components(g_fail)
  
  # Track largest component proportion
  current <- max(comp$csize) / gorder(g_fail)
  largest_component_sizes[i] <- current
  
  # Compute dropoff from previous step (i > 1)
  if (i > 1) {
    dropoffs[i] <- largest_component_sizes[i-1] - current
  }
}

# Identify indices where the drop exceeds a chosen threshold
threshold <- 0.01   # 5% sudden drop
significant_drop_indices <- which(dropoffs > threshold)

significant_drop_indices
