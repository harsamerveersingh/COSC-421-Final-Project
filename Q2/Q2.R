library(igraph)
library(geosphere)

# Load the data
stations <- read.csv("C:/Users/nicho/Cosc421_Final/COSC-421-Final-Project/Dataset/EV_stations/bc_electric_charging_stations_CLEAN.csv")

# Clean stations
stations_clean <- stations[!is.na(stations$latitude) & !is.na(stations$longitude), ]
stations_clean <- stations_clean[, c("id", "station_name", "city", "latitude", "longitude")]

# Distance matrix in meters
coords <- stations_clean[, c("longitude", "latitude")]
dist_matrix <- distm(coords, fun = distHaversine)

# Adjacency for edges within 10 km
threshold <- 10000
adj_matrix <- dist_matrix <= threshold
diag(adj_matrix) <- 0

weight_matrix <- dist_matrix
weight_matrix[!adj_matrix] <- 0
diag(weight_matrix) <- 0


# Create weighted graph
g <- graph_from_adjacency_matrix(weight_matrix, mode = "undirected", weighted = TRUE)
V(g)$name <- stations_clean$station_name
V(g)$city <- stations_clean$city

summary(g)
baseline <- distances(g, weights = E(g)$weight)

# All stations to iterate over
stations_list <- V(g)$name
n <- length(stations_list)

head(data.frame(
  from = as.character(ends(g, E(g))[ ,1]),
  to = as.character(ends(g, E(g))[ ,2]),
  weight = E(g)$weight
))

# Prepare results
results <- data.frame(
  station_removed = stations_list,
  avg_extra_distance_km = numeric(n),
  max_extra_distance_km = numeric(n)
)

cat("Starting failure scenario analysis for", n, "stations...\n")

for (i in seq_along(stations_list)) {
  station <- stations_list[i]
  cat("Processing station", i, "of", n, ":", station, "\n")
  
  # Delete station from graph
  g_fail <- delete_vertices(g, station)
  
  # Remaining station names
  remaining_names <- setdiff(stations_list, station)
  
  # Distances after removal (subset only the remaining stations)
  dist_after <- distances(g_fail, v = remaining_names, to = remaining_names, weights = E(g_fail)$weight)
  
  # Distances before removal
  dist_before <- baseline[remaining_names, remaining_names]
  
  # Extra distance
  extra_dist <- dist_after - dist_before
  
  # Keep only finite positive distances
  valid_idx <- which(is.finite(extra_dist) & extra_dist > 0)
  
  # Compute summary statistics safely
  results$avg_extra_distance_km[i] <- if(length(valid_idx) > 0) mean(extra_dist[valid_idx])/1000 else 0
  results$max_extra_distance_km[i] <- if(length(valid_idx) > 0) max(extra_dist[valid_idx])/1000 else 0
}

# Save results to CSV
write.csv(results, "Q2_station_removal_results_full.csv", row.names = FALSE)

head(results)