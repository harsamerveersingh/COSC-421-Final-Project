library(igraph)
library(geosphere)
library(readr)
# Load the data
stations <- read_csv("Dataset/EV_stations/bc_electric_charging_stations_CLEAN.csv")

# Clean stations
stations_clean <- stations[!is.na(stations$latitude) & !is.na(stations$longitude), ]
stations_clean <- stations_clean[, c("id", "station_name", "city", "latitude", "longitude")]

# Distance matrix in meters
coords <- stations_clean[, c("longitude", "latitude")]

dist_matrix <- distm(coords, fun = distHaversine)
threshold <- 10000

# All i,j where distance <= 10 km
idx <- which(dist_matrix <= threshold & dist_matrix > 0, arr.ind = TRUE)

# Keep only upper triangle to avoid duplicates
idx <- idx[idx[,1] < idx[,2], ]

# Build edge list
edge_list <- data.frame(
  from = stations_clean$id[idx[,1]],
  to   = stations_clean$id[idx[,2]],
  weight = dist_matrix[idx]
)

cat("creating weighted graph)")
# Create weighted graph
g <- graph_from_data_frame(
  edge_list, 
  directed = FALSE,
  vertices = stations_clean
)
summary(g)
baseline <- distances(g, weights = E(g)$weight)

#betweeness to narrow down the search
betw <- betweenness(g, directed = FALSE, weights = E(g)$weight, normalized=TRUE)

avg_dist <- rowMeans(baseline)
deg <- degree(g)

top_k <- 100  # number of top stations to simulate
top_ids <- names(sort(betw, decreasing = TRUE))[1:top_k]

top_far <- names(sort(avg_dist, decreasing=TRUE))[1:20] # peripheral nodes
top_deg <- names(sort(deg, decreasing=TRUE))[1:40] #high degree nodes

#merge the unique outlier nodes
selected_nodes <- unique(c(top_ids, top_far, top_deg))

results <- data.frame(
  station_removed = selected_nodes,
  avg_extra_distance_km = numeric(length(selected_nodes)),
  max_extra_distance_km = numeric(length(selected_nodes))
)

for(i in seq_along(selected_nodes)){
  station <- selected_nodes[i]
  cat("Processing", i, "of", length(selected_nodes), "station:", station, "\n")
  
  #remove station
  g_fail <- delete_vertices(g, station)
  remaining <-setdiff(V(g)$name, station)
  
  dist_after <- distances(g_fail, v = remaining, to = remaining, weights = E(g_fail)$weight)
  
  dist_before <- baseline[remaining, remaining]
  
  extra_dist = dist_after - dist_before
  
  #values must be finite and positive
  idx <- which(is.finite(extra_dist) & extra_dist > 0)
  
  if (length(idx) > 0) {
    results$avg_extra_distance_km[i] <- mean(extra_dist[idx]) / 1000
    results$max_extra_distance_km[i] <- max(extra_dist[idx]) / 1000
  } else {
    results$avg_extra_distance_km[i] <- 0
    results$max_extra_distance_km[i] <- 0
  }
  
}
results_sorted <- results[order(-results$avg_extra_distance_km), ]
results <- results_sorted

write.csv(results, "Q2/Q2_station_removal_results_by_avg.csv", row.names = FALSE)

results_sorted <- results[order(-results$max_extra_distance_km), ]
results <- results_sorted

write.csv(results, "Q2/Q2_station_removal_by_max.csv")

cat("Done! Results saved to Q2_station_removal_results.csv\n")


#special edge case for nodes that have degree 0 under the current thresshold while also being on the outsides of the network
# Load your results
results <- read.csv("Q2/Q2_station_removal_by_max.csv")
# Suppose your CSV of results is loaded as `results`
isolated_nodes <- results$station_removed[results$max_extra_distance_km < 1e-6]

# Pick the first isolated node
nearest_info <- data.frame(
  station_id = isolated_nodes,
  station_name = NA_character_,
  nearest_id = NA_character_,
  nearest_name = NA_character_,
  nearest_dist_m = numeric(length(isolated_nodes)),
  stringsAsFactors = FALSE
)


for (i in seq_along(isolated_nodes)) {
  iso_id <- isolated_nodes[i]
  iso_index <- match(iso_id, stations_clean$id)
  
  # distances to all other stations
  dists <- dist_matrix[iso_index, ]
  dists[iso_index] <- Inf  # ignore self
  
  nearest_index <- which.min(dists)
  
  nearest_info$station_name[i] <- stations_clean$station_name[iso_index]
  nearest_info$nearest_id[i] <- stations_clean$id[nearest_index]
  nearest_info$nearest_name[i] <- stations_clean$station_name[nearest_index]
  nearest_info$nearest_dist_m[i] <- dists[nearest_index]
}
nearest_info
write.csv(nearest_info, "Q2/isolated_nodes_nearest.csv", row.names = FALSE)




