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

# TODO: Create a simulation to see how much the network can take before failure