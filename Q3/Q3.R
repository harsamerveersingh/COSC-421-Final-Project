# Q3.R — Critical Hub Identification
# Author: Abijeet Dhillon
# Purpose: Which charging stations that are in place currently are ‘critical hubs’ in the network?
#          Identify critical EV charging stations in BC using Degree and PageRank centrality.

# libraries
install.packages(c("tidyverse","igraph","geosphere","ggplot2"))
library(tidyverse)
library(igraph)
library(geosphere)
library(ggplot2)

# load and clean data
stations <- read.csv("./Dataset/EV_stations/bc_electric_charging_stations.csv")
stations_clean <- stations[!is.na(stations$latitude) & !is.na(stations$longitude), ]
stations_clean <- stations_clean[, c("id", "station_name", "city",
                                     "latitude", "longitude",
                                     "ev_network", "access_code",
                                     "ev_level2_evse_num", "ev_dc_fast_num")]

# create distance matrix
coordinates <- stations_clean[, c("longitude", "latitude")]
dist_matrix <- distm(coordinates, fun = distHaversine)

# define distance threshold (10 km)
threshold <- 10000
adj_matrix <- dist_matrix < threshold
diag(adj_matrix) <- FALSE

# build graph
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected")
V(g)$name <- stations_clean$station_name
V(g)$city <- stations_clean$city

# compute centrality metrics
V(g)$degree <- degree(g, mode = "all")
V(g)$pagerank <- page_rank(g, directed = FALSE)$vector

# rank top critical hubs
centrality_df <- data.frame(
  station_name = V(g)$name,
  city = V(g)$city,
  degree = V(g)$degree,
  pagerank = V(g)$pagerank
)

# top 10 by degree
top_degree <- centrality_df %>%
  arrange(desc(degree)) %>%
  slice_head(n = 10)

# top 10 by pagerank
top_pagerank <- centrality_df %>%
  arrange(desc(pagerank)) %>%
  slice_head(n = 10)

# print results
cat("\nTop 10 Stations by Degree Centrality:\n")
print(top_degree)

cat("\nTop 10 Stations by PageRank Centrality:\n")
print(top_pagerank)

