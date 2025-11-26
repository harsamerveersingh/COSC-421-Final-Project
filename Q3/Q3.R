# Q3.R — Critical Hub Identification
# Author: Abijeet Dhillon
# Purpose: Which charging stations that are in place currently are ‘critical hubs’ in the network?
# Methodology: Identify critical charging stations in BC using Degree and PageRank centralities.

# Import libraries
install.packages(c("igraph", "geosphere"))
library(igraph)
library(geosphere)

# Load and clean the station data
stations <- read.csv("bc_electric_charging_stations_CLEAN.csv")
stations_clean <- stations[!is.na(stations$latitude) & !is.na(stations$longitude), ]
stations_clean <- stations_clean[, c(
  "id", "station_name", "city",
  "latitude", "longitude",
  "ev_network", "access_code",
  "ev_level2_evse_num", "ev_dc_fast_num",
  "access_days_time", "access", "charging_type",
  "has_level2", "has_dc_fast"
)]

# Create a distance matrix & define distance threshold (<= 10km is an edge)
coordinates <- stations_clean[, c("longitude", "latitude")]
dist_matrix <- distm(coordinates, fun = distHaversine)
threshold <- 10000
adj_matrix <- dist_matrix < threshold
diag(adj_matrix) <- FALSE

# Build the graph
graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected")
V(graph)$name <- stations_clean$station_name
V(graph)$city <- stations_clean$city

# Compute degree and pagerank centralities
V(graph)$degree_centrality <- degree(graph, mode = "all")
V(graph)$pagerank_centrality <- page_rank(graph, directed = FALSE)$vector

# Create a dataframe to store our critical hubs
stations_df <- data.frame(
  id = stations_clean$id,
  station_name = V(graph)$name,
  city = V(graph)$city,
  degree_centrality = V(graph)$degree_centrality,
  pagerank_centrality = V(graph)$pagerank_centrality
)

# Normalize and combine degree & pagerank into a hub score in [0, 1] with
# degree centrality weighing 20% and pagerank centrality weighing 80%
degree_centrality_normalized <- normalize_centralities(stations_df$degree_centrality)
pagerank_centrality_normalized <- normalize_centralities(stations_df$pagerank_centrality)

stations_df$degree_centrality_normalized <- degree_centrality_normalized
stations_df$pagerank_centrality_normalized <- pagerank_centrality_normalized

stations_df$hub_score <- 0.05 * degree_centrality_normalized +
  0.95 * pagerank_centrality_normalized

# Function to normalize the degree and pagerank centralities
normalize_centralities <- function(x) {
  value_range <- range(x, na.rm = TRUE)
  if (diff(value_range) == 0) {
    return(rep(0, length(x)))
  }
  (x - value_range[1]) / diff(value_range)
}

# Sort stations by degree centrality in decreasing order & define how many critical hubs we want
number_of_stations <- 50
stations_df_sorted_by_degree_centrality <- stations_df[order(stations_df$degree_centrality, decreasing = TRUE), ]
sorted_stations_by_degree_centrality <- stations_df_sorted_by_degree_centrality[1:number_of_stations, ]

# Sort stations by pagerank centrality in decreasing order
stations_df_sorted_by_pagerank_centrality <- stations_df[order(stations_df$pagerank_centrality, decreasing = TRUE), ]
sorted_stations_by_pagerank_centrality <- stations_df_sorted_by_pagerank_centrality[1:number_of_stations, ]

# Sort stations by hub score in decreasing order
stations_df_sorted_by_hub_score <- stations_df[order(stations_df$hub_score, decreasing = TRUE), ]
sorted_stations_by_hub_score <- stations_df_sorted_by_hub_score[1:number_of_stations, ]

# Join back information for our three data frames by merging with the original stations
top_stations_by_degree_centrality <- merge(
  sorted_stations_by_degree_centrality,
  stations_clean,
  by = "id"
)
top_stations_by_degree_centrality <- top_stations_by_degree_centrality[order(top_stations_by_degree_centrality$degree_centrality, decreasing = TRUE), ]

top_stations_by_pagerank_centrality <- merge(
  sorted_stations_by_pagerank_centrality,
  stations_clean,
  by = "id"
)
top_stations_by_pagerank_centrality <- top_stations_by_pagerank_centrality[order(top_stations_by_pagerank_centrality$pagerank_centrality, decreasing = TRUE), ]

top_stations_by_hub_score <- merge(
  sorted_stations_by_hub_score,
  stations_clean,
  by = "id"
)
top_stations_by_hub_score <- top_stations_by_hub_score[order(top_stations_by_hub_score$hub_score, decreasing = TRUE), ]

# Print sorted data frames calculated above
cat("\nTop Stations by Degree Centrality:\n")
top_stations_by_degree_centrality
cat("\nTop Stations by PageRank Centrality:\n")
top_stations_by_pagerank_centrality
cat("\nTop Stations by Calculated Hub Score:\n")
top_stations_by_hub_score

# Create summary tables
hubs_by_city <- table(top_stations_by_hub_score$city.x, dnn = c("City"))
cat("\nHubs By City:\n")
hubs_by_city
hubs_by_ev_network <- table(top_stations_by_hub_score$ev_network, dnn = c("EV Network"))
cat("\nHubs By EV Network:\n")
hubs_by_ev_network
hubs_by_access <- table(top_stations_by_hub_score$access, dnn = c("Access"))
cat("\nHubs By Access:\n")
hubs_by_access

# Create a visual of a scatter plot to show the degree centrality vs pagerank centrality
plot(stations_df$degree_centrality_normalized, stations_df$pagerank_centrality_normalized,
  xlab = "Normalized Degree Centrality", ylab = "Normalized PageRank Centrality",
  main = "Degree vs PageRank Centrality", col = "grey"
)

points(
  stations_df$degree_centrality_normalized[stations_df$id %in% top_stations_by_hub_score$id], 
  stations_df$pagerank_centrality_normalized[stations_df$id %in% top_stations_by_hub_score$id],
  col = "red"
)

legend(
  "bottomright", legend = c("Not Critical Hubs", "Critical Hubs"),
  col = c("grey", "red")
)

# Print out critical hubs into a csv with their information
write.csv(top_stations_by_hub_score, "./Q3/output/critical_hubs_bc.csv")
