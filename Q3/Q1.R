# Title: EV Underserved Priority Analysis
# Author: Harsamerveer Singh
# Date: 11/25/2025
# Goal: Identify areas for prioritizing backup EV station placement based on 
#       Population, Network Accessibility, and Existing EV Stations.

install.packages("igraph")
install.packages("dplyr") 
library(igraph)
library(dplyr)

# setwd("")
print(paste("Current Working Directory:", getwd()))

# 1. Load Area Population Data
area_df <- read.csv("bc_area_population.csv", 
                    header = TRUE, stringsAsFactors = FALSE)
area_df$Population <- as.numeric(area_df$Population)

valid_area_ids <- area_df$AreaID
rownames(area_df) <- area_df$AreaID

# 2. Load and Aggregate EV Station Data
ev_stations_df <- read.csv("bc_electric_charging_stations_CLEAN.csv", 
                           header = TRUE, stringsAsFactors = FALSE)

filtered_stations_df <- ev_stations_df %>% 
  filter(city %in% valid_area_ids)

# 3. Calculate the total number of stations per AreaID (City)
ev_station_counts <- filtered_stations_df %>% 
  group_by(AreaID = city) %>% 
  summarise(EV_Stations = n())


print(head(ev_station_counts, 5))


# 4. Merge the station counts into the main area_df
area_df <- area_df %>% 
  left_join(ev_station_counts, by = "AreaID")

print(area_df)

if(!"EV_Stations" %in% colnames(area_df)) {
  area_df$EV_Stations <- 0
} else {
  area_df$EV_Stations[is.na(area_df$EV_Stations)] <- 0
}

nrow(ev_station_counts)

head(ev_station_counts)

# 5. Load Network Edges and Build Graph
network_edges_data <- read.csv("bc_network_edges.csv", 
                               header = TRUE, 
                               stringsAsFactors = FALSE)

network_graph <- graph_from_data_frame(d = network_edges_data,
                                       vertices = area_df$AreaID,
                                       directed = FALSE)

# 6. Assign Attributes to Graph Vertices
V(network_graph)$Population <- area_df[V(network_graph)$name, "Population"]
V(network_graph)$EV_Stations <- area_df[V(network_graph)$name, "EV_Stations"] 


# 7. Closeness Centrality Calculation
closeness_scores <- closeness(network_graph, normalized = TRUE)
print(closeness_scores)
V(network_graph)$population <- area_df$Population[match(V(network_graph)$name, area_df$AreaID)]
V(network_graph)$ev_stations <- area_df$EV_Stations[match(V(network_graph)$name, area_df$AreaID)]

results_df <- data.frame(
  AreaID = V(network_graph)$name,
  Population = V(network_graph)$population,
  EV_Stations = V(network_graph)$ev_stations,
  ClosenessCentrality = closeness_scores
)
print(results_df)
# Handle zero closeness centrality replace 0 with a small fraction of the minimum non-zero value
min_non_zero_closeness <- min(results_df$ClosenessCentrality[results_df$ClosenessCentrality > 0])
results_df$ClosenessCentrality[results_df$ClosenessCentrality == 0] <- min_non_zero_closeness / 10

# 8. Calculate New EV_UnderservedScore
# Proposed Metric: (Population * (1 + Mean Stations)) / (Closeness Centrality * (Existing EV Stations per Area + 1))
mean_stations <- mean(results_df$EV_Stations)
print(mean_stations)
results_df$EV_UnderservedScore <- (results_df$Population * (1 + mean_stations)) / 
  (results_df$ClosenessCentrality * (results_df$EV_Stations + 1))
results_df$PriorityRank <- rank(-results_df$EV_UnderservedScore)
top_results <- results_df[order(results_df$EV_UnderservedScore, decreasing = TRUE), ]
write.csv(top_results, "BC_Cities_EV_Underserved_Score.csv", row.names = FALSE)

# Use the 'match' function for reliable indexing of the score, preventing 'invalid indexing' error
V(network_graph)$score <- results_df[match(V(network_graph)$name, results_df$AreaID), "EV_UnderservedScore"]
V(network_graph)$size <- V(network_graph)$score / max(V(network_graph)$score, na.rm=TRUE) * 15 + 5 


# Plot the network
plot(network_graph,
     vertex.label = V(network_graph)$name,
     vertex.label.cex = 0.8,
     main = "BC EV Backup Station Priority Analysis"
)
