library(igraph)
library(geosphere)

#initating my graph

stations <- read.csv("bc_electric_charging_stations_CLEAN.csv")

#clean stations contains only the necessary columns and no NA values
stations_clean <- stations[!is.na(stations$latitude) & !is.na(stations$longitude), ]
stations_clean <- stations_clean[, c("id", "station_name", "city", "latitude", "longitude")]

#matrix of distacnes between stations
coords <- stations_clean[, c("longitude", "latitude")]
dist_matrix <- distm(coords, fun = distHaversine) #dist in KM

#adjacency matrix
threshold <- 10000 #within 10 km

adj_matrix <- dist_matrix <= threshold
diag(adj_matrix) <- 0 #no self loops

weight_matrix <- dist_matrix
weight_matrix[!adj_matrix] <- 0
diag(weight_matrix) <- 0
#create the weighted graph with distances to neighbours as wieghts, nodes are the stations
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

V(g)$name <- stations_clean$station_name
V(g)$city <- stations_clean$city

summary(g)
#remove stations iteratively, see how much extra time is aded if a station is removed ON avergage
#also which station is the alternative the most often, given a different staton fails

#data frame of results
stations_list <- V(g)$name
n <- length(stations_list)

results <- data.frame(
  station_removed = stations_list,
  avg_extra_distance_km = numeric(n),
  max_extra_distance_km = numeric(n)
)

# Test with first 10 stations only
test_n <- min(10, n)
stations_list_test <- stations_list[1:test_n]

cat("Testing with", test_n, "stations...\n")
for (i in 1:test_n) {
    cat("Processing station", i, "of", test_n, "\n")
    station <- stations_list_test[i]
    g_fail <- delete_vertices(g, station)
    dist_after <- distances(g_fail, weights = E(g_fail)$weight)

    remaining <- setdiff(1:n, which(stations_list == station)[1])
    dist_before <- distances(
        g,
        v = remaining,
        to = remaining,
        weights = E(g)$weight
        )

    extra_dist <- dist_after - dist_before

    #Summary of results
    valid_idx <- which(is.finite(extra_dist) & extra_dist > 0)
    avg_dist <- mean(extra_dist[valid_idx], na.rm = TRUE) / 1000
    max_dist <- max(extra_dist[valid_idx], na.rm = TRUE) / 1000
    results$avg_extra_distance_km[i] <- avg_dist
    results$max_extra_distance_km[i] <- max_dist
}

# Keep only the tested rows
results <- results[1:test_n, ]

#Compute baseline shortest paths
write.csv(results, "Q2_station_removal_results.csv", row.names = FALSE)
head(results)