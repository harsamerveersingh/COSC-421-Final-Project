# Research Question1: 

# Metrics: Population of each area, closeness centrality of each area in the network, find underserved score = population/closeness centrality, highlighting areas with high population but low accessibility.

# Analysis: Identify areas with high underserved scores as priority zones. These are the areas where adding backup stations would likely have the most impact. Plot a graph to visualize results.



install.packages("igraph")
library(igraph)
getwd()
setwd("C:/Users/hsingh77.stu/Downloads/COSC-421-Final-Project/Dataset/EV_stations/learning_purpose/learning_purpose")
list.files()

area_df <- read.csv("bc_area_population.csv", 
                    header = TRUE,stringsAsFactors = FALSE)

area_df$Population <- as.numeric(area_df$Population)

rownames(area_df) <- area_df$AreaID

network_edges_data <- read.csv("C:/Users/hsingh77.stu/Downloads/COSC-421-Final-Project/Dataset/EV_stations/learning_purpose/learning_purpose/bc_network_edges.csv", 
                               header = TRUE, 
                               stringsAsFactors = FALSE)

network_graph <- graph_from_data_frame(d = network_edges_data,
                                       vertices = area_df$AreaID,
                                       directed = FALSE)
print(area_df)
print(network_edges_data)

V(network_graph)$Population <- area_df[V(network_graph)$name, "Population"]

closeness_scores <- closeness(network_graph, normalized = TRUE)

results_df <- data.frame(
  AreaID = V(network_graph)$name,
  Population = V(network_graph)$Population,
  ClosenessCentrality = closeness_scores
)
print(results_df)

min_non_zero_closeness <- min(results_df$ClosenessCentrality[results_df$ClosenessCentrality > 0])
results_df$ClosenessCentrality[results_df$ClosenessCentrality == 0] <- min_non_zero_closeness / 10
results_df$UnderservedScore <- results_df$Population / results_df$ClosenessCentrality

results_df$PriorityRank <- rank(-results_df$UnderservedScore)


print("BC Cities Underserved Score Analysis (Higher Score = Higher Priority):")
top_results <- results_df[order(results_df$UnderservedScore, decreasing = TRUE), ]
print(top_results)

write.csv(top_results, "BC_Cities_Underserved_Score.csv", row.names = FALSE)

plot(network_graph,
     vertex.label = V(network_graph)$name,
     vertex.label.cex = 0.8,
     main = "BC Cities: Underserved Priority Analysis "
)
