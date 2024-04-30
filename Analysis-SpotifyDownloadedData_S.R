library(gganimate)
library(lubridate)
library(gghighlight)
library(spotifyr)
library(reshape2)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)
library(jsonlite)
library(igraph)
library(networkD3)
library(viridis)



# Load and prepare data
#streaming_history <- fromJSON("/Users/simarjeetss529/Desktop/saar/uni/spr 24/elements of network science /spotify data analysis/StreamingHistory_music_0.json", flatten = TRUE)

streaming_history <- fromJSON("StreamingHistory_music_0_S.json", flatten = TRUE)

# Convert msPlayed to minutes
streaming_history$minutesPlayed <- streaming_history$msPlayed / 60000

#simple graph where each artist is connected to the next in the streaming history
edges <- data.frame(from = head(streaming_history$artistName, -1),
                    to = tail(streaming_history$artistName, -1))


#Here nodes = artists & edges = listening transitions
artist_network <- graph_from_data_frame(edges, directed = TRUE)
plot(artist_network)


# Create edges for artist transition network
edges <- streaming_history %>%
  arrange(endTime) %>%
  mutate(next_artist = lead(artistName)) %>%
  filter(!is.na(next_artist)) %>%
  select(artistName, next_artist)

# Build the graph
artist_network <- graph_from_data_frame(dplyr::select(edges, artistName, next_artist), directed = TRUE)

# Community detection
communities <- cluster_walktrap(artist_network)

# Plot community detection results with improved layout and styling
plot(communities, artist_network, 
     vertex.size = 10, 
     vertex.label.cex = 0.5,
     vertex.color = rainbow(length(unique(membership(communities))))[membership(communities)],
     edge.arrow.size = 0.5, 
     layout = layout_with_fr(artist_network),  # Use Fruchterman-Reingold layout for better visualization
     main = "Community Detection in Artist Transition Network")


# Filter nodes based on degree centrality
top_degree_nodes <- names(sort(degree(artist_network), decreasing = TRUE)[1:20])  # Select top 20 nodes by degree centrality

# Subgraph containing only top degree nodes
top_degree_subgraph <- induced_subgraph(artist_network, top_degree_nodes)

# Re-run community detection on the filtered subgraph
top_degree_communities <- cluster_walktrap(top_degree_subgraph)

# Plot community detection results with improved layout and styling
plot(top_degree_communities, top_degree_subgraph, 
     vertex.size = 10, 
     vertex.label.cex = 0.5,
     vertex.color = rainbow(length(unique(membership(top_degree_communities))))[membership(top_degree_communities)],
     edge.arrow.size = 0.5, 
     layout = layout_with_fr(top_degree_subgraph),  # Use Fruchterman-Reingold layout for better visualization
     main = "Community Detection in Top Degree Artist Transition Network")



# Define a custom color palette for communities
library(viridis)  # Load the viridis color palette library
community_colors <- viridis_pal(option = "C")(length(unique(membership(top_degree_communities))))

# Plot community detection results with improved layout and styling
plot(top_degree_communities, top_degree_subgraph, 
     vertex.size = 10, 
     vertex.label.cex = 0.8,
     vertex.color = community_colors[membership(top_degree_communities)],
     vertex.frame.color = "white",  # Add white border around nodes for better visibility
     edge.arrow.size = 0.5, 
     edge.color = "gray",  # Set edge color to gray
     edge.width = 1,  # Increase edge width for better visibility
     layout = layout_with_fr(top_degree_subgraph),  # Use Fruchterman-Reingold layout for better visualization
     main = "Community Detection in Top Degree Artist Transition Network",
     asp = 0.6)  # Adjust aspect ratio for better layout



# Calculate degrees
degree_data <- degree(artist_network, mode = "all")

# Assume an average song duration
average_duration_ms = 180000  # 3 minutes per song

# Classify interactions based on play duration
streaming_history <- streaming_history %>%
  mutate(interaction = ifelse(msPlayed < 0.1 * average_duration_ms, -1,
                              ifelse(msPlayed > 0.9 * average_duration_ms, 1, 0)))

# Filter and create a signed network
signed_edges <- streaming_history %>%
  filter(interaction != 0) %>%
  arrange(endTime) %>%
  mutate(next_artist = lead(artistName), next_interaction = lead(interaction)) %>%
  filter(!is.na(next_artist), !is.na(next_interaction))

# Create the signed network graph
signed_artist_network <- graph_from_data_frame(
  dplyr::select(signed_edges, artistName, next_artist, next_interaction),
  directed = TRUE)

# Assign weights for signed network
E(signed_artist_network)$weight <- signed_edges$next_interaction

# Visualize the signed network
plot(signed_artist_network,
     vertex.size = 10,
     vertex.label.cex = 0.5,
     edge.color = ifelse(E(signed_artist_network)$weight == 1, "green", "red"),
     layout = layout_nicely(signed_artist_network),
     main = "Signed Network: Positive (Green) & Negative (Red) Edges")


# Plot the artist network with degree data
plot(artist_network,
     vertex.size = degree_data,
     vertex.color = membership(communities),
     vertex.label.cex = 0.7,
     vertex.label.color = "darkblue",
     edge.arrow.size = 0.5,
     layout = layout_nicely(artist_network),
     main = "Artist Network with Node Sizes Based on Degree")

#temporal analysis
# Convert endTime to POSIXct format for better handling of date-time data
streaming_history$endTime <- as.POSIXct(streaming_history$endTime, format="%Y-%m-%d %H:%M")

# Aggregate listening by hour to see most active hours
streaming_hourly <- streaming_history %>%
  mutate(hour = format(endTime, "%H")) %>%
  group_by(hour) %>%
  summarize(TotalMinutes = sum(minutesPlayed))

# Plot listening habits by hour
ggplot(streaming_hourly, aes(x = hour, y = TotalMinutes)) +
  geom_line(group=1, color="blue") +
  labs(x = "Hour of Day", y = "Total Listening Minutes", title = "Listening Patterns by Hour of Day")


# Calculate and examine various network centrality measures
centrality_measures <- data.frame(
  degree = degree(artist_network, mode = "all"),
  betweenness = betweenness(artist_network, directed = TRUE),
  closeness = closeness(artist_network, normalized = TRUE)
)

# Add artist names to the centrality measures data frame
centrality_measures$artistName <- V(artist_network)$name

# View top 10 influential artists by degree centrality
top_artists <- centrality_measures %>%
  arrange(desc(degree)) %>%
  head(10)
print(top_artists)

# Combine plots for all centrality measures
centrality_melted <- melt(centrality_measures, id.vars = "artistName", variable.name = "CentralityType", value.name = "Value")

ggplot(centrality_melted, aes(x = reorder(artistName, -Value), y = Value, fill = CentralityType)) +
  geom_bar(stat = "identity") +
  facet_wrap(~CentralityType, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Artist Name", y = "Centrality Measure", title = "Centrality Measures Across Artists")
