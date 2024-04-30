library(spotifyr)
library(gganimate)
library(lubridate)
library(gghighlight)
library(reshape2)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)
library(jsonlite)
library(igraph)
library(networkD3)
library(viridis)



# Set Spotify client ID and client secret
Sys.setenv(SPOTIFY_CLIENT_ID = 'YOUR_CLIENT_ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOUR_CLIENT_SECRET')

# Define the scopes
scopes <- c(
  "user-library-read",
  "playlist-read-private",
  "playlist-read-collaborative",
  "user-read-recently-played"
)

# Get the Spotify authorization code
access_token <- get_spotify_authorization_code(
  client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
  client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
  scope = scopes
)

# Print or use the access token
print(access_token)



# GET SPECIFIC PLAYLIST FEATURES
playlist_username <- "INSERT_PLAYLIST_USERNAME"
playlist_uris <- c("INSERT_PLAYLIST_URI")
playlistFavsEnglish <- get_playlist_audio_features(playlist_username, playlist_uris)

# PLOT LESS POPULARITY TRACKS ON SPECIFIC PLAYLIST
playlistFavsEnglish %>%
  group_by(track.popularity) %>%
  filter(track.popularity <= "35") %>%
  ggplot(aes(x = track.name, y = track.popularity)) +
  geom_col(aes(fill = track.album.name)) +
  labs(x= "Track name", y= "Popularity") +
  ggtitle("What are the least popular songs I listen to on Spotify?", "Popularity ranking < 35 in a specific playlist") +
  theme(axis.text.x = element_text(angle = 90))

# GET FAVORITE TRACKS (modified)
top_tracks <- get_my_recently_played(
  limit = 10,
  after = NULL,
  before = NULL,
  authorization = access_token,
  include_meta_info = FALSE
)  # Change the limit as needed
top_tracks_data <- data.frame(
  Track = top_tracks$track.name,
  Popularity = top_tracks$track.popularity
)
print(top_tracks)

ggplot(top_tracks_data, aes(x = Track, y = Popularity)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Top 10 Tracks by Popularity",
       x = "Track",
       y = "Popularity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Fetch liked tracks
liked_tracks <- get_my_saved_tracks(limit = 50, authorization = access_token)
if (!"track.artists" %in% names(liked_tracks)) {
  stop("Expected column 'track.artists' does not exist in the data. Please check the data structure with str(liked_tracks).")
}
if (nrow(liked_tracks) == 0) {
  stop("No liked tracks found. Please ensure you have liked tracks in your Spotify account.")
}

# Extract the first added liked track
if (any(is.na(liked_tracks$added_at))) {
  stop("Missing data in 'added_at'. Cannot determine the first added liked track.")
}
first_added_liked_track <- liked_tracks[which.min(liked_tracks$added_at), ]

# Extract and analyze top artists from liked tracks
liked_artists <- tryCatch({
  liked_tracks %>%
    unnest(track.artists) %>%
    group_by(name) %>%
    summarize(TrackCount = n(), .groups = "drop") %>%
    arrange(desc(TrackCount)) %>%
    head(10)
}, error = function(e) {
  stop("An error occurred during artist extraction: ", e$message)
})

# Plot top 10 artists
tryCatch({
  ggplot(liked_artists, aes(x = reorder(name, -TrackCount), y = TrackCount, fill = name)) +
    geom_col() +
    labs(title = "Top 10 Artists Based on Liked Tracks",
         x = "Artist",
         y = "Number of Liked Tracks") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
          legend.position = "none")
}, error = function(e) {
  cat("An error occurred while plotting: ", e$message, "\n")
})


print(liked_artists)


# Extract artist IDs correctly
artist_details <- liked_tracks %>%
  unnest(tracks.artists) %>%
  distinct(artist.id, .keep_all = TRUE)

# Check the structure
str(artist_details)

# Retrieve genres for each artist
artist_genres <- map_df(artist_details$artist.id, function(id) {
  artist_info <- tryCatch({
    get_artist(id)
  }, error = function(e) {
    message(paste("Failed to fetch artist with ID:", id, "Error:", e$message))
    return(data.frame())  # Return an empty data frame on failure
  })
  if (!is.null(artist_info) && "genres" %in% names(artist_info)) {
    return(data.frame(artist_name = artist_info$name, genre = artist_info$genres))
  } else {
    return(data.frame())  # Return an empty data frame if no genre data
  }
})

# Unnest genres and count them
genre_counts <- artist_genres %>%
  unnest(genre) %>%
  count(genre, sort = TRUE) %>%
  top_n(10, n)

# Plot genre distribution
ggplot(genre_counts, aes(x = reorder(genre, n), y = n, fill = genre)) +
  geom_col() +
  labs(title = "Top Genres Among Liked Artists",
       x = "Genre",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Check the structure of liked_tracks to ensure the correct columns are accessed
str(liked_tracks)

