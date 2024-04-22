# REQUIRED LIBRARIES

library(lubridate)
library(gghighlight)
library(spotifyr)
library(knitr)
library(ggplot2)
library(plotly)
library(httpuv)
library(dplyr)
library(DT)
library(gt)
library(skimr) 
library(ggdist) 
library(showtext) 
library(purrr)

Sys.setenv(SPOTIFY_CLIENT_ID = 'YOUR_CLIENT_ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOUR_CLEINT_SECRET')

access_token <- get_spotify_access_token()

# GETTING AUDIO FEATURES OF A ARTIST - BASIC ANALYSIS

beatles <- get_artist_audio_features('the beatles')

beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kable()

# GET SPECIFIC PLAYLIST FEATURES

playlist_username <- 'INSERT_PLAYLIST_USERNAME' 
playlist_uris <- c('INSERT_PLAYLIST_URI') 

playlistFavs <- get_playlist_audio_features(playlist_username, playlist_uris)

# COLUMN NAMES OF THE PLAYLIST DATA FRAME OBTAINED
# colnames(playlistFavs)

# PLOT LESS POPULARITY TRACKS ON SPECIFIC PLAYLIST 

playlistFavs %>%
  mutate(track.popularity = as.numeric(track.popularity)) %>% # Convert to numeric if not already
  filter(track.popularity <= 10 & track.popularity != 0) %>% # Corrected logical condition
  ggplot(aes(x = track.name, y = track.popularity)) + 
  geom_col(aes(fill = track.album.name)) +
  labs(x = "Track name", y = "Popularity") + 
  ggtitle("What are the least popular songs I listen to on Spotify?", "Popularity ranking < 35 in a specific playlist") +
  theme(axis.text.x = element_text(angle = 90))
ggplotly()


# GET FAVORITE TRACKS

playlistFavs %>%
  mutate(track.popularity = as.numeric(track.popularity)) %>% # Convert to numeric if not already
  filter(track.popularity >= 90) %>% # Filter for tracks with popularity greater than or equal to 65
  ggplot(aes(x = track.name, y = track.popularity)) + 
  geom_col(aes(fill = track.album.name)) +
  labs(x = "Track name", y = "Popularity") + 
  ggtitle("What are the most popular songs I listen to on Spotify?", "Popularity ranking >= 65 in a specific playlist") +
  theme(axis.text.x = element_text(angle = 90))
ggplotly()

# GET FIRST ADDED LIKED TRACK

first_added_track <- playlistFavs %>%
  slice(1) %>%
  mutate(artist_names = paste(track.artists, collapse = ", ")) %>%
  select(track.name, track.album.release_date) %>%
  rename(Track_Name = track.name, Release_Date = track.album.release_date)


datatable(first_added_track, options = list(dom = 't'))

# GET TOP ARTISTS BASED ON LIKED TRACKS 

favTracksArtist <- playlistFavs %>%
  select(track.artists) %>%
  reduce(rbind) %>%
  reduce(rbind) %>%
  select(id, name)

trackNumArtist <- favTracksArtist %>%
  count(id, sort = TRUE) %>%
  left_join(favTracksArtist, by = 'id',.) %>%
  unique() %>%
  select(-id) %>%
  top_n(10, n)

# PLOT TOP 10 ARTISTS BASED ON LIKED TRACKS

plotMyFavs <- trackNumArtist %>%
  mutate(freq = case_when(n > 100 ~ '> 100 tracks',
                          between(n, 50, 99) ~ '50-99 tracks',
                          between(n, 20, 49) ~ '20-49 tracks',
                          TRUE ~ '< 20 tracks')) %>%
  mutate(freq = factor(freq, levels = c('> 100 tracks', '50-99 tracks', '20-49 tracks', '< 20 tracks'))) %>%
  ggplot(mapping = aes(x = reorder(name, -n), y = n, fill = freq)) +
  geom_col() +
  scale_fill_brewer(palette="Dark2") +
  labs(x= "Artist name", y= "Number of tracks", fill = NULL) +
  ggtitle("What are my Top 10 favorite artists?", "Based on my ♥ tracks") +
  theme(axis.text.x = element_text(angle = 90))
plotMyFavs
ggplotly()

# GET FEATURES TOP 10 FAVORITE ARTISTS

favArtist1 <- get_artist_audio_features(artist= "Drake") %>% 
  select(artist_name, track_name, album_name, key_mode, album_release_year, c(danceability:tempo)) 
favArtist2 <- get_artist_audio_features(artist= "BTS") %>% 
  select(artist_name, track_name, album_name, key_mode, album_release_year, c(danceability:tempo)) 
favArtist3 <- get_artist_audio_features(artist= "21 Savage") %>% 
  select(artist_name, track_name, album_name, key_mode, album_release_year, c(danceability:tempo)) 
favArtist4 <- get_artist_audio_features(artist= "Travis Scott") %>% 
  select(artist_name, track_name, album_name, key_mode, album_release_year, c(danceability:tempo)) 
favArtist5 <- get_artist_audio_features(artist= "Lil Baby") %>% 
  select(artist_name, track_name, album_name, key_mode, album_release_year, c(danceability:tempo)) 
favArtist6 <- get_artist_audio_features(artist= "Gunna") %>% 
  select(artist_name, track_name, album_name, key_mode, album_release_year, c(danceability:tempo)) 
favArtist7 <- get_artist_audio_features(artist= "Lil Durk") %>% 
  select(artist_name, track_name, album_name, key_mode, album_release_year, c(danceability:tempo)) 
favArtist8 <- get_artist_audio_features(artist= "Arjit Singh") %>% 
  select(artist_name, track_name, album_name, key_mode, album_release_year, c(danceability:tempo))
favArtist9 <- get_artist_audio_features(artist= "Metro Boomin") %>% 
  select(artist_name, track_name, album_name, key_mode, album_release_year, c(danceability:tempo)) 
favArtist10 <- get_artist_audio_features(artist= "AP Dhillon") %>% 
  select(artist_name, track_name, album_name, key_mode, album_release_year, c(danceability:tempo)) 

# LIST OF ARTISTS TO MAP THROUGH 

library(tibble)

# Assuming 'trackNumArtist' contains the necessary data
top_ten_artists <- trackNumArtist %>%
  mutate(Artist = case_when(
    name == "flor" ~ "0szWPxzzE8DVEfXFRCLBUb", # Artist ID from the provided resource
    TRUE ~ name  # Keep the artist name if it's not "flor"
  )) %>% 
  slice_max(order_by = n, n = 10) %>%
  pull(Artist)

# MAPPING THROUGH ALL THE ARTISTS

top_ten_df <- map_dfr(top_ten_artists, ~{
  get_artist_audio_features(.x) %>% 
    select(artist_name, track_name, album_name, key_mode, album_release_year, c(danceability:tempo)) 
})

# SKIMMING THE DATA

skim(top_ten_df) %>% 
  gt()

# MAKE A SINGLE DATA SET

topFourArtists <- rbind(favArtist1, favArtist2, favArtist3, favArtist4)

# PLOT EMOTIONAL QUADRANT TOP FOUR ARTISTS

emotionalQuadrant <- ggplot(data = topFourArtists, aes(x = valence, y = energy, color = artist_name)) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Angry / Turbulent") +
  annotate('text', 1.75 / 2, 0.95, label = "Joyful / Happy") +
  annotate('text', 1.75 / 2, 0.05, label = "Peace / Chill") +
  annotate('text', 0.25 / 2, 0.05, label = "Depressing / Sad") +
  labs(x= "Valence", y= "Energy") +
  ggtitle("Emotional quadrant Top four artists", "Based on energy y valence")  
emotionalQuadrant
ggplotly()

# DANCEABILITY OF MY TOP ARTISTS

font_add_google(name = "Orbitron",
                family = "orbitron")

showtext::showtext_auto()

top_ten_df %>% 
  ggplot(aes(danceability, color = danceability, fill = danceability)) +
  geom_dots() +
  scale_color_viridis_c(option = "plasma") +
  scale_fill_viridis_c(option = "plasma") +
  theme_dark() +
  labs(x = "Danceability",
       y = "",
       title = "My Top Artists' Tracks Have A Wide Range of Danceability",
       subtitle = ""
  ) +
  theme(plot.background = element_rect(fill = "black"),
        legend.position = "none",
        text = element_text(family = "orbitron", color = "white", size = 11),
        panel.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.text.x = element_text(angle = 25),
        axis.text.y = element_blank(),
        plot.title.position = "plot") 

# DANCEABILITY SEPARATELY OF EACH ARTIST

top_ten_df %>% 
  ggplot(aes(danceability, color = danceability, fill = danceability)) +
  geom_dots() +
  scale_color_viridis_c(option = "plasma") +
  scale_fill_viridis_c(option = "plasma") +
  theme_dark() +
  labs(x = "",
       y = "",
       title = "But Some of My Top Artists Seem More Danceable Than Others",
       subtitle = ""
  ) +
  theme(plot.background = element_rect(fill = "black"),
        legend.position = "none",
        text = element_text(family = "orbitron", color = "white", size = 10),
        panel.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title.position = "plot") +
  facet_grid(.~artist_name, scales = "free")


# TRADITIONAL SCATTER PLOT OF DANCEABILITY

top_ten_df %>% 
  ggplot(aes(danceability, instrumentalness, color = danceability)) +
  scale_color_viridis_c(option = "plasma") +
  geom_jitter() +
  geom_smooth(method = "lm", formula = "y~x") +
  theme_dark() +
  labs(x = "Danceability",
       y = "Instrumentalness",
       title = "This Plot Isn't Helping Us Understand the Association\nBetween Danceability & Instrumentalness Much",
       subtitle = "Instrumentalness is Skewed With Lots of Values Near 0"
  ) +
  theme(plot.background = element_rect(fill = "black"),
        legend.position = "none",
        text = element_text(family = "orbitron", color = "white", size = 13),
        panel.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.text.x = element_text(angle = 25),
        axis.text.y = element_text(hjust = 1.5),
        plot.title.position = "plot")

# DOT PLOT

top_ten_df %>% 
  ggplot(aes(danceability, color = instrumentalness, fill = instrumentalness)) +
  geom_dots() +
  scale_color_viridis_c(option = "plasma", guide = FALSE) +
  scale_fill_viridis_c(option = "plasma") +
  theme_dark() +
  labs(x = "Danceability",
       y = "",
       title = "We See High and Low Levels of Instrumentalness\nThroughout Danceability Distribution",
       fill = "Instrumentalness"
  ) +
  theme(plot.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        text = element_text(family = "orbitron", color = "white", size = 13),
        panel.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.text.x = element_text(angle = 25),
        axis.text.y = element_blank(),
        plot.title.position = "plot") 




