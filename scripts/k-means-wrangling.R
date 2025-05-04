# ------------------------------------------------------------------------------
# wrangling for k-means analysis
# Gloria Wu
# ------------------------------------------------------------------------------

### load packages
library(tidyverse)
library(purrr)
library(broom)

### load dataset
# set working directory to blog-jbg
spotify_songs <- readRDS("raw-data/tt_spotify_songs.Rds")

# get rid of duplicate songs
spotify_distinct <- spotify_songs|>
  distinct(track_name, .keep_all = TRUE)

### wrangling
# remove any non-numerical variables
spotify_kmeans <- spotify_distinct |>
  select(- track_id, - track_name, - track_artist, - track_album_id,
         - track_album_name, - track_album_release_date, - playlist_name,
         - playlist_id, -playlist_genre, - playlist_subgenre)

# standardize variables
spotify_kmeans_standardized <- spotify_kmeans |>
  mutate(across(where(is.numeric),
                ~ (.x - mean(.x)) / sd(.x),
                .names = "{.col}_z")) |>
  select(ends_with("_z"))

# elbow plot to see optimal amount of clusters
set.seed(777)

elbow_plot <- tibble(k = 1:10) |>
  mutate(
    kmeans_results = purrr::map(k, ~kmeans(spotify_kmeans_standardized,
                                           .x, nstart = 20)),
    glanced = purrr::map(kmeans_results, glance)) |>
  unnest(cols = c(glanced))

# k-means analysis (4 clusters)
set.seed(777)

spotify_kmeans_4 <- spotify_kmeans_standardized |>
  kmeans(centers = 4, nstart = 20)

spotify_k4 <- augment(spotify_kmeans_4, spotify_kmeans_standardized) |>
  rename(Popularity = track_popularity_z,
         Danceability = danceability_z,
         Energy = energy_z,
         Key = key_z,
         Loudness = loudness_z,
         Mode = mode_z,
         Speechiness = speechiness_z,
         Acousticness = acousticness_z,
         Instrumentalness = instrumentalness_z,
         Liveness = liveness_z,
         Valence = valence_z,
         Tempo = tempo_z,
         Duration = duration_ms_z,
         Cluster = .cluster)

### save final objects
save(elbow_plot, spotify_k4, file = "data/k-means-data.Rds")