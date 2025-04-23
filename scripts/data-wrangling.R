# ------------------------------------------------------------------------------
# Data Wrangling
# STAT 231 Blog Project
# Gloria Wu, Justyce Williams, Ben Snyderman
# ------------------------------------------------------------------------------
  
knitr::opts_chunk$set(eval = FALSE, message = FALSE)

# improve digit and NA display 
options(scipen = 1, knitr.kable.NA = '')

# load packages
library(tidyverse)
library(rvest)
library(ggplot2)
library(stringr)
library(purrr)
library(robotstxt)
library(tidytext)
library(tidyr)
library(dplyr)
library(spotifyr)
library(tidytuesdayR)
library(geniusr)
library(httr)
library(jsonlite)
library(broom)
library(GGally)
library(rgenius)

# ------------------------------------------------------------------------------

### load and basic wrangling
## check if we can scrape

# genius
paths_allowed("https://genius.com")

# spotify
paths_allowed("https://spotify.com")

# Individual Song Lyrics Website Allows us to Scrape
paths_allowed("https://open.spotify.com/track/00Cwaioho8d1pETtPFO79T")

# Spotify DataSet 
spotify_songs <- readRDS("data/tt_spotify_songs.Rds")

# Wikipedia 
paths_allowed("https://en.wikipedia.org/wiki/Post_Malone") # Post Malone 
# Just to check if Wikipedia lets us scrape their website 

# All Songs - Full Set for Cluster 
# get rid of duplicate songs
spotify_distinct <- spotify_songs|>
  distinct(track_name, .keep_all = TRUE)

# Popular Songs
songs_top100 <- spotify_distinct |>
  filter(track_popularity >= 87) |>
  select(- track_album_id, - track_album_release_date,
         - playlist_name, -playlist_id, -playlist_id) |>
  group_by(track_id)

# ------------------------------------------------------------------------------

### scrape lyrics from genius.com
#Genius Lyric Data

Sys.getenv("ADf5awyFcKidqW0da12mAzZsO8U_PQ6XDrF3ecWrhbqJzrQM1-lM9fysJFWk_K79")

# Will need to figure out how to make this a full data frame eventually 

# Helpful Website: https://ewenme.github.io/geniusr/

#lyrics
lyrics <- get_lyrics_search(artist_name = "Kanye West",
                            song_title = "Good Morning") 

# ------------------------------------------------------------------------------      

### wrangling for k-means analysis
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
      elbow_plot <- tibble(k = 1:10) |>
        mutate(
          kmeans_results = purrr::map(k, ~kmeans(spotify_kmeans_standardized,
                                                      .x, nstart = 20)),
               glanced = purrr::map(kmeans_results, glance)) |>
          unnest(cols = c(glanced))
           
          elbow_plot |>
            ggplot(aes(x = k, y = tot.withinss)) +
            geom_point() +
            geom_line() +
            scale_x_continuous(breaks = 1:10) +
            labs(x = "Number of clusters (k)",
                y = "Total within-cluster sum of squares")
           
  # k-means analysis (4 clusters)
    set.seed(777)
           
    spotify_kmeans_4 <- spotify_kmeans_standardized |>
      kmeans(centers = 4, nstart = 20)
           
    spotify_k3 <- augment(spotify_kmeans_4, spotify_kmeans_standardized)
    
# ------------------------------------------------------------------------------
    
### wrangling for Sentiment Analysis
           
  ##Still working on removing filler at the top
      playlist_lyrics <- tibble(
        track_name = rep(NA_character_, 91),
        track_id = rep(NA_character_, 91),
        lyrics = rep(NA_character_, 91)
        )
           
           
           
           for (i in 1:91) {
             song_title <- songs_top100$track_name[i]
             song_id <- songs_top100$track_id[i]
             
             search_song <- GET("https://api.genius.com/search",
                                query = list(q = song_title),
                                add_headers(Authorization = paste("Bearer", "OQQUZXvKYhXxNvw9_x7RZgVypJWWwUQvSGmJMD77mTonSK75psaEQL48qYbIpfaj")))
             song_info <- content(search_song, as = "parsed", type = "application/json")
             
             
             song_url <- song_info$response$hits[[1]]$result$url
             
             lyrics <- 
               read_html(song_url) |>
               html_elements("div[data-lyrics-container='true']") |>
               html_text2() |>
               paste(collapse = "\n")
             
             
             
             playlist_lyrics$track_name[i] <- song_title
             playlist_lyrics$track_id[i] <- song_id
             playlist_lyrics$lyrics[i] <- lyrics
             
           }
           
           
           lyrics_words <- playlist_lyrics |>
             unnest_tokens(output = word, input = lyrics)
           
           afinn_lexicon <- get_sentiments("afinn")
           
           
           afinn_lyrics <- lyrics_words |>
             inner_join(afinn_lexicon, by = "word")
           
           
           
           afinn_song_scores <- afinn_lyrics |>
             group_by(track_name, track_id) |>
             summarize(sentiment_score = sum(value))
           
           
           my_stop_words <- tibble(
             word = c("contributors", "Translations")
           )
           
           lyrics_no_stop_words <- lyrics_words |>
             anti_join(stop_words, by = "word") |>
             anti_join(my_stop_words, by = "word")
           
           
           word_counts <- lyrics_no_stop_words |>
             count(word)
           
           
           songs_top100_genre <- songs_top100 |>
             select(track_id, playlist_genre)
           
           playlist_lyrics <- playlist_lyrics |>
             left_join(songs_top100_genre, by = "track_id")

# ------------------------------------------------------------------------------
           
### Wrangle for Network 
           
           #Urls Iteration
           Urls<- songs_top100|>
             mutate( 
               urlartist=  gsub(" ", "_", track_artist), #creates a space for the url to go first, and the separation of the artist names with a underscoe instead of a space because that is how Wikipedia works 
               url= paste0("https://en.wikipedia.org/wiki/", urlartist) #creates the base website URL 
             )
           
           #Reading the Website w/ HTML
           wikihtml <- Urls|>
             select(url)|>
             read_html(url)
           
           #Pulling by Paragraphs
           paragraphs <-Urls|>
             html_nodes("p")|>
             html_text()|>
             paste(collapse = " ")
           
           #Fromn Lab to help make sense 
           toc_url |>
             read_html() |> 
             html_elements("div.wst-plainlist> ul>li>a") |>
             html_attr("href")
           
# ------------------------------------------------------------------------------
           
### save final objects