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
# set working directory to blog-jbg
spotify_songs <- readRDS("raw-data/tt_spotify_songs.Rds")

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
           
           #Read in Hometown names from excel created sheet using wikipedia site to keep it consist with the sourced used for the information in the network map
           #Removed unnecessary blank columns 
           ht<- read.csv("raw-data/hometownsheet.csv")
           ht<- ht|>
             select(-X, - X.1)
           
           
           #Merge Excel Data for Hometown Information Required Later on:
           spotifydata<-inner_join(songs_top100, ht,  by = join_by(track_artist))
           
           
           #Edit Names For Wikipedia Urls 
           #Some artists names are too vague, forcing R to quit reading through because it can't find the specfic urls 
           #Some artisit just need changes for their name need chnages 
           songs_top1001<- songs_top100|>
             mutate(track_artist= case_when(
               track_artist == "MEDUZA" ~ "Meduza (producers)",
               track_artist == "KAROL G" ~ "Karol G",
               track_artist == "ROSALÍA" ~ "Rosalía",
               track_artist == "JACKBOYS" ~ "JackBoys",
               track_artist == "Regard" ~ "Regard (DJ)",
               track_artist == "blackbear" ~ "Blackbear (musician)",
               track_artist == "Y2K" ~ "Y2K!",
               track_artist == "Halsey" ~ "Halsey (singer)",
               track_artist == "Sech" ~ "Sech (singer)",
               track_artist == "Trevor Daniel" ~ "Trevor Daniel (singer)",
               track_artist == "Dan + Shay" ~ "Dan %2B Shay",
               track_artist == "Mustard" ~ "Mustard (record producer)",
               TRUE ~ track_artist
             ))
           
           #Urls Iteration for Each Artisit in Comprehensive List
           songs_top100_urls<- songs_top1001 |>
             mutate( 
               urlartist=  gsub(" ", "_", track_artist), #creates a space for the url to go first, and the separation of the artist names with a underscoe instead of a space because that is how Wikipedia works 
               url= paste0("https://en.wikipedia.org/wiki/", urlartist), #creates the base website URL
               text = "",
             )
           
           
           
           #Save and Read Urls Through each link 
           for (i in 1:91) {
             songs_top100_urls$text[i] <- songs_top100_urls$url[i]|>
               read_html()|>
               html_elements("p") |>
               html_text2()|>
               paste(collapse = ".") 
             
           }
           
           i #Used to confirm full iteration happened 
           
           #Making Artist Names Lowercase to befound in the text paragraphs from Wikipedia
           songs_top100_urls<- songs_top100_urls|>
             mutate(track_artist= str_to_lower(track_artist))
           
           #Creating Separate Dataframe with just text for merge 
           songs_text<- songs_top100_urls|>
             select(track_artist, text)|>
             unnest_tokens(output = sentences, input = text, token = "sentences")
           
           #allocation space for new data entries 
           textspacesallocation<- data.frame(track_artist=NA, sentences= NA, collabArtist=NA)
           
           #Creating Vector for the For Loop  
           artistvec<- pull(songs_text, track_artist)|>
             unique()
           
           for (a in artistvec){
             spotify_text_identify<-  songs_text|>
               mutate(collabArtist = str_extract(sentences, a))|>
               filter(!is.na(collabArtist)) #Used to remove NAs and cut down iteration computation time
             textspacesallocation<- bind_rows(spotify_text_identify, textspacesallocation )
           }
           
           #Creating a new column that tells me when the artist's wikipedia site mentions another artist within the data set that R will extract, to display which artist within the data set have worked together 
           textspacesallocation1<- textspacesallocation|>
             mutate( track_artist1 = case_when (
               track_artist == collabArtist ~ "x",
               TRUE ~ track_artist
             ))
           
           #Get rid of artist mentions of themselves and thn removing the column because it does not add any useful information (it just reopeats the artists again), also don't need the wikipedia text anymore once I have the artist they have collabed with
           textspacesallocation1<- textspacesallocation1|>
             filter(!track_artist1== "x")
           #calling it network to save as the offical data set 
           network<- textspacesallocation1|>
             select( -track_artist1, -sentences)
           
           
           genre<- songs_top1001|>
             select(track_artist, genre)|>
             mutate(track_artist= str_to_lower(track_artist))
           
           networkdata<- network|>
             left_join(genre, by= join_by(track_artist))
           
           
           #Datasets to Move Forward With-- 
           networkdata
           
           saveRDS(networkdata, "data/networkdata.Rds")
           
           
           ##Plan Out The Network Graph:
           #1. Artist Collab (Using Offical_artist_collabset data set)
           # Create the Nodes and Edges for a visble Graph 
           
           #artistcollab_network<- as_tbl_graph(x= Offical_artist_collabset, nodes=track_artist, edges = containtextartist, directed= FALSE)
           #gorder(artistcollab_network) #Number of Nodes 
           #gsize(artistcollab_network) # Number of Edges 
           
# ------------------------------------------------------------------------------
           
### save final objects