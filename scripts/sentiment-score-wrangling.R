# ------------------------------------------------------------------------------
# Wrangling for Sentiment Score Analysis
# Ben Snyderman
# ------------------------------------------------------------------------------

### load packages
library(tidyverse)
library(rvest)
library(stringr)
library(robotstxt)
library(tidytext)
library(dplyr)
library(httr)
library(jsonlite)
library(stopwords)

# check if it's okay to scrape
paths_allowed("https://genius.com/")

#need to set working directory to entire folder
spotify_songs <- readRDS("raw-data/tt_spotify_songs.Rds")


# get rid of duplicate songs
spotify_distinct <- spotify_songs|>
  distinct(track_name, .keep_all = TRUE)

# Popular Songs
songs_top100 <- spotify_distinct |>
  filter(track_popularity >= 87) |>
  select(- track_album_id, - track_album_release_date,
         - playlist_name, -playlist_id, -playlist_id)


# creates the table to contain the tracks after scraping
playlist_lyrics <- tibble(
  track_name = rep(NA_character_, 91),
  track_id = rep(NA_character_, 91),
  lyrics = rep(NA_character_, 91)
)



##To help with parenthesis problem and specific Sunflower song
songs_top100 <- songs_top100 |>
  mutate(track_name = str_replace(track_name, "\\(.*?\\)", ""))

songs_top100 <- songs_top100 |>
  mutate(track_name = str_replace(track_name, "Sunflower - Spider-Man: Into the Spider-Verse", "Sunflower"))


# Scraping Genius to obtain the lyrics for each song
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

##used ChatGPT prompt to help get (?s)^.*?\\bLyrics\\b\\s*\\n+ within the str_replace method
playlist_lyrics_2 <- playlist_lyrics |>
  mutate(lyrics = str_replace(lyrics, "(?s)^.*?\\bLyrics\\b\\s*\\n+", ""))

# gets rid of everything before Read More
playlist_lyrics_3 <- playlist_lyrics_2 |>
  mutate(lyrics = str_replace(lyrics, "(?s)^.*?\\bRead More\\b\\s*\\n+", ""))

# gets rid of everything in brackets (ie [chorus])
playlist_lyrics_4 <- playlist_lyrics_3 |>
  mutate(lyrics = str_remove_all(lyrics, "\\[.*?\\]"))

# puts the lyrics into word by word
lyrics_words <- playlist_lyrics_4 |>
  unnest_tokens(output = word, input = lyrics)

# creates a set of my own stop words to be filtered out later
my_stop_words <- tibble(
  word = c("contributors", "Translations", "uh", "ooh", "ayy", "ah", "woo", "eh", "yeh", "em", "hmm", "mmh", "yeah")
)




#filters stop words
lyrics_no_stop_words <- lyrics_words |>
  anti_join(stop_words, by = "word") |>
  anti_join(my_stop_words, by = "word")

#gets the stop words in Spanish and gets rid of them
stop_words_es <- stopwords(language = "es")

stop_words_es <- tibble(
  word = stop_words_es)

lyrics_no_stop_words <- lyrics_no_stop_words |>
  anti_join(stop_words_es, by = "word")


# gives the counts of each word
word_counts <- lyrics_no_stop_words |>
  count(word) |>
  mutate(frequency = n / sum(n))


afinn_lexicon <- get_sentiments("afinn")


afinn_lyrics <- lyrics_words |>
  inner_join(afinn_lexicon, by = "word")


#gives the afinn scores
afinn_song_scores <- afinn_lyrics |>
  group_by(track_name, track_id) |>
  summarize(sentiment_score = sum(value))



# sentiments for each word
nrc_lexicon <- get_sentiments("nrc")

playlist_nrc <- word_counts |>
  inner_join(nrc_lexicon, by = "word") |>
  filter(sentiment %in% c("anger", "anticipation", "fear"
                          , "joy", "surprise", "trust"))  |>
  arrange(sentiment, desc(n)) |>
group_by(sentiment) |>
  slice(1:10)

#filters words
playlist_nrc_bleeped <- playlist_nrc |>
  mutate(word = str_replace(word, "bitch", "b****")) |>
  mutate(word = str_replace(word, "nigga", "n****")) |>
  mutate(word = str_replace(word, "shit", "s***")) |>
  mutate(word = str_replace(word, "fuck", "f***")) |>
  mutate(word = str_replace(word, "niggas", "n****s")) |>
  mutate(word = str_replace(word, "fuckin", "f*****"))


#filters words
word_counts_bleeped <- word_counts |>
  mutate(word = str_replace(word, "bitch", "b****")) |>
  mutate(word = str_replace(word, "nigga", "n****")) |>
  mutate(word = str_replace(word, "shit", "s***")) |>
  mutate(word = str_replace(word, "fuck", "f***")) |>
  mutate(word = str_replace(word, "niggas", "n****s")) |>
  mutate(word = str_replace(word, "fuckin", "f*****"))
  

#filters words
word_counts_no_swears <- word_counts |>
  mutate(word = str_remove(word, "bitch")) |>
  mutate(word = str_remove(word, "nigga")) |>
  mutate(word = str_remove(word, "shit")) |>
  mutate(word = str_remove(word, "fuck")) |>
  mutate(word = str_remove(word, "niggas")) |>
  mutate(word = str_remove(word, "fuckin"))


# to be used to put the genre in the afinn score dataset
songs_top100_genre <- songs_top100 |>
  select(track_id, playlist_genre)

playlist_lyrics_4 <- playlist_lyrics_4 |>
  left_join(songs_top100_genre, by = "track_id")

afinn_song_scores <- afinn_song_scores |>
  left_join(songs_top100_genre, by = "track_id") |>
  mutate(playlist_genre = str_replace(playlist_genre, "pop", "Pop")) |>
  mutate(playlist_genre = str_replace(playlist_genre, "rap", "Rap"))

# only the top 10 songs
afinn_best <- afinn_song_scores |>
  arrange(desc(sentiment_score)) |>
  ungroup() |>
  slice(1:10)
# only the bottom 10 songs
afinn_worst <- afinn_song_scores |>
  arrange((sentiment_score)) |>
  ungroup() |>
  slice(1:10)


#Only rap and pop songs and gives top and bottom 10
afinn_rap_pop_good <- afinn_song_scores |>
  filter(playlist_genre %in% c("Pop", "Rap")) |>
  group_by(playlist_genre) |>
  arrange(desc(sentiment_score)) |>
  slice(1:10)

afinn_rap_pop_bad <- afinn_song_scores |>
  filter(playlist_genre %in% c("Pop", "Rap")) |>
  group_by(playlist_genre) |>
  arrange((sentiment_score)) |>
  slice(1:10)

word_counts_bleeped_sliced <- word_counts_bleeped |>
  arrange(desc(n)) |>
  slice(1:10)



##Save Final
save(playlist_nrc_bleeped, afinn_best, afinn_worst, afinn_rap_pop_good, afinn_rap_pop_bad, word_counts_bleeped_sliced, word_counts_bleeped, file = "data/sentiment-data.Rds")

