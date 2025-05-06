# ------------------------------------------------------------------------------
# wrangling for network graph
# Justyce Williams
# ------------------------------------------------------------------------------

### load packages
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
library(GGally)
library(igraph)
library(ggraph)
library(tidygraph)
library(sf)
library(leaflet)
library(ggspatial)

### load dataset
# set working directory to blog-jbg
spotify_songs <- readRDS("raw-data/tt_spotify_songs.Rds")

# check if it's okay to scrape
paths_allowed("https://en.wikipedia.org/wiki/")

# All Songs - Full Set for Cluster 
# get rid of duplicate songs
spotify_distinct <- spotify_songs|>
  distinct(track_name, .keep_all = TRUE) #For dataframes only

# Popular Songs
songs_top100 <- spotify_distinct |>
  rename(genre= playlist_genre)|>
  filter(track_popularity >= 87) |>
  select(- track_album_id, - track_album_release_date,
         - playlist_name, -playlist_id, -playlist_id) 

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


#Save Dataset
saveRDS(networkdata, "data/networkdata.Rds")




### Wrangle for Table chart 

# Only have information from the original data that I actually need for bar data 
#Summarizing and counting data for bar transfer
tabledata<- songs_top100|>
  select(track_artist)|>
  group_by(track_artist)|>
  summarise(Count = n())

#Get Only Genre Details and Valence scores 
#Must Inlcude Track Names for Clarity in tracking valence scoring 
songs_genreVal<- songs_top100|>
  select(track_artist,track_name, genre,track_popularity)

#creating Dataset to move forward with 
officaltabledata<- tabledata |>
  left_join(songs_genreVal, by = join_by(track_artist))

saveRDS(officaltabledata, "data/officaltabledata.Rds")


###Maps for Hometowns
#Pull in Excel Data using Wikipedia sites tokeep the information consistent 
hometownAdd<- read.csv("raw-data/hometownsheet.csv")

#Remove Unnecessary Columns 
hometownAdd<- hometownAdd|>
  select( -X, -X.1)

#Join Data with Excel Sheet
spotifydata<- hometownAdd|>
  right_join(songs_top100, by= ("track_artist"))


#Only want Artist and locations for this dataset
spotifydataHT <- spotifydata|>
  select(track_artist, hometown)|>
  distinct(track_artist, .keep_all = TRUE)|>
  mutate(hometown = str_replace(hometown, ",.*", ""),
         hometown= str_to_lower(hometown))


# Map Tabs -- Canada, USA, Colombia
#United States
usa_sf<- geodata::gadm("United States", level= 1, path = tempdir(), resolution = 1) |>
  st_as_sf()
usa_sf <- st_simplify(usa_sf, dTolerance = 1000)
usa_sf <- st_transform(usa_sf, 2163)  #USA contiguous Albers Equal Area 

#USA Wrangling
usa<- usa_sf|>
  st_transform(4326)|> # need this for interactive leaflet 
  select(NAME_1, GID_1,geometry)|>
  filter(!NAME_1 =="Alaska" & !NAME_1 == "Hawaii")|>
  rename(hometown = NAME_1)|>
  mutate(hometown= str_to_lower(hometown))


#Collapsing track names into each state for each dataset 
Artist_hometown_collapse <- spotifydataHT|>
  tibble(
    category = hometown,
    text = track_artist)|>
  group_by(category)|>
  summarise(text = str_c(text, collapse = ", "))|>
  rename(hometown = category, #renaming for later dataset join 
         track_artist = text)

#tibble
#https://forum.posit.co/t/combining-multiple-rows-into-one-single-row-in-dataframe-in-r/118856


#Colombia
colombia_sf <- geodata::gadm("Colombia",level= 1, path = tempdir(), resolution = 1)|>
  st_as_sf()
colombia_sf <- st_simplify(colombia_sf, dTolerance = 1000)

colombia<- colombia_sf|>
  st_transform(4326)|> # need this for interactive leaflet 
  select(NAME_1, GID_1,geometry)|>
  rename(hometown = NAME_1)|>
  mutate(hometown= str_to_lower(hometown))


#Canada
canada_sf <- geodata::gadm("Canada", level = 1, path = tempdir(), resolution = 1) |>
  st_as_sf()

canada_sf <- st_simplify(canada_sf, dTolerance = 1000) #The tolerance/ simplify function removes some of the more complicated mapping for speed with loading
canada_sf <- st_transform(canada_sf, 3348) #Canada Lambert Conformal Conic 

canada<- canada_sf|>
  st_transform(4326)|> # need this for interactive leaflet 
  select(NAME_1, GID_1,geometry)|>
  rename(hometown = NAME_1)|>
  mutate(hometown= str_to_lower(hometown))

#combine all the spatial dataframes together 
NorthSouthAmerica<- bind_rows(usa,colombia,canada)

#cobining artist name to their hometowns
NorthSouthAmericaTotal<- NorthSouthAmerica|>
  left_join(Artist_hometown_collapse, by= (join_by("hometown")))|>
  drop_na()

#Adding weight to see how many artist' hometown are in which place on the map 
NorthSouthAmericaTotal<- NorthSouthAmericaTotal|>
  select(-GID_1)|>
  mutate(Weight= str_count(track_artist, ",")+ 1)


#saving data
saveRDS(NorthSouthAmericaTotal, "data/NorthSouthAmericaTotal")
