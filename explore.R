library(tidyverse)
library(ggplot2)
library(lubridate)

setwd('/Users/jakeevans/repos/byu/is555/group_project/spotify-data')

raw <- read_csv('19_train.csv')

# Notes on some of the columns (Taken from Spotify Documentation - https://developer.spotify.com/documentation/web-api/reference/get-audio-features)

# 1. Danceability - Describes suitability for dancing based on tempo, rhythm stability, beat strength, and regularity
# 2. Acousticness - Confidence measure of whether track is acoustic (1 represents high confidence that it is acoustic)
# 3. Energy - Features contributing include dynamic range, perceived loudness (LUF), timbre, onset rate, general entropy
# 4. Key - Starts with C = 0, C#/Db = 1, ... B = 1. If no key is detected, value is -1 (https://en.wikipedia.org/wiki/Pitch_class)
# 5. Loudness - Overall loudness of track in decibels (dB). Averaged across track, typically between -60 and 0 db
# 6. Instrumentalness - Predicts whether a track contains no vocals. (Ooh/aah are instrumental). Closer to 1, more likely it's Zimmer
# 7. Mode - Modality of track. Major is 1, Minor is 0... guess the other ones aren't important enough
# 8. Speechiness - Presence of spoken words. 0.666 and above is probably all words. 0.333 and below probably all music.
# 9. Liveness - Detects audience in recording. Higher value indicates that track was probably recorded live
# 10. Valence - Measure that describes the musical positiveness conveyed by a track. Cheerful, happy songs are closer to 1. Low valence (< 0.5) sounds angry, sad


# Exploratory Data Analysis

# Austin: Investigate dependent variable correlations

setwd('./555/spotify-analytics/data/')

#MODE AND KEY ARE CATEGORICAL

sample <- slice_sample(raw, prop = .4)

raw %>% 
  distinct() %>% 
  select(track_popularity, danceability, acousticness, energy, loudness, instrumentalness, speechiness, liveness, valence) %>% 
  pivot_longer(
    cols = c(danceability, acousticness, energy, loudness, instrumentalness, speechiness, liveness, valence),
    names_to = 'metric',
    values_to = 'value'
  ) %>% 
  ggplot(aes(y = track_popularity, x = value, fill = metric, color = metric)) +
  geom_point(alpha = 0.05) +
  facet_wrap(~metric, scales = 'free') + 
  theme_bw()

# Genres

raw %>% 
  distinct() %>%
  select(track_popularity, playlist_genre, playlist_subgenre) %>% 
  pivot_longer(
    cols = c(playlist_genre, playlist_subgenre),
    names_to = 'metric',
    values_to = 'value'
  ) %>% 
  ggplot(aes(y = track_popularity, x = value, fill=value)) +
  geom_violin() +
  facet_wrap(~metric, scales = 'free', ncol = 1) + 
  theme_bw()

#Key and Mode

raw %>% 
  distinct() %>%
  mutate(key = as.character(key), mode = as.character(mode)) %>% 
  select(track_popularity, key, mode) %>% 
  pivot_longer(
    cols = c(key, mode),
    names_to = 'metric',
    values_to = 'value'
  ) %>% 
  ggplot(aes(y = track_popularity, x = value, fill=value)) +
  geom_violin() +
  facet_wrap(~metric, scales = 'free', ncol = 1) + 
  theme_bw()

# Release Date

dates <- raw %>% 
  distinct() %>% 
  select(track_popularity, track_album_release_date) %>% 
  mutate(formatted_date = ymd(track_album_release_date)) %>% 
  mutate(year = if_else(!is.na(formatted_date),year(formatted_date),year(as.numeric(substr(track_album_release_date, 1, 4)))), month = month(formatted_date))



dates %>% 
  group_by(month) %>% 
  summarize(count = n()) %>% 
  print(n=200)

dates %>% 
  filter(is.na(year))

raw %>% 
  group_by(track_album_release_date) %>% 
  summarize() %>% 
  print(n=3000)

# Jake: Examine effects of independent vars on dependent

# Look at shape and column types
raw %>% glimpse()

# We have 5 missing values
missing <- raw %>% 
  subset(!complete.cases(raw)) 
missing

# Find duplicates across all rows
duplicated <- sum(duplicated(raw))
duplicated

# Find duplicates by track_id - looks like songs that appear in more than 1 playlist are duplicated here
filtered_duplicates <- raw %>%
  filter(duplicated(track_id) | duplicated(track_id, fromLast = TRUE)) %>%
  arrange(track_id)
filtered_duplicates

# Basic univariate analysis
summary(raw)

# Unique track count
num_unique_tracks <- raw %>% 
  summarise(unique_tracks = n_distinct(track_id))
num_unique_tracks

# Genre count
genre_count <- raw %>%
  count(playlist_genre) %>% arrange(desc(n))
genre_count

# Sub-genre count
subgenre_count <- raw %>% 
  count(playlist_subgenre)
subgenre_count %>% arrange(desc(n)) %>% print(n=30) 

# Top 10 artists with song counts
top_ten_artists <- raw %>% 
  select(track_artist, track_id)
top_ten_artists %>% group_by(track_artist) %>% summarise(num_tracks = n_distinct(track_id)) %>% arrange(desc(num_tracks)) %>% head(10)

# Find invalid values for range attributes
rows_outside_range <- raw %>%
  filter(valence > 1 | valence < 0,
         liveness > 1 | liveness < 0,
         speechiness > 1 | speechiness < 0,
         loudness > 1 | loudness < 0,
         acousticness > 1 | acousticness < 0,
         instrumentalness > 1 | instrumentalness < 0)
rows_outside_range



##### Data Cleanup and Standardization

library(lubridate)

raw

# Remove Null values - should result in 24,619 values
data_no_null <- raw %>% 
  filter(complete.cases(.))
data_no_null

missing <- data_no_null %>% 
  subset(!complete.cases(data_no_null)) 
missing

# convert track_album_release_date to date
date_fixed <- data_no_null %>% 
  mutate(track_album_release_date = as.Date(track_album_release_date))
date_fixed

clean_data <- date_fixed


### TODO: Fix duplicates



### Data Visualizations


clean_data %>% 
  ggplot(aes(x=track_popularity)) +
  geom_histogram(fill="skyblue")
  

