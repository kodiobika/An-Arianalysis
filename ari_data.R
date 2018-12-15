# Imports necessary libraries

library(rvest)
library(stringr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(formattable)
library(plotly)
library(tidytext)
library(rapportools)
library(snakecase)

# Uses an HTML parser to read in tracklist from Ariana's albums 
# from Genius. Only included the first {tracks} tracks and 
# accounted for an {intro} in order to omit remixes and intros
# and only keep the real songs.

get_tracklist <- function(album, tracks, intro = 0) {
  cleaned_name <- album %>% 
    to_sentence_case() %>% 
    str_replace_all(" ", "-")
  
  tracklist <- read_html(paste("https://genius.com/albums/Ariana-grande/", cleaned_name, sep = "")) %>%
    html_nodes(".chart_row-content-title") %>% 
    html_text %>% 
    str_remove_all("\n") %>% 
    str_remove_all("Lyrics") %>% 
    str_remove_all("  ") %>% 
    head(tracks) %>% 
    tail(tracks-intro)
  
  tracklist <- trimws(gsub("\\((.*?)\\)", "", tracklist))
  trimws(gsub(" by(.*)", "", tracklist)) %>% 
    str_remove_all("\\u200b") %>% 
    tolower()
}

# Uses an HTML parser to read in lyrics from each
# song from Genius. Removed everything that is not
# an alphanumeric in order to make the analysis of 
# the actual text easier later on.

get_lyrics <- function(song) {
  cleaned_name <- song %>% 
    str_replace_all(" ", "-") %>% 
    str_replace_all("\\'", "") %>% 
    str_replace_all("[.]", "") %>% 
    str_replace_all(",", "") %>% 
    str_replace_all("/-", "")
  link <- paste("https://genius.com/Ariana-grande-", cleaned_name, "-lyrics", sep="")
  link <- ifelse(str_detect(link, "popular"), "https://genius.com/Mika-popular-song-lyrics", link)
  link <- ifelse(str_detect(link, "bang"), "https://genius.com/Jessie-j-ariana-grande-and-nicki-minaj-bang-bang-lyrics", link)
  raw_lyrics <- read_html(link) %>%
    html_nodes("div.lyrics") %>% 
    html_text
  
  lyrics <- trimws(gsub("([a-z])([A-Z])", "\\1 \\2", raw_lyrics)) %>% 
    str_replace_all("\n", "  ") %>% 
    str_replace_all("  ", " ") %>% 
    tolower() %>% 
    str_remove_all("\"")
  
  lyrics <- gsub("\\[(.*?)\\]", " ", lyrics) %>% 
    str_replace_all(",", "")
  
  trimws(gsub("[^a-zA-Z0-9' ]", "", lyrics))
}

# Uses an HTML parser to read in the titles of
# Ariana's songs that hit the Billboard Hot 100
# chart. Takes a section as input because that's
# how the Billboard website divides the rankings.

get_chart_title <- function(section) {
  read_html(paste("https://www.billboard.com/music/ariana-grande/chart-history/hot-100/", section, sep="")) %>% 
    html_nodes(".artist-section--chart-history__title-list__title__text--title") %>% 
    html_text %>% 
    str_remove_all("\n")
}

# Uses an HTML parser to read in the level that
# each charted song peaked at on the Billboard Hot
# 100. Takes a section as input for the same reason
# as above.

get_chart_peak <- function(section) {
  read_html(paste("https://www.billboard.com/music/ariana-grande/chart-history/hot-100/", section, sep="")) %>% 
    html_nodes(".artist-section--chart-history__title-list__title__text--peak-rank") %>% 
    html_text %>% 
    str_remove_all("(.*)#") %>% 
    str_remove_all(" on(.*)") %>% 
    str_remove_all("\n") %>% 
    trimws()
}

# Construct array of album titles for use later

albums <- c("Yours Truly", "My Everything", "Dangerous Woman", "Sweetener", "Single")

# Get track list for each album

yours_truly <- get_tracklist(albums[1], 12)
my_everything <- get_tracklist(albums[2], 15, 1)
dangerous_woman <- get_tracklist(albums[3], 16)
sweetener <- get_tracklist(albums[4], 15, 1)

# construct array of songs for use later

song <- c(yours_truly, my_everything, dangerous_woman, sweetener, "thank u, next")

# Construct data frame of charted songs using helper functions

charted_songs <- unlist(sapply(1:4, get_chart_title)) %>% 
  tolower() %>% 
  data_frame(unlist(sapply(1:4, get_chart_peak))) %>%
  filter(. %in% tolower(song))
colnames(charted_songs) <- c("song", "peak")

# Constructs data frame with albums, songs, chart level,
# charted/uncharted, lyrics, etc. Wasn't really sure what
# I will need later on so just including as many factors
# as I can just in case.

ari_df <- data.frame(song) %>% 
  mutate(album = case_when(
    song %in% yours_truly ~ "yours truly",
    song %in% my_everything ~ "my everything",
    song %in% dangerous_woman & str_detect(song, "focus") == FALSE ~ "dangerous woman",
    song %in% sweetener ~ "sweetener",
    TRUE ~ "single"
  )) %>% 
  mutate(lyrics = sapply(song, get_lyrics)) %>%
  full_join(charted_songs, by = "song") %>% 
  mutate(peak = strtoi(peak)) %>% 
  mutate(charted = case_when(
    !is.na(peak)  ~ "Charted",
    TRUE ~ "Uncharted"
  )) %>% 
  mutate(billboard = case_when(
    peak %in% 1:10 ~ "Top 10",
    peak %in% 11:100 ~ "Top 100",
    TRUE ~ "Uncharted"
  )) %>% 
  mutate(song = tocamel(song, delim = " ", upper = TRUE, sep = " "), 
         album = tocamel(album, delim = " ", upper = TRUE, sep = " "),
         album_num = case_when(
           album == "Yours Truly" ~ 1,
           album == "My Everything" ~ 2, 
           album == "Dangerous Woman" ~ 3,
           album == "Sweetener" ~ 4,
           TRUE ~ 0),
         album = fct_relevel(album, albums))

# Write to RDS file inside Shiny app directory to use for server

write_rds(ari_df, "ari/ari_df")

# Start with data frame, unnest lyrics
# into indivdual words, find number of
# words for each song and arrange in 
# descending order to get table of songs
# with number of words and write to RDS

numwords <- ari_df %>%
  unnest_tokens(word, lyrics) %>% 
  group_by(song, billboard, album) %>% 
  mutate(num_words = n(song)) %>% 
  arrange(desc(num_words)) %>% 
  ungroup(num_words, song)

write_rds(numwords, "ari/numwords")

# Find lexical diversity by unnesting,
# removing stop words, grouping by song/album,
# and finding the number of distinct words
# for each grouping and then write to RDS

lexical_diversity <- ari_df %>% 
  unnest_tokens(word, lyrics) %>% 
  anti_join(stop_words) %>% 
  group_by(song, album) %>% 
  mutate(word_count = n_distinct(word)) %>% 
  ungroup() %>% 
  select(-peak, -word) %>% 
  unique() %>% 
  arrange(desc(word_count))

write_rds(lexical_diversity, "ari/lexical_diversity")

# Unnest data frame to find all words,
# remove stop words, then find unique
# words in order to get most frequent
# words used, and write to RDS

words <- ari_df %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  unique()

write_rds(words, "ari/words")

# Find important words by unnesting,
# finding unique words, filtering out
# stop/short words, and then applying
# bind_tf_idf and arrange in descending order
# (then filter out some meaningless words I noticed)
# and allow user to select slice that gets selected

important_words <- ari_df %>%
  unnest_tokens(word, lyrics) %>% 
  unique() %>%
  filter(nchar(word) > 3) %>% 
  anti_join(stop_words) %>% 
  count(album, word, sort = TRUE) %>% 
  bind_tf_idf(word, album, n) %>% 
  arrange(desc(tf_idf)) %>% 
  filter(str_detect(word, "it") == FALSE) %>% 
  filter(str_detect(word, "you") == FALSE) %>% 
  filter(str_detect(word, "can") == FALSE) %>% 
  filter(str_detect(word, "ooh") == FALSE) %>%
  filter(str_detect(word, "dont") == FALSE) %>% 
  filter(str_detect(word, "each") == FALSE) %>% 
  group_by(album) 

write_rds(important_words, "ari/important_words")

# Reconstruct data frame without
# stop words and with sentiments
# attached to each word to get 
# starting code for positivity and 
# write to RDS

positivity <- ari_df %>%
  unnest_tokens(word, lyrics) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing"))

write_rds(positivity, "ari/positivity")