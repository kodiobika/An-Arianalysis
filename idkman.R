library(rvest)
library(stringr)
library(tidyverse)
library(snakecase)
library(knitr)
library(kableExtra)
library(formattable)
library(tidy)
library(lettercase)
library(rapportools)
library(tools)
library(wordcloud2)
library(gridExtra)
library(plotly)
library(tidytext)

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

get_chart_title <- function(section) {
  read_html(paste("https://www.billboard.com/music/ariana-grande/chart-history/hot-100/", section, sep="")) %>% 
    html_nodes(".artist-section--chart-history__title-list__title__text--title") %>% 
    html_text %>% 
    str_remove_all("\n")
}

get_chart_peak <- function(section) {
  read_html(paste("https://www.billboard.com/music/ariana-grande/chart-history/hot-100/", section)) %>% 
    html_nodes(".artist-section--chart-history__title-list__title__text--peak-rank") %>% 
    html_text %>% 
    str_remove_all("(.*)#") %>% 
    str_remove_all(" on(.*)") %>% 
    str_remove_all("\n") %>% 
    trimws()
}


albums <- c("Yours Truly", "My Everything", "Dangerous Woman", "Sweetener", "Single")

yours_truly <- get_tracklist(albums[1], 12)
my_everything <- get_tracklist(albums[2], 15, 1)
dangerous_woman <- get_tracklist(albums[3], 16)
sweetener <- get_tracklist(albums[4], 15, 1)

song <- c(yours_truly, my_everything, dangerous_woman, sweetener, "thank u, next")

charted_songs <- unlist(sapply(1:4, get_chart_title)) %>% 
  tolower() %>% 
  data_frame(unlist(sapply(1:4, get_chart_peak))) %>%
  filter(. %in% tolower(song))
colnames(charted_songs) <- c("song", "peak")

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
  mutate(top_chart = case_when(
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

write_rds(ari_df, "ari/ari_df")



ari_df %>%
  filter(top_chart == "Top 10" | top_chart == "Top 100") %>% 
  select(song, peak, album) %>% 
  group_by(album, peak) %>% 
  arrange(album, peak) %>%
  ungroup() %>% 
  mutate(song = color_bar("pink")(song),
         album = color_bar("pink")(album),
         peak = color_bar("pink")(peak)) %>%
  kable(format = "html", table.attr = "style=\"color: black; background-color: black;\"", escape = FALSE, align = "c", caption = "Ari's Top 10s", col.names = c("Song", "Peaked at", "Album")) %>%
  kable_styling(bootstrap_options = c("responsive", "hover"), full_width = TRUE, position = "center", font_size = 15)

ari_df %>%
  unnest_tokens(word, lyrics) %>% 
  group_by(song, top_chart) %>% 
  summarize(num_words = n(top_chart)) %>% 
  arrange(desc(num_words)) %>% 
  ungroup(num_words, song) %>% 
  kable(format = "html", table.attr = "style=\"color: pink; font-weight: bold; background-color: black;\"", escape = FALSE, align = "c", col.names = c("Song", "Billboard", "Number of Words")) %>%
  kable_styling(bootstrap_options = c("responsive", "hover"), full_width = TRUE, position = "center", font_size = 20)

filtered <- ari_df %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>% 
  filter(nchar(word) > 3)

filtered %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  plot_ly(x = ~word, y = ~n, type = "bar", color = I("pink")) %>% 
  layout(plot_bgcolor = "black")
