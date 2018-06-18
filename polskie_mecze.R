### TO DO
# ogarnąc przekleństwa - liczba w czasie - buble na osi czasu
# ogarnąć składy - liczba wspomnien w czasie (buble na osi czasu)
# częstotliwość par - osoba + przekleństwo (tile)

# najpopularniejsze twitty (rt_fav)

suppressPackageStartupMessages(library(methods))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(rtweet))


theme_set(theme_minimal() +
            theme(plot.title = element_text(family = NULL, face = "bold", size = 24, color = "black"),
                  plot.subtitle = element_text(family = NULL, face = "plain", size = 12, color = "black"),
                  plot.caption = element_text(family = NULL, face = "italic", size = 14, color = "brown"),
                  plot.background = element_rect(fill="#efefef", color="#aaaaaa"),
                  panel.background = element_rect(fill = "white", color="black"),
                  strip.background.x = element_rect(fill = "gray80"),
                  strip.text.x = element_text(face = "bold", color = "black", size = 14),
                  axis.text = element_text(size = 12),
                  legend.text = element_text(size = 12)))


setwd("~/RProjects/Mundial2018_twitter_stream/twitter_data/")
#setwd("~/RProjects/Mundial2018_twitter_stream/arch/")

rm(list = ls())


# szukamy najnowszego pliku w folderze
tweets <- list.files() %>%
  tibble(file = .) %>%
  mutate(mktime = lapply(file, file.mtime)) %>%
  unnest() %>%
  filter(mktime == max(mktime)) %>%
  pull(file) %>%
  # wczytujemy najnowszy
  readRDS() %>%
  filter(lang == "pl") %>%
  distinct(status_id, .keep_all = TRUE)

setwd("..")
shit_words <- read_csv("dicts/shit_words.csv", col_types = "cc")
player_names <- read_csv("dicts/players.csv", col_types = "cc") %>% mutate(slowo = str_to_lower(slowo))



get_shit_word_tweets <- function(f_shit_word) {

  # zamienic f_shit_word na regexp dajacego tylko cale wyrazy
  f_shit_word_reg <- str_to_lower(f_shit_word)

  tweets %>%
    # bez linków
    mutate(text = gsub("\n", "", text)) %>%
    mutate(text = gsub("?(f|ht)tp(s?)://(.*)[.][a-zA-Z0-9/]+", "", text)) %>%
    mutate(text = tolower(text)) %>%
    filter(str_detect(text, f_shit_word_reg)) %>%
    select(status_id, created_at, text) %>%
    mutate(shit_word = f_shit_word)
}


# tabela wystąpień brzyskich wyrazów
shit_words_df <- shit_words$str %>%
  unique() %>%
  map_df(get_shit_word_tweets) %>%
  mutate(created_at = with_tz(floor_date(created_at, unit = "minutes"),
                              tzone = "Europe/Warsaw")) %>%
  select(status_id, created_at, shit_word, text) %>%
  left_join(shit_words, by = c("shit_word" = "str")) %>%
  select(status_id, created_at, shit_word = slowo, text)

# tabela wystąpień nazwisk
player_names_df <- player_names$slowo %>%
  tolower() %>%
  unique() %>%
  map_df(get_shit_word_tweets) %>%
  mutate(created_at = with_tz(floor_date(created_at, unit = "minutes"),
                              tzone = "Europe/Warsaw")) %>%
  rename(player_name = shit_word) %>%
  select(status_id, created_at, player_name, text) %>%
  left_join(player_names, by = c("player_name" = "slowo")) %>%
  select(status_id, created_at, player_name = zawodnik, text)


# wystąpienia brzydkich słów
shit_words_df %>%
  count(shit_word, created_at) %>%
  ggplot() +
  geom_jitter(aes(created_at, shit_word, size = n),
              color = "red", alpha = 0.7,
              height = 0.1, width = 0,
              show.legend = FALSE)


# to wykresem ggjoy?
player_names_df %>%
  count(player_name, created_at) %>%
  ggplot() +
  geom_jitter(aes(created_at, player_name, size = n),
              color = "red", alpha = 0.7,
              height = 0.1, width = 0,
              show.legend = FALSE)

shit_players <- inner_join(shit_words_df, player_names_df, by = "status_id")

if(nrow(shit_players) != 0) {
  shit_players %>%
    count(shit_word, player_name) %>%
    ggplot() +
    geom_tile(aes(shit_word, player_name, fill = n), color = "gray20") +
    scale_fill_distiller(palette = "YlOrRd", direction = -1)
}
