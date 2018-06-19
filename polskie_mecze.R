### TO DO
# ogarnąc przekleństwa - liczba w czasie - buble na osi czasu
# ogarnąć składy - liczba wspomnien w czasie (buble na osi czasu)
# częstotliwość par - osoba + przekleństwo (tile)

# args[1] - post tweet?
# args[2] - dict (pl / eng)
# args[3] - 1st team iso
# args[4] - 2nd team iso

# polskie_mecze.R 1 pl POL SEG
# najpopularniejsze twitty (rt_fav)

rm(list = ls())

dict <- "eng"

suppressPackageStartupMessages(library(methods))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(rtweet))

Sys.setenv(TWITTER_PAT="/home/lemur/RProjects/Mundial2018_twitter_stream/twitter_token.rdata")

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

plot_shit <- NULL
plot_players <- NULL

player_names <- read_csv("../dicts/players.csv", col_types = "ccc") %>% mutate(slowo = str_to_lower(slowo))
teams <- read_csv("../dicts/teams.csv", col_types = "cc")



# czy skrypt opalony z shella?
if(length(commandArgs()) == 2) {

  teamA_s <- "RUS" # 1st team
  teamB_s <- "EGY" # 2nd team

  post_tweets <- FALSE

} else {

  # skrypt z shela
  options(echo=FALSE)
  args <- commandArgs(trailingOnly = TRUE)

  # jak za mało parametrów to nie ma co się bawić dalej :)
    if(length(args) != 4) {
      cat("You need mode parameters: post dict teamA teamB\n")
      quit()
    }

  teamA_s <- args[3] # 1st team name
  teamB_s <- args[4] # 2nd team name

  post_tweets <- ifelse(as.numeric(args[1]) == 0, FALSE, TRUE)

  dict <- args[2]

}

teamA_f <- as.character(teams[teams$iso == teamA_s, "country"]) # 1st team name
teamB_f <- as.character(teams[teams$iso == teamB_s, "country"]) # 2nd team name

if(dict == "pl") {
  mecz <- paste0("Mecz ", teamA_f, " #", toupper(teamA_s), " - ", teamB_f, " #", toupper(teamB_s), " w hashtagach ")
  mecz_tw <- paste0(teamA_f, " #", toupper(teamA_s), " - ", teamB_f, " #", toupper(teamB_s), " w hashtagach ")
} else {
  mecz <- paste0("Match ", teamA_f, " #", toupper(teamA_s), " - ", teamB_f, " #", toupper(teamB_s), " on hashtags ")
  mecz_tw <- paste0(teamA_f, " #", toupper(teamA_s), " - ", teamB_f, " #", toupper(teamB_s), " on hashtags ")
}

twitter_query <- paste0("#", tolower(teamA_s), tolower(teamB_s))
twitter_query_rev <- paste0("#", tolower(teamB_s), tolower(teamA_s))
twitter_query_tw <- paste0(twitter_query, " & ", twitter_query_rev)

caption_str <- "(c) 2018, Łukasz Prokulski, @rstatspl, fb.com/DaneAnalizy"

if(dict == "pl") {
  shit_words <- read_csv("../dicts/shit_words.csv", col_types = "cc")
} else {
  shit_words <- read_csv("../dicts/shit_words_eng.csv", col_types = "cc")
}

# szukamy najnowszego pliku w folderze
tweets <- list.files() %>%
  tibble(file = .) %>%
  mutate(mktime = lapply(file, file.mtime)) %>%
  unnest() %>%
  filter(mktime == max(mktime)) %>%
  pull(file) %>%
  # wczytujemy najnowszy
  readRDS() %>%
  filter(!is_retweet) %>%
  distinct(status_id, .keep_all = TRUE)


setwd("..")

if(dict == "pl") {
  tweets <- tweets %>% filter(lang == "pl")
}


get_shit_word_tweets <- function(f_shit_word) {

  # zamienic f_shit_word na regexp dajacego tylko cale wyrazy
  f_shit_word_reg <- str_to_lower(paste0(" ", f_shit_word, " "))

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
  select(status_id, created_at, shit_word = slowo, text) %>%
  distinct(status_id, .keep_all = TRUE)


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
  select(status_id, created_at, player_name = zawodnik, text) %>%
  distinct(status_id, .keep_all = TRUE)



# wystąpienia brzydkich słów
plot_shit <- shit_words_df %>%
  count(shit_word, created_at) %>%
  arrange(desc(shit_word)) %>%
  mutate(shit_word = fct_inorder(shit_word)) %>%
  ggplot() +
  geom_jitter(aes(created_at, shit_word, size = n, alpha = n),
              color = "red",
              height = 0.1, width = 0,
              show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.4, 0.7))

if(dict == "pl") {
  plot_shit <- plot_shit + labs(title = "Użycie \"brzydkich wyrazów\"", x = "", y = "",
                                subtitle = paste0(mecz, toupper(twitter_query_tw)),
                                caption = caption_str)
} else {
  plot_shit <- plot_shit + labs(title = "Frequency of swear words", x = "", y = "",
                                subtitle = paste0(mecz, toupper(twitter_query_tw)),
                                caption = caption_str)
}


# to wykresem ggjoy?
plot_players <- player_names_df %>%
  count(player_name, created_at) %>%
  arrange(desc(player_name)) %>%
  mutate(player_name = fct_inorder(player_name)) %>%  ggplot() +
  geom_jitter(aes(created_at, player_name,  size = n, alpha = n),
              color = "red",
              height = 0.1, width = 0,
              show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.4, 0.7))

if(dict == "pl") {
  plot_players <- plot_players + labs(title = "Wymieniane osoby", x = "", y = "",
                                      subtitle = paste0(mecz, toupper(twitter_query_tw)),
                                      caption = caption_str)
}

if(!is.null(plot_shit)) {
  ggsave("pics/plot_shit.png", plot = plot_shit, width = 12, height = 9, units = "in", dpi = 100)
  if(post_tweets) {
    if(dict == "pl") {
      post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": rozkład użycia \"brzydkich wyrazów\"\n#worldcup2018 #worldcup"), media = "pics/plot_shit.png")
    } else {
      post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": frequency of \"swear words\"\n#worldcup2018 #worldcup"), media = "pics/plot_shit.png")
    }
  }
}


# twitujemy tylko dla jezyka polskiego - ten z kolei tylko dla meczy Polaków ma sens
if(!is.null(plot_players)) {
  ggsave("pics/plot_players.png", plot = plot_players, width = 12, height = 9, units = "in", dpi = 100)
  if(post_tweets & dict == "pl") {
    post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": kto był wymieniany?\n#worldcup2018 #worldcup"), media = "pics/plot_players.png")
  }
}


shit_players <- inner_join(shit_words_df, player_names_df, by = "status_id")

if(nrow(shit_players) != 0) {
  plot_shit_players <- shit_players %>%
    count(shit_word, player_name) %>%
    ggplot() +
    geom_tile(aes(shit_word, player_name, fill = n), color = "gray20") +
    geom_text(aes(shit_word, player_name, label = n), color = "gray20") +
    scale_fill_distiller(palette = "YlOrRd", direction = -1)

  # twitujemy tylko dla jezyka polskiego - ten z kolei tylko dla meczy Polaków ma sens
  if(dict == "pl") {
    plot_shit_players <- plot_shit_players + labs(title = "\"Brzydkie wyrazy\" przy osobach użyte w jednym tweecie", x = "", y = "",
                                                  subtitle = paste0(mecz, toupper(twitter_query_tw)),
                                                  caption = caption_str)

    ggsave("pics/plot_shit_players.png", plot = plot_shit_players, width = 12, height = 9, units = "in", dpi = 100)
    if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": \"brzydkie wyrazy\" przy osobach użyte w jednym tweecie\n#worldcup2018 #worldcup"), media = "pics/plot_shit_players.png")
  }
}


if(dict == "pl") {

  person_populatiry <- tweets %>%
    unnest_tokens(word, text) %>%
    left_join(player_names, by = c("word" = "slowo")) %>%
    count(zawodnik, typ, sort = TRUE) %>%
    filter(zawodnik %in% player_names$zawodnik) %>%
    mutate(zawodnik = fct_inorder(str_to_title(zawodnik))) %>%
    ggplot() +
    geom_col(aes(zawodnik, n, fill = typ), color = "gray20",
             show.legend = FALSE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
    labs(title = "Popularność osób wymienionych w twittach", x = "", y = "",
         subtitle = paste0(mecz, toupper(twitter_query_tw)),
         caption = caption_str)

  ggsave("pics/person_populatiry.png", plot = person_populatiry, width = 12, height = 9, units = "in", dpi = 100)
  if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": popularność osób wymienionych w twittach\n#worldcup2018 #worldcup"), media = "pics/person_populatiry.png")

}
