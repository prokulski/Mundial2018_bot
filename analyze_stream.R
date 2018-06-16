# paramety z shela
# args[1] # 1st team name
# args[2] # 2nd team name
# args[3] # 1st team
# args[4] # 2nd team
# args[5] # post tweet? 0/1

library(methods)
library(tidyverse)
library(lubridate)
library(tidytext)
library(ggthemes)
library(wordcloud)
library(fs)
library(rtweet)
library(ggrepel)
library(jsonlite)

setwd("~/RProjects/Mundial2018_twitter_stream/twitter_data/")
rm(list = ls())


rtag1 <- ifelse(rnorm(1) > 2, " #rstats used!", "")
rtag2 <- ifelse(rnorm(1) > 2, "\n#rstats magic :)", "")
rtag3 <- ifelse(rnorm(1) > 2, "\nMade with #rstats <3", "")

momenty_show <- FALSE

# czy skrypt opalony z shella?
if(length(commandArgs()) == 2) {

  teamA_f <- "Croatia" # 1st team name
  teamB_f <- "Nigeria" # 2nd team name
  teamA_s <- "CRO" # 1st team
  teamB_s <- "NGA" # 2nd team

  post_tweets <- FALSE

} else {

  # skrypt z shela
  options(echo=FALSE)
  args <- commandArgs(trailingOnly = TRUE)

  teamA_f <- args[1] # 1st team name
  teamB_f <- args[2] # 2nd team name
  teamA_s <- args[3] # 1st team
  teamB_s <- args[4] # 2nd team

  post_tweets <- ifelse(as.numeric(args[5]) == 0, FALSE, TRUE)
}


# mecz <- "Match Portugal #POR - Spain #ESP on hashtag "
mecz <- paste0("Match ", teamA_f, " #", toupper(teamA_s), " - ", teamB_f, " #", toupper(teamB_s), " on hashtags ")

#mecz_tw <- "Portugal #POR - Spain #ESP on hashtags "
mecz_tw <- paste0(teamA_f, " #", toupper(teamA_s), " - ", teamB_f, " #", toupper(teamB_s), " on hashtags ")

#twitter_query <- "#poresp"
twitter_query <- paste0("#", tolower(teamA_s), tolower(teamB_s))

#twitter_query_rev <- "#esppor"
twitter_query_rev <- paste0("#", tolower(teamB_s), tolower(teamA_s))

#twitter_query_tw <- paste0(twitter_query, " & ", twitter_query_rev)
twitter_query_tw <- paste0(twitter_query, " & ", twitter_query_rev)

cat(paste0("mecz = '", mecz, "'\n"))
cat(paste0("mecz_tw = '", mecz_tw, "'\n"))
cat(paste0("twitter_query = '", twitter_query, "'\n"))
cat(paste0("twitter_query_rev = '", twitter_query_rev, "'\n"))
cat(paste0("twitter_query_tw = '", twitter_query_tw, "'\n"))
cat(paste0("post tweet? = '", post_tweets, "'\n"))

teamA_f_split <- teamA_f %>% str_split(" ") %>% unlist()
teamB_f_split <- teamB_f %>% str_split(" ") %>% unlist()

stop_tags <- unique(tolower(c(teamA_f, teamB_f, teamA_s, teamB_s,
                       teamA_f_split, teamB_f_split,
                       twitter_query, twitter_query_rev)))




# wynik, jaki wynik?
fixtures_url <- "http://api.football-data.org/v1/competitions/467/fixtures"
fixtures_df <- fromJSON(fixtures_url, flatten = TRUE)$fixtures
match_df <- fixtures_df %>% filter(homeTeamName == teamA_f, awayTeamName == teamB_f)
match_result_df <- fromJSON(match_df$`_links.self.href`, flatten = TRUE)$fixture
match_result_str <- case_when(
  match_df$status == "IN_PLAY" ~ paste0(match_result_df$result$goalsHomeTeam, ":", match_result_df$result$goalsAwayTeam, " (in play)"),
  match_df$status == "FINISHED" ~ paste0(match_result_df$result$goalsHomeTeam, ":", match_result_df$result$goalsAwayTeam, " (finished)"),
  match_df$status == "TIMED" ~ "")



dedicated_stop_words <- c("https", "t.co", "manofthematch", "budweiser",
                          "match", "game", "team",
                          "worldcuprussia2018", "worldcup2018", "worldcup", "worldcup18",
                          "fifaworldcup", "fifaworldcup2018",
                          "rusia2018", "russia2018",
                          "copa2018", "cm2018",
                          "wm2018", "mundial", "mundial2018", "mundial18",
                          gsub("#", "", stop_tags))


spam_strings <- c("Tap below to vote now", "You can vote for your",
                  "Absolute world class performance")

momenty <- tribble(~time, ~co,
                   "2018-06-15 20:00", "Start",
                   "2018-06-15 20:45", "End of 1st half",
                   "2018-06-15 21:02", "End of break",
                   "2018-06-15 21:52", "End of 2nd half"
) %>%
  mutate(time = ymd_hm(time, tz = "Europe/Warsaw"))


caption_str <- "(c) 2018, Łukasz Prokulski, @rstatspl, fb.com/DaneAnalizy"

Sys.setenv(TWITTER_PAT="/home/lemur/RProjects/Mundial2018_twitter_stream/twitter_token.rdata")

p1 <- NULL
p2 <- NULL
p3 <- NULL
p4 <- NULL
p5 <- NULL
p7 <- NULL
p7b <- NULL
p8 <- NULL
p9 <- NULL

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

# szukamy najnowszego pliku w folderze
tweets <- list.files() %>%
  tibble(file = .) %>%
  mutate(mktime = lapply(file, file.mtime)) %>%
  unnest() %>%
  filter(mktime == max(mktime)) %>%
  pull(file) %>%
  # wczytujemy najnowszy
  readRDS() %>%
  filter(!grepl(spam_strings[[1]], text)) %>%
  filter(!grepl(spam_strings[[2]], text)) %>%
  filter(!grepl(spam_strings[[3]], text)) %>%
  distinct(status_id, .keep_all = TRUE)


# sprzatamy folder na obrazki
setwd("..")
dir_delete("pics")
dir_create("pics")


# ile tweetów na minute
p1 <- tweets %>%
  mutate(created_at = floor_date(with_tz(created_at, "Europe/Warsaw"), unit = "minutes")) %>%
  # bez pierwszej i ostatniej minuty - mogą być niepełne
  filter(created_at < floor_date(max(created_at), unit = "minutes")) %>%
  filter(created_at > floor_date(min(created_at), unit = "minutes")) %>%
  count(created_at, is_retweet) %>%
  ggplot() +
  geom_area(aes(created_at, n, fill = is_retweet), color = "gray10") +
  scale_fill_manual(labels = c("no", "yes"), values = c("lightblue", "lightgreen")) +
  theme(legend.position = "bottom") +
  labs(x = "", y = "", fill = "RT?",
       title = "Number of tweets",
       subtitle = paste0(mecz, toupper(twitter_query_tw)),
       caption = caption_str)

if(momenty_show) {
  if(Sys.time() > min(momenty$time)) {
    p1 <- p1 + geom_vline(data = momenty %>% filter(time < Sys.time()),
                          aes(xintercept = time), color = "blue") +
      geom_label_repel(data = momenty %>% filter(time < Sys.time()),
                       aes(x = time, y = -50, label = co))
  }
}

top_langs <- tweets %>%
  count(lang) %>%
  arrange(desc(n))

p5 <- top_langs %>%
  mutate(p = 100*n/sum(n)) %>%
  filter(n > 1) %>%
  top_n(10, n) %>%
  mutate(lang = sprintf("%s (%.1f%%)", lang, p)) %>%
  mutate(lang = fct_inorder(lang)) %>%
  ggplot() +
  geom_bar(aes(x ="", p, fill = lang), color = "gray10",
           width = 1, stat = "identity") +
  coord_polar("y", start = 0, direction = -1) +
  theme(axis.text.x = element_blank(), legend.position = "bottom") +
  labs(x = "", y = "", fill = "",
       title = "In what language were tweets written?",
       subtitle = paste0(mecz, toupper(twitter_query_tw)),
       caption = caption_str)




# skąd pochodzą
places <- tweets %>%
  select(status_id, place_full_name, country) %>%
  na.omit()

if(nrow(places) != 0) {

  p2 <- places %>%
    count(place_full_name) %>%
    mutate(p = 100*n/sum(n)) %>%
    filter(n > 1) %>%
    top_n(10, n) %>%
    arrange(n, place_full_name) %>%
    mutate(place_full_name = fct_inorder(place_full_name)) %>%
    separate(place_full_name, c("city", "country"),
             sep = ", ", remove = FALSE) %>%
    mutate(country = ifelse(is.na(country), city, country)) %>%
    ggplot() +
    geom_col(aes(place_full_name, n, fill = country),
             show.legend = FALSE, color = "gray10") +
    geom_text(aes(place_full_name, n, label = sprintf("%.1f%%", p)), hjust = -0.2) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.15), add = 0)) +
    coord_flip() +
    theme(axis.text.x = element_blank(), panel.grid = element_blank()) +
    labs(y = "% of tweets", x = "",
         title = "What cities do tweets come from?",
         subtitle = paste0(mecz, toupper(twitter_query_tw),
                           " (only for tweets that have a given location)"),
         caption = caption_str)



  p3 <- places %>%
    mutate(country = ifelse(country == "Rusia", "Russia", country)) %>%
    count(country) %>%
    mutate(p = 100*n/sum(n)) %>%
    filter(n > 1) %>%
    top_n(10, n) %>%
    arrange(n, country) %>%
    mutate(country = fct_inorder(country)) %>%
    ggplot() +
    geom_col(aes(country, n), fill = "lightgreen", color = "gray10") +
    geom_text(aes(country, n, label = sprintf("%.1f%%", p)), hjust = -0.2) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.15), add = 0)) +
    coord_flip() +
    theme(axis.text.x = element_blank(), panel.grid = element_blank()) +
    labs(x = "", y = "",
         title = "What countries do tweets come from?",
         subtitle = paste0(mecz, toupper(twitter_query_tw),
                           " (only for tweets that have a given location)"),
         caption = caption_str)


}

# skąd pochodzą tweety (o ile mają współrzędne)
places_geo <- tweets %>%
  select(status_id, geo_coords) %>%
  na.omit() %>%
  distinct(status_id, .keep_all = TRUE) %>%
  unnest(geo_coords) %>%
  group_by(status_id) %>%
  mutate(p = c("lat", "long")) %>%
  ungroup() %>%
  spread(p, geo_coords) %>%
  na.omit()

if(nrow(places_geo) != 0) {
  # mapa <- map_data("world") %>% filter(region == "Poland")
  mapa <- map_data("world") # %>% filter(region == "USA", subregion == "New York")

  places_points <- places_geo %>%
    filter(lat >= min(mapa$lat), lat <= max(mapa$lat),
           long >= min(mapa$long), long <= max(mapa$long)) %>%
    count(long, lat)

  p4 <- ggplot() +
    geom_polygon(data = mapa, aes(long, lat, group=group),
                 fill = NA, color = "gray30") +
    geom_point(data = places_points,
               aes(long, lat, size = n),
               color = "red", alpha = 0.5,
               show.legend = FALSE) +
    scale_size_continuous(range = c(2,7)) +
    coord_quickmap() +
    theme(axis.text = element_blank(), axis.ticks = element_blank()) +
    labs(x = "", y = "", size = "",
         title = "What places tweets come from?",
         subtitle = paste0(mecz, toupper(twitter_query_tw),
                           " (only for tweets that have a given location)"),
         caption = caption_str)
}

# text
words <- tweets %>%
  filter(!is_retweet) %>%
  select(created_at, text, lang) %>%
  # bez linków
  mutate(text = gsub("\n", "", text)) %>%
  mutate(text = gsub("?(f|ht)tp(s?)://(.*)[.][a-zA-Z0-9/]+", "", text)) %>%
  unnest_tokens("word", text, token = "words") %>%
  # międzynarodowe stop words
  filter(!word %in% unlist(stopwords::data_stopwords_stopwordsiso)) %>%
  # dedykkowane stopwords
  filter(!word %in% dedicated_stop_words)


p8 <- words %>%
  mutate(created_at = floor_date(created_at, unit = "minute")) %>%
  count(created_at, word) %>%
  ungroup() %>%
  group_by(created_at) %>%
  filter(n == max(n), n > 1) %>%
  ungroup() %>%
  mutate(created_at = with_tz(created_at, "Europe/Warsaw")) %>%
  ggplot() +
  geom_point(aes(created_at, word, size = n),
             color = "red", alpha = 0.25, show.legend = FALSE) +
  labs(size = "", x = "", y = "",
       title = "The most popular words over time",
       subtitle = paste0(mecz, toupper(twitter_query_tw)),
       caption = caption_str) +
  theme(axis.text = element_text(size = 14))



if(momenty_show) {

  if(Sys.time() > min(momenty$time)) {
    p8 <- p8 +
      geom_vline(data = momenty %>% filter(time < Sys.time()),
                 aes(xintercept = time), color = "blue") +
      geom_label_repel(data = momenty %>% filter(time < Sys.time()),
                       aes(x = time, y = 0, label = co))
  }
}


p9_data <- words %>%
  mutate(created_at = floor_date(created_at, unit = "5 minutes")) %>%
  count(created_at, word) %>%
  ungroup() %>%
  group_by(created_at) %>%
  top_n(3, n) %>%
  ungroup() %>%
  mutate(created_at = with_tz(created_at, "Europe/Warsaw")) %>%
  group_by(created_at) %>%
  arrange(desc(n)) %>%
  mutate(pos = row_number(),
         time_n_max = max(n)) %>%
  ungroup() %>%
  filter(pos <= 3)

p9 <- p9_data %>%
  ggplot() +
  geom_col(aes(created_at, n, fill = word),
           color = "gray10", position = position_dodge()) +
  geom_text(aes(created_at, 0.1*max(p9_data$n)+time_n_max, color = word,
                label = ifelse(pos == 1, word, "")),
            show.legend = FALSE, angle = 90, size = 6) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text = element_text(size = 16)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.175), add = 0)) +
  labs(x = "", y = "Number of tweets", fill = "",
       title = "Three of the most popular words in 5-minute blocks",
       subtitle = paste0(mecz, toupper(twitter_query_tw)),
       caption = caption_str)

if(momenty_show) {

  if(Sys.time() > min(momenty$time)) {
    p9 <- p9 +
      geom_vline(data = momenty %>% filter(time < Sys.time()),
                 aes(xintercept = time), color = "blue") +
      geom_label_repel(data = momenty %>% filter(time < Sys.time()),
                       aes(x = time, y = max(p9_data$n), label = co))
  }
}


# text
biwords <- tweets %>%
  filter(!is_retweet) %>%
  select(created_at, text, lang) %>%
  # bez linków
  mutate(text = gsub("\n", "", text)) %>%
  mutate(text = gsub("?(f|ht)tp(s?)://(.*)[.][a-zA-Z0-9/]+", "", text)) %>%
  unnest_tokens("word", text, token = "ngrams", n = 2) %>%
  separate(word, c("word1", "word2")) %>%
  na.omit() %>%
  # międzynarodowe stop words
  filter(!word1 %in% unlist(stopwords::data_stopwords_stopwordsiso)) %>%
  filter(!word2 %in% unlist(stopwords::data_stopwords_stopwordsiso)) %>%
  # dedykkowane stopwords
  filter(!word1 %in% dedicated_stop_words) %>%
  filter(!word2 %in% dedicated_stop_words) %>%
  unite(word, word1, word2, sep = " ")



top_langs <- top_langs %>% filter(lang != "und") %>% top_n(9, n) %>% pull(lang)


p7b <- biwords %>%
  filter(lang %in% top_langs) %>%
  count(lang, word) %>%
  ungroup() %>%
  group_by(lang) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  group_by(lang) %>%
  top_n(11, n) %>%
  arrange(desc(n)) %>%
  mutate(rw = row_number()) %>%
  filter(rw <= 10) %>%
  ungroup() %>%
  arrange(desc(word)) %>%
  mutate(word = fct_inorder(word),
         lang = factor(lang, levels = top_langs)) %>%
  ggplot(aes(word, p, fill = lang)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~lang, scales = "free_y") +
  labs(y = "% words from tweets written in a given language", x = "",
       title = "Most popular bi-words in tweets from individual languages",
       subtitle = paste0(mecz, toupper(twitter_query_tw)))



words_cloud <- words %>%
  count(word, lang) %>%
  group_by(word) %>%
  summarise(n = sum(n)) %>%
  ungroup()


png("pics/p6.png", width=10, height=10, units="in", res=100)
wordcloud(words_cloud$word, words_cloud$n,
          max.words = 100, min.freq = quantile(words_cloud$n, 0.25),
          scale = c(2.8, 1.2),
          colors = colorRampPalette(c("#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b"))(16),
          random.color = FALSE)
title(paste0("100 most popular words in tweets from the hashtags ", twitter_query_tw))
dev.off()

biwords_cloud <- biwords %>% count(word)

png("pics/p6b.png", width=10, height=10, units="in", res=100)
wordcloud(biwords_cloud$word, biwords_cloud$n,
          max.words = 100, min.freq = quantile(biwords_cloud$n, 0.25),
          scale = c(2.2, 0.8),
          colors = colorRampPalette(c("#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b"))(16),
          random.color = FALSE)
title(paste0("100 most popular bi-words in tweets from the hashtags ", twitter_query_tw))
dev.off()


words <- words %>%
  count(word, lang) %>%
  group_by(lang) %>%
  mutate(nlang = sum(n)) %>%
  ungroup()

p7 <- words %>%
  # bez szukanego tagu
  filter(word != tolower(gsub("#", "", twitter_query, fixed = TRUE))) %>%
  filter(word != tolower(gsub("#", "", twitter_query_rev, fixed = TRUE))) %>%
  mutate(p = 100*n/nlang) %>%
  filter(lang %in% top_langs) %>%
  group_by(lang) %>%
  top_n(11, n) %>%
  filter(n > min(n)) %>%
  ungroup() %>%
  arrange(desc(word)) %>%
  mutate(word = fct_inorder(word),
         lang = factor(lang, levels = top_langs)) %>%
  ggplot(aes(word, p, fill = lang)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~lang, scales = "free_y") +
  labs(y = "% words from tweets written in a given language", x = "",
       title = "Most popular words in tweets from individual languages",
       subtitle = paste0(mecz, toupper(twitter_query_tw)))


# zapisanie utworzonych obrazków

if(!is.null(p1)) {
  ggsave("pics/p1.png", plot = p1, width = 12, height = 9, units = "in", dpi = 100)
  if(post_tweets) post_tweet(status = paste0(match_result_str, " ", mecz_tw, toupper(twitter_query_tw), ": number of tweets over time\n#worldcup2018 #worldcup"), media = "pics/p1.png")
}

if(!is.null(p2)) {
  # ggsave("pics/p2.png", plot = p2, width = 12, height = 9, units = "in", dpi = 100)
  # if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": z jakich miast pochodzą tweety?"), media = "pics/p2.png")
}

if(!is.null(p3)) {
  # ggsave("pics/p3.png", plot = p3, width = 12, height = 9, units = "in", dpi = 100)
  # if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": What countries do tweets come from?\n#worldcup2018 #worldcup"), media = "pics/p3.png")
}

if(!is.null(p4)) {
  # ggsave("pics/p4.png", plot = p4, width = 12, height = 9, units = "in", dpi = 100)
  # if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": What places do tweets come from?\n#worldcup2018 #worldcup"), media = "pics/p4.png")
}

if(!is.null(p5)) {
  # ggsave("pics/p5.png", plot = p5, width = 12, height = 9, units = "in", dpi = 100)
  # if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": What language are tweets written in?\n#worldcup2018 #worldcup"), media = "pics/p5.png")
}

if(post_tweets) post_tweet(status = paste0(match_result_str, " ", mecz_tw, toupper(twitter_query_tw), ": the most popular words in tweets\n#worldcup2018 #worldcup"), media = "pics/p6.png")

if(post_tweets) post_tweet(status = paste0(match_result_str, " ", mecz_tw, toupper(twitter_query_tw), ": the most popular bi-words in tweets\n#worldcup2018 #worldcup"), media = "pics/p6b.png")

if(!is.null(p7)) {
  ggsave("pics/p7.png", plot = p7, width = 12, height = 9, units = "in", dpi = 100)
  if(post_tweets) post_tweet(status = paste0(match_result_str, " ", mecz_tw, toupper(twitter_query_tw), ": the most popular words in individual languages\n#worldcup2018 #worldcup"), media = "pics/p7.png")
}

if(!is.null(p7b)) {
  ggsave("pics/p7b.png", plot = p7b, width = 12, height = 9, units = "in", dpi = 100)
  if(post_tweets) post_tweet(status = paste0(match_result_str, " ", mecz_tw, toupper(twitter_query_tw), ": the most popular bi-words in individual languages\n#worldcup2018 #worldcup", rtag1), media = "pics/p7b.png")
}


if(!is.null(p8)) {
  ggsave("pics/p8.png", plot = p8, width = 12, height = 9, units = "in", dpi = 100)
  if(post_tweets) post_tweet(status = paste0(match_result_str, " ", mecz_tw, toupper(twitter_query_tw), ": the most popular words over time\n#worldcup2018 #worldcup", rtag2), media = "pics/p8.png")
}


if(!is.null(p9)) {
  ggsave("pics/p9.png", plot = p9, width = 12, height = 9, units = "in", dpi = 100)
  if(post_tweets) post_tweet(status = paste0(match_result_str, " ", mecz_tw, toupper(twitter_query_tw), ": the three most popular words in 5-minute blocks\n#worldcup2018 #worldcup", rtag3), media = "pics/p9.png")
}

