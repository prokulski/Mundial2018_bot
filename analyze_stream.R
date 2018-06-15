setwd("~/RProjects/Mundial2018_twitter_stream/twitter_data/")
rm(list = ls())

momenty_show <- FALSE


options(echo=FALSE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)

# args[1] # 1st team name
# args[2] # 2nd team name
# args[3] # 1st team
# args[4] # 2nd team
# args[5] # post tweet? 0/1

# post twtiter
#mecz <- "Match Portugal #POR - Spain #ESP on hashtag "
mecz <- paste0("Match ", args[1], " #", toupper(args[3]), " - ", args[2], " #", toupper(args[4]), " on hashtag ")

#mecz_tw <- "Portugal #POR - Spain #ESP on hashtags "
mecz_tw <- paste0(args[1], " #", toupper(args[3]), " - ", args[2], " #", toupper(args[4]), " on hashtag ")

#twitter_query <- "#poresp"
twitter_query <- paste0("#", tolower(args[3]), tolower(args[4]))

#twitter_query_rev <- "#esppor"
twitter_query_rev <- paste0("#", tolower(args[4]), tolower(args[3]))

#twitter_query_tw <- paste0(twitter_query, " & ", twitter_query_rev)
twitter_query_tw <- paste0(twitter_query, " & ", twitter_query_rev)

post_tweets <- ifelse(as.numeric(args[5]) == 0, FALSE, TRUE)

cat(paste0("mecz = '", mecz, "'\n"))
cat(paste0("mecz_tw = '", mecz_tw, "'\n"))
cat(paste0("twitter_query = '", twitter_query, "'\n"))
cat(paste0("twitter_query_rev = '", twitter_query_rev, "'\n"))
cat(paste0("twitter_query_tw = '", twitter_query_tw, "'\n"))
cat(paste0("post tweet? = '", post_tweets, "'\n"))


library(methods)
library(tidyverse)
library(lubridate)
library(tidytext)
library(ggthemes)
library(wordcloud)
library(fs)
library(rtweet)
library(ggrepel)
#
# twitter_query <- "#poresp"
# twitter_query_rev <- "#esppor"
# twitter_query_tw <- paste0(twitter_query, " & ", twitter_query_rev)
#
# mecz <- "Match Portugal #POR - Spain #ESP on hashtag "
# mecz_tw <- "Portugal #POR - Spain #ESP, let's see hashtags "

dedicated_stop_words <- c("https", "t.co",
                          "worldcuprussia2018", "worldcup2018", "worldcup",
                          "fifaworldcup", "fifaworldcup2018",
                          "rusia2018", "russia2018",
                          "wm2018", "mundial", "mundial2018")

spam_strings <- c("Tap below to vote now", "You can vote for your",
                  "Absolute world class performance")

momenty <- tribble(~time, ~co,
                   "2018-06-15 20:00", "Start",
                   "2018-06-15 20:45", "End of 1st half",
                   "2018-06-15 21:02", "End of break",
                   "2018-06-15 21:52", "End of 2nd half"
) %>%
  mutate(time = ymd_hm(time, tz = "Europe/Warsaw"))

Sys.setenv(TWITTER_PAT="/home/lemur/RProjects/Mundial2018_twitter_stream/twitter_token.rdata")

p1 <- NULL
p2 <- NULL
p3 <- NULL
p4 <- NULL
p5 <- NULL
p7 <- NULL
p8 <- NULL
p9 <- NULL

theme_set(theme_minimal() +
            theme(plot.title = element_text(family = NULL, face = "bold", size = 18, color = "black"),
                  plot.subtitle = element_text(family = NULL, face = "plain", size = 12, color = "black"),
                  plot.caption = element_text(family = NULL, face = "italic", size = 9, color = "darkgray"),
                  plot.background = element_rect(fill="#efefef", color="#aaaaaa"),
                  panel.background = element_rect(fill = "white", color="black"),
                  strip.text.x = element_text(face = "bold")))

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
  filter(!grepl(spam_strings[[3]], text))

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
       caption = "(c) 2018, Łukasz Prokulski, fb.com/DaneAnalizy")

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
       caption = "(c) 2018, Łukasz Prokulski, fb.com/DaneAnalizy")




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
    labs(x = "% of tweets", y = "",
         title = "What cities do tweets come from?",
         subtitle = paste0(mecz, toupper(twitter_query_tw),
                           " (only for tweets that have a given location)"),
         caption = "(c) 2018, Łukasz Prokulski, fb.com/DaneAnalizy")



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
         caption = "(c) 2018, Łukasz Prokulski, fb.com/DaneAnalizy")


}

# skąd pochodzą tweety (o ile mają współrzędne)
places_geo <- tweets %>%
  select(status_id, geo_coords) %>%
  na.omit() %>%
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
         caption = "(c) 2018, Łukasz Prokulski, fb.com/DaneAnalizy")


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
  filter(!word %in% dedicated_stop_words) %>%
  # tag z którego pochodzą tweety
  filter(word != tolower(gsub("#", "", twitter_query, fixed = TRUE))) %>%
  filter(word != tolower(gsub("#", "", twitter_query_rev, fixed = TRUE)))



p8 <- words %>%
  mutate(created_at = floor_date(created_at, unit = "minute")) %>%
  count(created_at, word) %>%
  ungroup() %>%
  group_by(created_at) %>%
  filter(n == max(n)) %>%
  ungroup() %>%
  mutate(created_at = with_tz(created_at, "Europe/Warsaw")) %>%
  ggplot() +
  geom_point(aes(created_at, word, size = n),
             color = "red", alpha = 0.25, show.legend = FALSE) +
  labs(size = "", x = "", y = "",
       title = "The most popular words over time",
       subtitle = paste0(mecz, toupper(twitter_query_tw)),
       caption = "(c) 2018, Łukasz Prokulski, fb.com/DaneAnalizy")

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
  ungroup()

p9 <- p9_data %>%
  group_by(created_at) %>%
  top_n(3, n) %>%
  ungroup() %>%
  mutate(created_at = with_tz(created_at, "Europe/Warsaw")) %>%
  group_by(created_at) %>%
  arrange(desc(n)) %>%
  mutate(pos = row_number()) %>%
  ungroup() %>%
  filter(pos <= 3) %>%
  ggplot() +
  geom_col(aes(created_at, n, fill = word),
           color = "gray10", position = position_dodge()) +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Number of tweets", fill = "",
       title = "Three of the most popular words in 5-minute blocks",
       subtitle = paste0(mecz, toupper(twitter_query_tw)),
       caption = "(c) 2018, Łukasz Prokulski, fb.com/DaneAnalizy")

if(momenty_show) {

  if(Sys.time() > min(momenty$time)) {
    p9 <- p9 +
      geom_vline(data = momenty %>% filter(time < Sys.time()),
                 aes(xintercept = time), color = "blue") +
      geom_label_repel(data = momenty %>% filter(time < Sys.time()),
                       aes(x = time, y = max(p9_data$n), label = co))
  }
}

words_cloud <- words %>%
  count(word, lang) %>%
  group_by(word) %>%
  summarise(n = sum(n)) %>%
  ungroup()


png("pics/p6.png", width=12, height=8, units="in", res=100)
wordcloud(words_cloud$word, words_cloud$n,
          max.words = 150, min.freq = quantile(words_cloud$n, 0.25),
          scale = c(2.8, 1.2),
          colors = colorRampPalette(c("#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b"))(16),
          random.color = FALSE)
title(paste0("The most popular words in tweets from the hashtags ", twitter_query_tw))
dev.off()

words <- words %>%
  count(word, lang) %>%
  group_by(lang) %>%
  mutate(nlang = sum(n)) %>%
  ungroup()

top_langs <- top_langs %>% filter(lang != "und") %>% top_n(9, n) %>% pull(lang)

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
       title = "Most popular words in tweets from individual languages (most popular)",
       subtitle = paste0(mecz, toupper(twitter_query_tw)))


# zapisanie utworzonych obrazków

if(!is.null(p1)) {
  ggsave("pics/p1.png", plot = p1, width = 12, height = 8, units = "in", dpi = 100)
  if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": number of tweets over time\n#worldcup2018 #worldcup"), media = "pics/p1.png")
}

if(!is.null(p2)) {
  ggsave("pics/p2.png", plot = p2, width = 12, height = 8, units = "in", dpi = 100)
  #  if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": z jakich miast pochodzą tweety?"), media = "pics/p2.png")
}

if(!is.null(p3)) {
  ggsave("pics/p3.png", plot = p3, width = 12, height = 8, units = "in", dpi = 100)
  # if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": What countries do tweets come from?\n#worldcup2018 #worldcup"), media = "pics/p3.png")
}

if(!is.null(p4)) {
  ggsave("pics/p4.png", plot = p4, width = 12, height = 8, units = "in", dpi = 100)
  # if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": What places do tweets come from?\n#worldcup2018 #worldcup"), media = "pics/p4.png")
}

if(!is.null(p5)) {
  ggsave("pics/p5.png", plot = p5, width = 12, height = 8, units = "in", dpi = 100)
  if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": What language are tweets written in?\n#worldcup2018 #worldcup"), media = "pics/p5.png")
}

if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": the most popular words in tweets\n#worldcup2018 #worldcup"), media = "pics/p6.png")

if(!is.null(p7)) {
  ggsave("pics/p7.png", plot = p7, width = 12, height = 8, units = "in", dpi = 100)
  if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": the most popular words in individual languages\n#worldcup2018 #worldcup"), media = "pics/p7.png")
}

if(!is.null(p8)) {
  ggsave("pics/p8.png", plot = p8, width = 12, height = 8, units = "in", dpi = 100)
  if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": the most popular words over time\n#worldcup2018 #worldcup"), media = "pics/p8.png")
}

if(!is.null(p9)) {
  ggsave("pics/p9.png", plot = p9, width = 12, height = 8, units = "in", dpi = 100)
  if(post_tweets) post_tweet(status = paste0(mecz_tw, toupper(twitter_query_tw), ": the three most popular words in 5-minute blocks\n#worldcup2018 #worldcup"), media = "pics/p9.png")
}

