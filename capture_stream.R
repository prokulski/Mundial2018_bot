# paramety z shela
# args[1] # end hour:min
# args[2] # 1st tag
# args[3] # 2nd tag

suppressPackageStartupMessages(library(methods))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(rtweet))
suppressPackageStartupMessages(library(fs))

Sys.setenv(TWITTER_PAT="/home/lemur/RProjects/Mundial2018_twitter_stream/twitter_token.rdata")


options(echo=FALSE)
args <- commandArgs(trailingOnly = TRUE)

# args <- c("CRC", "SRB", "16:15")

setwd("~/RProjects/Mundial2018_twitter_stream/")

# 1 tag
if(length(commandArgs()) == 7) {

  tag1 <- str_sub(args[2], 1, 3)
  tag2 <- str_sub(args[2], 4, 6)
  twitter_query <- paste0("#", tag1, tag2, ",#", tag2, tag1)
  rm(tag1, tag2)
}

# 2 tagi
if(length(commandArgs()) == 8) {

  tag1 <- str_sub(args[2], 1, 3)
  tag2 <- str_sub(args[2], 4, 6)
  tag3 <- str_sub(args[3], 1, 3)
  tag4 <- str_sub(args[3], 4, 6)
  twitter_query <- paste0("#", tag1, tag2, ",#", tag2, tag1,
                          ",#", tag3, tag4, ",#", tag4, tag3)
  rm(tag1, tag2, tag3, tag4)
}

twitter_query <- str_to_lower(twitter_query)


# do kiedy zbieramy tweety?
end_time <- paste0(format(Sys.Date(), "%Y-%m-%d "), args[1], ":00")

cat(paste0("\ttwitter_query = '", twitter_query, "'\n"))
cat(paste0("\tend_time = '", end_time, "'\n\n"))



stream_tweets_safe <- safely(stream_tweets)

end_time <- ymd_hms(end_time, tz = "Europe/Warsaw")
tweets_full <- tibble()

# dopóki nie minie ustalona pora
while(Sys.time() < end_time) {
  cat(paste0(
    "Jest: ", format(Sys.time(), "%H:%M (%d/%m)"),
    " czekam na: ", format(end_time, "%H:%M (%d/%m)"),
    "\n"))


  # streamuj przez 5 minut
  twitter_stream <- stream_tweets_safe(
    q = twitter_query,
    timeout = 60, # in secs
    parse = TRUE,
    file = "tweets"
  )

  # jeśli bez błędów to jedziemy
  if(!is.null(twitter_stream$result)) {

    # wybierze tylko potrzebne później kolumny
    tweets <- twitter_stream$result %>%
      select(status_id, created_at,
             screen_name,
             text, is_retweet, lang,
             retweet_count, favorite_count,
             place_full_name, country, geo_coords) %>%
      distinct(status_id, .keep_all = TRUE)

    # usuń atrybuty z df
    attr(tweets, "users") <- NULL

    # złącz razem
    tweets_full <- bind_rows(tweets_full, tweets)

    # zapisz w RDS z aktualną godziną
    file_name <- paste0("twitter_data/", format(Sys.time(), "%Y_%m_%d_%H%M_twitts_full.RDS"))
    saveRDS(tweets_full, file = file_name)

    # kasujemy pliki zebrane w sstreamingu
    file_delete("tweets.json")

    # kasujemy stare pliki rds
    files_to_del <- list.files("twitter_data/", pattern = "*.RDS") %>%
      paste0("twitter_data/", .) %>%
      .[. != file_name]

     if(length(files_to_del) != 0) file_delete(files_to_del)

  } else {
    cat(paste0(
      "Jest: ", format(Sys.time(), "%H:%M"),
      " i zjebało się błędem:\n", twitter_stream$error, "\n\n"))
  }
}

# na koniec odkładamy plik do archiwum
saveRDS(tweets_full,
        file = paste0("arch/", gsub("[#,]", "", twitter_query), ".RDS"))

