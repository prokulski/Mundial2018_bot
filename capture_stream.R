options(echo=FALSE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)

# args[1] # 1st team
# args[2] # 2nd team
# args[3] # end hour:min

setwd("~/RProjects/Mundial2018_twitter_stream/")

twitter_query <- paste0("#", tolower(args[1]), tolower(args[2]))
twitter_query_rev <- paste0("#", tolower(args[2]), tolower(args[1]))

twitter_query <- paste0(twitter_query, ",", twitter_query_rev)
end_time <- paste0(format(Sys.Date(), "%Y-%m-%d "), args[3], ":00")

cat(paste0("twitter_query = '", twitter_query, "'\n"))
cat(paste0("end_time = '", end_time, "'\n"))

library(methods)
library(tidyverse)
library(lubridate)
library(rtweet)
library(fs)

Sys.setenv(TWITTER_PAT="/home/lemur/RProjects/Mundial2018_twitter_stream/twitter_token.rdata")


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

    twitter_stream <- twitter_stream$result

    # wybierze tylko potrzebne później kolumny
    tweets <- twitter_stream %>%
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
    saveRDS(tweets_full, file = paste0("twitter_data/",
                                       format(Sys.time(),
                                              "%Y_%m_%d_%H%M_twitts_full.RDS")))

    # kasujemy pliki zebrane w sstreamingu
    file_delete("tweets.json")
  } else {
    cat(paste0(
      "Jest: ", format(Sys.time(), "%H:%M"),
      " i zjebało się błędem:\n", twitter_stream$error, "\n\n"))
  }
}
