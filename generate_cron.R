# generator wpisów do CRONa dla meczy rozgrywających się pojedynczo (jeden mecz o jednej porze)

library(tidyverse)
library(lubridate)

ttable <- read.table(text = "
19 14:00 COL JPN
19 17:00 POL SEN
19 20:00 RUS EGY
20 14:00 POR MAR
20 17:00 URU KSA
20 20:00 IRN ESP
21 14:00 DEN AUS
21 17:00 FRA PER
21 20:00 ARG CRO
22 14:00 BRA CRC
22 17:00 NGA ISL
22 20:00 SRB SUI
23 14:00 BEL TUN
23 17:00 KOR MEX
23 20:00 GER SWE
24 14:00 ENG PAN
24 17:00 JPN SEN
24 20:00 POL COL
") %>%
  set_names(c("day", "hour", "teama", "teamb"))

for(i in 1:nrow(ttable)) {
  # # capture
  # 45 13 19 6 * Rscript /home/lemur/RProjects/Mundial2018_twitter_stream/capture_stream.R 16:15 COLJPN
  # # post
  # 50 14 19 6 * Rscript /home/lemur/RProjects/Mundial2018_twitter_stream/analyze_stream.R 1 COL JPN
  # 2 16 19 6 * Rscript /home/lemur/RProjects/Mundial2018_twitter_stream/analyze_stream.R 1 COL JPN
  # 17 16 19 6 * Rscript /home/lemur/RProjects/Mundial2018_twitter_stream/polskie_mecze.R 1 eng COL JPN

  time <- ymd_hm(paste0("2018-06-", as.character(ttable[i, "day"]), " " ,
                        as.character(ttable[i, "hour"])))
  teamA <- as.character(ttable[i, "teama"])
  teamB <- as.character(ttable[i, "teamb"])

  # coment1
  cat(paste0("# ", teamA, " ", teamB, " ", format(time, "%Y-%m-%d %H:%M"), "\n"))
  # collect
  cat("# capture\n")
  cat(paste0(minute(time - minutes(15)), " ",
             hour(time - minutes(15)), " ",
             day(time), " ",
             month(time), " ",
             wday(time, week_start = 1), " ",
             "Rscript /home/lemur/RProjects/Mundial2018_twitter_stream/capture_stream.R ",
             hour(time + minutes(135)), ":",
             minute(time + minutes(135)), " ",
             teamA, teamB, "\n"))
  # post
  cat("# post\n")
  # half
  cat(paste0(minute(time + minutes(55)), " ",
             hour(time + minutes(55)), " ",
             day(time), " ",
             month(time), " ",
             wday(time, week_start = 1), " ",
             "Rscript /home/lemur/RProjects/Mundial2018_twitter_stream/analyze_stream.R 1 ",
             teamA, " " , teamB, "\n"))
  # end
  cat(paste0(minute(time + minutes(124)), " ",
             hour(time + minutes(124)), " ",
             day(time), " ",
             month(time), " ",
             wday(time, week_start = 1), " ",
             "Rscript /home/lemur/RProjects/Mundial2018_twitter_stream/analyze_stream.R 1 ",
             teamA, " " , teamB, "\n"))
  # f-words
  cat(paste0(minute(time + minutes(137)), " ",
             hour(time + minutes(137)), " ",
             day(time), " ",
             month(time), " ",
             wday(time, week_start = 1), " ",
             "Rscript /home/lemur/RProjects/Mundial2018_twitter_stream/polskie_mecze.R 1 ",
             ifelse(teamA == "POL" | teamB == "POL", "pl ", "eng "),
             teamA, " " , teamB, "\n\n"))

  }
