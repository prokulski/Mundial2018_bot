# generator wpisów do CRONa dla meczy rozgrywających się pojedynczo (jeden mecz o jednej porze)

library(tidyverse)
library(lubridate)

ttable <- read.table(text = "
25 6 16:00 URU RUS
25 6 16:00 KSA EGY
25 6 20:00 ESP MAR
25 6 20:00 IRN POR
26 6 16:00 AUS PER
26 6 16:00 DEN FRA
26 6 20:00 NGA ARG
26 6 20:00 ISL CRO
27 6 16:00 KOR GER
27 6 16:00 MEX SWE
27 6 20:00 SRB BRA
27 6 20:00 SUI CRC
28 6 16:00 JPN POL
28 6 16:00 SEN COL
28 6 20:00 PAN TUN
28 6 20:00 ENG BEL
") %>%
  set_names(c("day", "month", "hour", "teama", "teamb"))

for(i in 1:nrow(ttable)) {
  # # capture
  # 45 13 19 6 * Rscript /home/lemur/RProjects/Mundial2018_twitter_stream/capture_stream.R 16:15 COLJPN
  # # post
  # 50 14 19 6 * Rscript /home/lemur/RProjects/Mundial2018_twitter_stream/analyze_stream.R 1 COL JPN
  # 2 16 19 6 * Rscript /home/lemur/RProjects/Mundial2018_twitter_stream/analyze_stream.R 1 COL JPN
  # 17 16 19 6 * Rscript /home/lemur/RProjects/Mundial2018_twitter_stream/polskie_mecze.R 1 eng COL JPN

  time <- ymd_hm(paste0("2018-0", as.character(ttable[i, "month"]),
                        "-", as.character(ttable[i, "day"]), " " ,
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
