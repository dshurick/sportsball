
library(ffanalytics)

my_scrape <- ffanalytics::scrape_data(
  src = c(
    "CBS",
    "ESPN",
    "FantasyData",
    "FantasySharks",
    "FFToday",
    "NumberFire",
    "Yahoo",
    "RTSports",
    "Walterfootball"
  ),
  pos = c("QB", "RB", "WR", "TE", "DST", "K"),
  season = 2018,
  week = 2
)

my_scrape %>%
  readr::write_rds("./data/interim/scrape_data_20180910.rds")
