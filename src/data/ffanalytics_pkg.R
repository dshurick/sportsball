
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
    "NFL",
    "RTSports",
    "Walterfootball"
  ),
  pos = c("QB", "RB", "WR", "TE", "DST", "K"),
  season = 2018,
  week = 0
)

my_scrape %>%
  readr::write_rds('./data/interim/scrape_data_20180908.rds')

scoring_rules <- list(
  pass = list(
    pass_yds = 0.04,
    pass_tds = 4,
    pass_int = -2
  ),
  rush = list(
    all_pos = TRUE,
    rush_yds = 0.1,
    rush_att = 0,
    rush_tds = 6
  ),
  rec = list(
    all_pos = TRUE,
    rec = 0.5,
    rec_yds = 0.1,
    rec_tds = 6
  ),
  misc = list(
    all_pos = TRUE,
    fumbles_lost = -3,
    two_pts = 2
  ),
  kick = list(
    xp = 1.0,
    fg_0019 = 3.0,
    fg_2029 = 3.0,
    fg_3039 = 3.0,
    fg_4049 = 4.0,
    fg_50 = 5.0,
    fg_miss = -1.0
  ),
  dst = list(
    dst_fum_rec = 2,
    dst_int = 2,
    dst_safety = 2,
    dst_sacks = 2,
    dst_td = 6,
    dst_blk = 2
  ),
  pts_bracket = list(
    list(threshold = 0, points = 15),
    list(threshold = 6, points = 7),
    list(threshold = 13, points = 4),
    list(threshold = 20, points = 1),
    list(threshold = 27, points = 0),
    list(threshold = 34, points = -1),
    list(threshold = 99, points = -4)
  )
)



my_projections <-
  projections_table(
    my_scrape,
    scoring_rules = scoring_rules,
    vor_baseline = c(
      QB = 21,
      RB = 31,
      WR = 37,
      TE = 5,
      K = 11,
      DST = 11,
      DL = 11,
      LB = 11,
      DB = 11
    )
  )

add_aav <-
  function(projection_table,
           sources = c("RTS", "ESPN", "Yahoo", "NFL")) {
    sources <- match.arg(sources, several.ok = TRUE)
    lg_type <- attr(projection_table, "lg_type")
    season <- attr(projection_table, "season")
    week <- attr(projection_table, "week")
    if (week != 0) {
      warning("AAV data is not available for weekly data",
              call. = FALSE)
      return(projection_table)
    }
    adp_tbl <- get_adp(sources, type = "AAV") %>%
      select(1, length(.)) %>%
      rename_at(length(.), ~ function(x)
        return("aav"))
    
    projection_table <- left_join(projection_table, adp_tbl,
                                  by = "id")
    projection_table %>%
      `attr<-`(which = "season", season) %>%
      `attr<-`(which = "week", week) %>%
      `attr<-`(which = "lg_type",
               lg_type)
  }

my_projections <- my_projections %>%
  add_ecr() %>%
  add_risk() %>%
  add_adp(sources = "Yahoo") %>%
  add_aav(sources = "Yahoo")

my_projections <- my_projections %>% add_player_info()

appdata <- my_projections %>%
  dplyr::mutate(`Player (Team)` = sprintf("%s %s (%s)", first_name, last_name, team)) %>%
  dplyr::select(
    id,
    avg_type,
    `Rank` = rank,
    Team = team,
    Age = age,
    Exp = exp,
    sdPts = sd_pts,
    `Pos Rank` = pos_rank,
    `Player (Team)`,
    `Pos` = pos,
    `Points` = points,
    `Floor` = floor,
    `Ceiling` = ceiling,
    VOR = points_vor,
    Risk = risk,
    `Drop off` = drop_off,
    ECR = ecr,
    ADP = adp,
    `ADP Diff` = adp_diff,
    AAV = aav
  ) %>%
  dplyr::mutate(
    pos_equiv = case_when(Pos %in% c("WR", "TE") ~ "WR_TE",
                          TRUE ~ Pos),
    owner = factor(
      x = character(n()),
      levels = c(
        "Devon",
        "Nathan",
        "Taylor",
        "Brant",
        "Evan",
        "Kyle",
        "Kevin",
        "Adam",
        "Dave",
        "Tom"
      )
    ),
    paid = as.numeric(NA)
  ) %>%
  dplyr::mutate_if(is.character, as.factor)

appdata <- appdata %>%
  dplyr::arrange(id, avg_type, desc(Points)) %>%
  dplyr::distinct(id, avg_type, .keep_all = TRUE) %>%
  dplyr::select(-VOR)

my_projections %>%
  readr::write_rds("./data/processed/ff_projections.rds")

appdata %>%
  readr::write_rds("./data/processed/appdata.rds")
