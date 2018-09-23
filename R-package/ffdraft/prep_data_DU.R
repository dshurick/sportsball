
my_scrape$QB %>%
  dplyr::count(id, sort = TRUE)

my_scrape$QB %>%
  dplyr::filter(is.na(id))

QB <- my_scrape$QB

my_scrape$RB %>%
  ggformula::gf_boxplot(~rush_yds, color = ~data_src)

my_scrape$RB %>%
  dplyr::count(id, sort = TRUE) %>%
  View()

my_scrape <- readr::read_rds('./data/interim/scrape_data_20180903.rds')

scoring_rules_DU <- list(
  pass = list(
    pass_yds = 0.04,
    pass_tds = 6,
    pass_int = -3
  ),
  rush = list(
    all_pos = TRUE,
    rush_yds = 0.1,
    rush_att = 0,
    rush_40_yds = 0,
    rush_tds = 6,
    rush_100_yds = 3,
    rush_150_yds = 3,
    rush_200_yds = 10
  ),
  rec = list(
    all_pos = TRUE,
    rec = 0.5,
    rec_yds = 0.1,
    rec_tds = 6,
    rec_40_yds = 0,
    rec_100_yds = 3,
    rec_150_yds = 3,
    rec_200_yds = 10
  ),
  misc = list(
    all_pos = TRUE,
    fumbles_lost = -2,
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
  ret = list(
    all_pos = TRUE,
    return_tds = 6,
    return_yds = 0
  ),
  dst = list(
    dst_fum_rec = 2,
    dst_int = 2,
    dst_safety = 4,
    dst_sacks = 1,
    dst_td = 6,
    dst_blk = 4
  ),
  pts_bracket = list(
    list(threshold = 0, points = 10),
    list(threshold = 6, points = 7),
    list(threshold = 13, points = 4),
    list(threshold = 17, points = 1),
    list(threshold = 21, points = 0),
    list(threshold = 27, points = -1),
    list(threshold = 34, points = -4),
    list(threshold = 45, points = -7),
    list(threshold = 99, points = -10)
  )
)

my_projections_DU <-
  projections_table(
    my_scrape,
    scoring_rules = scoring_rules_DU,
    vor_baseline = c(
      QB = 12,
      RB = 41,
      WR = 44,
      TE = 10,
      K = 1,
      DST = 2
    )
  ) %>%
  add_ecr() %>%
  add_risk() %>%
  add_adp(sources = "ESPN")

appdata <- my_projections_DU %>%
  add_player_info() %>%
  dplyr::mutate(player = sprintf("%s %s (%s)", first_name, last_name, team)) %>%
  dplyr::mutate_if(is.character, as.factor) %>%
  dplyr::arrange(id, avg_type, desc(points)) %>%
  dplyr::distinct(id, avg_type, .keep_all = TRUE)

appdata %>%
  readr::write_rds("./data/processed/appdata_DU.rds")
