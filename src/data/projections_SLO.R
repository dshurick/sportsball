
library(ffanalytics)
library(rvest)

xmldoc <-
  read_html("https://projects.fivethirtyeight.com/2018-nfl-predictions/?ex_cid=rrpromo")

(
  dtf <- tibble::tibble(
    elo_rating =
      xmldoc %>%
      html_nodes("td.elo") %>%
      html_text(),
    team = xmldoc %>%
      html_nodes(".full") %>%
      html_text(),
    division = xmldoc %>%
      html_nodes("td.division") %>%
      html_text(),
    wins = xmldoc %>%
      html_nodes(".num.div") %>%
      html_text(),
    losses = xmldoc %>%
      html_nodes(".div+ .num") %>%
      html_text(),
    make_playoffs = xmldoc %>%
      html_nodes(".pct.div") %>%
      html_text(),
    win_division = xmldoc %>%
      html_nodes("td.division-chances") %>%
      html_text(),
    week1bye = xmldoc %>%
      html_nodes("td.pct.desktop") %>%
      html_text(),
    super_bowl = xmldoc %>%
      html_nodes(".superbowl") %>%
      html_text()
  ) %>%
    dplyr::mutate_at(
      .vars = vars(
        elo_rating,
        wins,
        losses,
        make_playoffs,
        win_division,
        week1bye,
        super_bowl
      ),
      .funs = parse_number
    ) %>%
    dplyr::mutate_at(
      .vars = vars(make_playoffs, win_division, week1bye, super_bowl),
      .funs = funs(. / 100)
    )
)

parse_draft <- function(x, ...) {
  xmldoc <- read_html(x, ...)
  players <- xmldoc %>%
    html_nodes(".tableBody td:nth-child(2)") %>%
    html_text()
  
  costs <- xmldoc %>%
    html_nodes(".tableBody td~ td+ td") %>%
    html_text()
  
  teamnames <- xmldoc %>%
    html_nodes(".tableHead td") %>%
    html_text() %>%
    stringr::str_trim() %>%
    stringr::str_squish()
  
  dtf <- dplyr::bind_cols(
    players %>%
      stringr::str_match(
        "(([^\\,]+), ([[:alpha:]]+)[[:space:]]([[:alpha:]\\/]+)|([[:alpha:]]+) (D/ST))"
      ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        player = case_when(is.na(V3) ~ V6,
                           TRUE ~ V3),
        pos = case_when(is.na(V5) ~ V7,
                        TRUE ~ V5),
        team = V4
      ) %>%
      dplyr::select(player, team, pos),
    tibble::tibble(paid = costs)
  ) %>%
    dplyr::mutate(paid = parse_number(paid),
                  ownder = rep(teamnames, each = 18))
  return(dtf)
}

draftresults <-
  map_dfr(2013:2017,
          ~ parse_draft(sprintf("./data/raw/ESPN_auction_%d.html", .x)) %>%
            dplyr::mutate(season = .x))

byseason <- draftresults %>%
  dplyr::mutate(value = paid - 1) %>%
  dplyr::group_by(season, pos) %>%
  dplyr::summarise(num = sum(value > 0),
                   total_value = sum(value))

draftresults %>%
  dplyr::mutate(value = paid - 1) %>%
  dplyr::group_by(pos) %>%
  dplyr::summarise(num = sum(value > 0) / 5,
                   total_value = sum(value) / 5)

my_scrape <-
  readr::read_rds("./data/interim/scrape_data_20180910.rds")

scoring_rules <- list(
  pass = list(
    pass_yds = 0.04,
    pass_tds = 4,
    pass_int = -2,
    pass_comp = 0.2,
    pass_300_yds = 2,
    pass_350_yds = 2,
    pass_400_yds = 5
  ),
  rush = list(
    all_pos = TRUE,
    rush_yds = 0.1,
    rush_att = 0,
    rush_tds = 6,
    rush_100_yds = 3,
    rush_150_yds = 3,
    rush_200_yds = 6
  ),
  rec = list(
    all_pos = TRUE,
    rec = 0.5,
    rec_yds = 0.1,
    rec_tds = 6,
    rec_100_yds = 3,
    rec_150_yds = 3,
    rec_200_yds = 6
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
  dst = list(
    dst_fum_rec = 2,
    dst_int = 2,
    dst_safety = 2,
    dst_sacks = 1,
    dst_td = 6,
    dst_blk = 2
  ),
  pts_bracket = list(
    list(threshold = 0, points = 12),
    list(threshold = 6, points = 7),
    list(threshold = 13, points = 4),
    list(threshold = 17, points = 1),
    list(threshold = 21, points = 0),
    list(threshold = 27, points = -1),
    list(threshold = 34, points = -4),
    list(threshold = 45, points = -7),
    list(threshold = 99, points = -12)
  )
)

my_projections <-
  projections_table(
    my_scrape,
    src_weights = c(
      CBS = 0.344,
      Yahoo = 0.400,
      ESPN = 0.329,
      NFL = 0,
      FFToday = 0.379,
      NumberFire = 0.322,
      FantasyPros = 0.000,
      FantasySharks = 0.327,
      FantasyFootballNerd = 0.000,
      Walterfootball = 0,
      RTSports = 0.330,
      FantasyData = 0.428,
      Fleaflicker = 0.428
    ),
    scoring_rules = scoring_rules,
    vor_baseline = c(
      QB = 24,
      RB = 41,
      WR = 44,
      TE = 11,
      K = 3,
      DST = 5
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
  add_adp(sources = "ESPN") %>%
  add_aav(sources = "ESPN")

my_projections <- my_projections %>% add_player_info()

.tbl <- my_projections %>%
  dplyr::filter(avg_type == 'weighted') %>%
  dplyr::mutate(player = sprintf("%s %s (%s)", first_name, last_name, team)) %>%
  dplyr::select(
    id,
    player,
    avg_type,
    rank,
    team,
    age,
    exp,
    sdPts = sd_pts,
    pos_rank,
    pos,
    points,
    points_vor,
    risk,
    drop_off,
    ecr,
    adp,
    adp_diff,
    aav
  )

library(tidyverse)
library(magrittr)
library(Rglpk)
library(googlesheets)

worksheet <- googlesheets::gs_title("NFL Draft 2018")

gs_edit_cells(
  worksheet,
  ws = 'Sheet1',
  input = .tbl %>%
    dplyr::select(id, player, rank, team),
  anchor = "A1",
  col_names = TRUE,
  trim = TRUE
)

worksheet <- googlesheets::gs_title("NFL Draft 2018")

gs_edit_cells(
  worksheet,
  ws = 'Sheet1',
  input = .tbl %>%
    dplyr::select(exp, sdPts, pos_rank, pos),
  anchor = "E1",
  col_names = TRUE
)

worksheet <- googlesheets::gs_title("NFL Draft 2018")

gs_edit_cells(
  worksheet,
  ws = 'Sheet1',
  input = .tbl %>%
    dplyr::select(points, points_vor, risk, drop_off),
  anchor = "I1",
  col_names = TRUE
)

worksheet <- googlesheets::gs_title("NFL Draft 2018")

gs_edit_cells(
  worksheet,
  ws = 'Sheet1',
  input = .tbl %>%
    dplyr::select(ecr, adp, aav),
  anchor = "M1",
  col_names = TRUE
)

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
    VOR_static = points_vor,
    Risk = risk,
    `Drop off` = drop_off,
    ECR = ecr,
    ADP = adp,
    `ADP Diff` = adp_diff,
    AAV = aav
  ) %>%
  dplyr::mutate(
    pos_equiv = Pos,
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
  dplyr::mutate(VOR_static = pmax(VOR_static, 0)) %>%
  dplyr::filter(!is.na(VOR_static))

# my_projections %>%
#   readr::write_rds("./data/processed/ff_projections.rds")

appdata %>%
  readr::write_rds("./data/processed/SLO_appdata.rds")
