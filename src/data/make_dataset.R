
library(tidyverse)

proj_DST <-
  readr::read_csv(
    "data/raw/FantasyPros_Fantasy_Football_Projections_DST.csv",
    col_names = c(
      'Player',
      'Team',
      'dstSack',
      'dstInt',
      'dstFumlRec',
      'dstFumlForce',
      'dstTd',
      'dstSafety',
      'dstPtsAllowed',
      'dstYdsAllowed',
      'points'
    ),
    col_types = cols(
      Player = col_character(),
      Team = col_character(),
      dstSack = col_double(),
      dstInt = col_double(),
      dstFumlRec = col_double(),
      dstFumlForce = col_double(),
      dstTd = col_double(),
      dstSafety = col_double(),
      dstPtsAllowed = col_double(),
      dstYdsAllowed = col_number(),
      points = col_double()
    ),
    skip = 1
  ) %>%
  mutate(pos = 'DST')


proj_K <-
  readr::read_csv(
    "data/raw/FantasyPros_Fantasy_Football_Projections_K.csv",
    col_names = c('Player', 'Team', 'fg', 'fga', 'xp', 'points'),
    col_types = cols(
      Player = col_character(),
      Team = col_character(),
      fg = col_double(),
      fga = col_double(),
      xp = col_double(),
      points = col_double()
    ),
    skip = 1
  ) %>%
  mutate(pos = 'K')

proj_QB <-
  readr::read_csv(
    "data/raw/FantasyPros_Fantasy_Football_Projections_QB.csv",
    col_names = c(
      'Player',
      'Team',
      'passAtt',
      'passComp',
      'passYds',
      'passTds',
      'passInt',
      'rushAtt',
      'rushYds',
      'rushTds',
      'fumbles',
      'points'
    ),
    col_types = cols(
      Player = col_character(),
      Team = col_character(),
      passAtt = col_double(),
      passComp = col_double(),
      passYds = col_number(),
      passTds = col_double(),
      passInt = col_double(),
      rushAtt = col_double(),
      rushYds = col_double(),
      rushTds = col_double(),
      fumbles = col_double(),
      points = col_double()
    ),
    skip = 2
  ) %>%
  mutate(pos = 'QB')

proj_RB <-
  readr::read_csv(
    "data/raw/FantasyPros_Fantasy_Football_Projections_RB.csv",
    col_names = c(
      'Player',
      'Team',
      'rushAtt',
      'rushYds',
      'rushTds',
      'rec',
      'recYds',
      'recTds',
      'fumbles',
      'points'
    ),
    col_types = cols(
      Player = col_character(),
      Team = col_character(),
      rushAtt = col_double(),
      rushYds = col_number(),
      rushTds = col_double(),
      rec = col_double(),
      recYds = col_double(),
      recTds = col_double(),
      fumbles = col_double(),
      points = col_double()
    ),
    skip = 2
  ) %>%
  mutate(pos = 'RB')

proj_TE <-
  readr::read_csv(
    "data/raw/FantasyPros_Fantasy_Football_Projections_TE.csv",
    col_names = c('Player',
                  'Team',
                  'rec',
                  'recYds',
                  'recTds',
                  'fumbles',
                  'points'),
    col_types = cols(
      Player = col_character(),
      Team = col_character(),
      rec = col_double(),
      recYds = col_number(),
      recTds = col_double(),
      fumbles = col_double(),
      points = col_double()
    ),
    skip = 2
  ) %>%
  mutate(pos = 'TE')

proj_WR <-
  readr::read_csv(
    "data/raw/FantasyPros_Fantasy_Football_Projections_WR.csv",
    col_names = c(
      'Player',
      'Team',
      'rec',
      'recYds',
      'recTds',
      'rushAtt',
      'rushYds',
      'rushTds',
      'fumbles',
      'points'
    ),
    col_types = cols(
      Player = col_character(),
      Team = col_character(),
      rec = col_double(),
      recYds = col_number(),
      recTds = col_double(),
      rushAtt = col_double(),
      rushYds = col_double(),
      rushTds = col_double(),
      fumbles = col_double(),
      points = col_double()
    ),
    skip = 2
  ) %>%
  mutate(pos = 'WR')


data_ff_2018 <-
  bind_rows(proj_QB, proj_RB, proj_WR, proj_TE, proj_DST, proj_K) %>%
  mutate_if(.predicate = is.numeric, .funs = funs(replace_na(., replace = 0))) %>%
  mutate(
    points = (passYds / 25) +
      (passTds * 4) +
      (passInt * -2) +
      (rushYds / 10) +
      (rushTds * 6) +
      (fumbles * -3) +
      (rec * 0.5) +
      (recYds / 10) +
      (recTds * 6) +
      (dstSack * 2) +
      (dstInt * 2) +
      (dstFumlRec * 2) +
      (dstTd * 6) +
      (dstSafety * 3) +
      (fg * 3.5) +
      (xp),
    owner = '',
    paid = NA
  ) %>%
  arrange(desc(points)) %>%
  mutate(overallRank = c(1:n())) %>%
  group_by(pos) %>%
  arrange(desc(points)) %>%
  mutate(positionRank = 1:n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(playerId = 1:n()) %>%
  dplyr::select(
    playerId,
    Player,
    Team,
    pos,
    owner,
    paid,
    overallRank,
    positionRank,
    starts_with("pass"),
    starts_with("rush"),
    starts_with("rec"),
    starts_with("dst"),
    fg,
    fga,
    xp,
    points
  ) %>%
  dplyr::mutate(pos_equiv = case_when(pos %in% c('WR', 'TE') ~ 'WR_TE',
                                      TRUE ~ pos))

# qb_replacement <-
#   data_ff_2018[pos == 'QB', sort(points, decreasing = TRUE)[21]]
# 
# rb_replacement <-
#   data_ff_2018[pos == 'RB', sort(points, decreasing = TRUE)[31]]
# 
# wrte_replacement <-
#   data_ff_2018[pos %in% c('WR', 'TE'), sort(points, decreasing = TRUE)[41]]
# 
# k_replacement <-
#   data_ff_2018[pos == 'K', sort(points, decreasing = TRUE)[11]]
# 
# dst_replacement <-
#   data_ff_2018[pos == 'DST', sort(points, decreasing = TRUE)[11]]

numTeams <- 10

# numMainStarters <- 9

#Season
season <- 2018
# weekNo <- 0   # Set weekNo = 0 for seasonal projections

#Roster
numQB <- 2
numRB <- 3
numWR <- 0
numTE <- 0
numW.T <- 4
numW.R <- 0
numW.R.T <- 0
numQ.W.R.T <- 0
numK <- 1
numDEF <- 1
numBench <- 6

numTotalStarters <- sum(c(
  numQB,
  numRB,
  numWR,
  numTE,
  numW.T,
  numW.R,
  numW.R.T,
  numQ.W.R.T,
  numK,
  numDEF
))

numTotalPlayers <- sum(c(numTotalStarters, numBench))

# League settings
leagueCap <- 200
maxCost <- leagueCap - numTotalPlayers

qbStart <- 20
qbBench <- 13
rbStart <- 30
rbBench <- 20
wrStart <- 36
wrBench <- 24
teStart <- 4
teBench <- 3
kStart <- 10
kBench <- 0
dstStart <- 10
dstBench <- 0

.tbl <- data_ff_2018
.tbl[200, 'owner'] <- 'Devon'
.tbl[200, 'paid'] <- 50

data_ff_2018 %>%
  filter(willBeDrafted)

markDrafted <- function(.tbl) {
  
  user_exprs <- enquo(.tbl)
  
  .tbl <- .tbl %>%
    dplyr::mutate(willDraft = owner != '')
  
  starters_QB <- .tbl %>%
    dplyr::filter(pos == 'QB') %>%
    dplyr::arrange(desc(willDraft), desc(points)) %>%
    dplyr::slice(1:20)
  
  starters_RB <- .tbl %>%
    dplyr::filter(pos == 'RB') %>%
    dplyr::arrange(desc(willDraft), desc(points)) %>%
    dplyr::slice(1:30)
  
  starters_WRTE <- .tbl %>%
    dplyr::filter(pos %in% c('WR', 'TE')) %>%
    dplyr::arrange(desc(willDraft), desc(points)) %>%
    dplyr::slice(1:40)
  
  starters_K <- .tbl %>%
    dplyr::filter(pos == 'K') %>%
    dplyr::arrange(desc(willDraft), desc(points)) %>%
    dplyr::slice(1:10)
  
  starters_DST <- .tbl %>%
    dplyr::filter(pos == 'DST') %>%
    dplyr::arrange(desc(willDraft), desc(points)) %>%
    dplyr::slice(1:10)
  
  starters <-
    bind_rows(starters_QB,
              starters_RB,
              starters_WRTE,
              starters_K,
              starters_DST)
  
  bench <- .tbl %>%
    dplyr::anti_join(starters, by = 'playerId') %>%
    dplyr::filter(pos %in% c('QB', 'RB', 'WR', 'TE')) %>%
    dplyr::arrange(desc(willDraft), desc(points)) %>%
    dplyr::slice(1:60)
  
  willBeDrafted <- bind_rows(starters, bench)
  
  .tbl <- .tbl %>%
    mutate(willDraft = playerId %in% willBeDrafted$playerId)
  
  assign(
    rlang::as_string(rlang::get_expr(user_exprs)),
    value = .tbl,
    envir = rlang::get_env(user_exprs)
  )
 
}

markStarters <- function(.tbl) {
  
  user_exprs <- enquo(.tbl)
  
  markDrafted(.tbl)
  
  willBeDrafted <- .tbl %>%
    dplyr::filter(willDraft == TRUE)
  
  starters_QB <- willBeDrafted %>%
    dplyr::filter(pos == 'QB') %>%
    dplyr::arrange(desc(points)) %>%
    dplyr::slice(1:20)
  
  starters_RB <- willBeDrafted %>%
    dplyr::filter(pos == 'RB') %>%
    dplyr::arrange(desc(points)) %>%
    dplyr::slice(1:30)
  
  starters_WRTE <- willBeDrafted %>%
    dplyr::filter(pos %in% c('WR', 'TE')) %>%
    dplyr::arrange(desc(points)) %>%
    dplyr::slice(1:40)
  
  starters_K <- willBeDrafted %>%
    dplyr::filter(pos == 'K') %>%
    dplyr::arrange(desc(points)) %>%
    dplyr::slice(1:10)
  
  starters_DST <- willBeDrafted %>%
    dplyr::filter(pos == 'DST') %>%
    dplyr::arrange(desc(points)) %>%
    dplyr::slice(1:10)
  
  starters <-
    bind_rows(starters_QB,
              starters_RB,
              starters_WRTE,
              starters_K,
              starters_DST)
  
  .tbl <- .tbl %>%
    mutate(willStart = playerId %in% starters$playerId) %>%
    mutate(willBench = willStart != TRUE & willDraft == TRUE)
  
  assign(
    rlang::as_string(rlang::get_expr(user_exprs)),
    value = .tbl,
    envir = rlang::get_env(user_exprs)
  )
  
}

# markDrafted(data_ff_2018)
markStarters(data_ff_2018)

totalMoneyPool <- numTeams * leagueCap

totalMoneySpent <-
  numTeams * numTotalPlayers + sum(data_ff_2018$paid - 1, na.rm = TRUE)

totalMoneyLeft <- totalMoneyPool - totalMoneySpent

data_ff_2018 <- data_ff_2018 %>%
  dplyr::select(-repl_value, -pnts_over_repl) %>%
  dplyr::left_join(
    data_ff_2018 %>%
      filter(owner == '', willStart == FALSE) %>%
      dplyr::group_by(pos_equiv) %>%
      dplyr::summarise(repl_value = max(points)),
    by = "pos_equiv"
  ) %>%
  dplyr::mutate(pnts_over_repl = pmax(points - repl_value, 0))
  
total_points <- sum(data_ff_2018$pnts_over_repl)

costPerPoint <- totalMoneyLeft / total_points

data_ff_2018 <- data_ff_2018 %>%
  dplyr::mutate(value = round(pnts_over_repl * costPerPoint) + 1)

data_ff_2018 %>%
  dplyr::select(Player, Team, pos, value) %>%
  View()
