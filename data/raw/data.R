
source('./R-package/utils.R')

library(readr)
library(dplyr)

nfl_games <-
  readr::read_csv(
    'https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv',
    col_types = cols(
      playoff = col_factor(levels = NULL),
      team1 = col_factor(levels = NULL),
      team2 = col_factor(levels = NULL)
    )
  ) %>%
  group_by(season) %>%
  mutate(
    week = ceiling((as.numeric(date - min(
      date
    )) + 1) / 7),
    gameid = sprintf('%d_%02d_%s_%s', season, week, team1, team2)
  ) %>%
  ungroup()

fitdf <- dplyr::bind_rows(
  nfl_games %>%
    dplyr::filter(!is.na(score1)) %>%
    dplyr::select(season, week, gameid,
                  away_team = team1,
                  home_team = team2,
                  points = score1,
                  neutral) %>%
    dplyr::mutate(home_adv = neutral - 1),
  nfl_games %>%
    dplyr::filter(!is.na(score1)) %>%
    dplyr::select(season, week, gameid,
                  away_team = team2,
                  home_team = team1,
                  points = score2,
                  neutral) %>%
    dplyr::mutate(home_adv = 1 - neutral)
) %>%
  filter(season >= 1950) %>%
  mutate(
    home_adv2 = factor(home_adv == 1, labels = c("No", "Yes")),
    away_team = factor(away_team),
    home_team = factor(home_team),
    season = factor(season)
  )

foldid <-
  caret::groupKFold(fitdf$gameid, k = 20)

fitControl <- caret::trainControl(
  method = "cv",
  index = foldid,
  search = "grid",
  allowParallel = TRUE
)

glmnetGrid <-
  expand.grid(lambda = exp(seq(-8, 0, length.out = 11)),
              alpha = seq(0, 1, length.out = 11))


X <- Matrix::sparse.model.matrix(
  points ~ season * (away_team + home_team + home_adv2),
  data = fitdf,
  contrasts.arg = list(
    away_team = "contr.sum",
    home_team = "contr.sum",
    season = "contr.sum"
  )
)[, -1]

poisson_fit <-
  caret::train(
    x = X,
    y = fitdf$points,
    method = "glmnet",
    family = "poisson",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    standardize = FALSE,
    intercept = TRUE
  )
