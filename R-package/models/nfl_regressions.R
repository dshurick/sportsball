



library(tidyverse)
library(magrittr)
library(Rglpk)
library(googlesheets)


load_data <- function() {
  worksheet17 <- googlesheets::gs_title("NFL 2017 Expected Wins")
  worksheet18 <- googlesheets::gs_title("NFL 2018 Expected Wins")

  # Read from 2017 Worksheet

  sheet17 <- gs_read(
    worksheet17,
    ws = "Season2017",
    col_types = cols(
      .default = col_number(),
      week = col_character(),
      away_team = col_character(),
      home_team = col_character()
    )
  ) %>%
    dplyr::mutate_at(.vars = vars(ends_with("_prob")), .funs = funs(. / 100)) %>%
    dplyr::mutate(
      away_team = factor(away_team),
      home_team = factor(home_team)
    )

  # Read from 2018 Worksheet

  sheet18 <- gs_read(
    worksheet18,
    ws = "Season2018",
    col_types = cols(
      .default = col_number(),
      week = col_character(),
      away_team = col_character(),
      home_team = col_character()
    )
  ) %>%
    dplyr::mutate_at(.vars = vars(ends_with("_prob")), .funs = funs(. / 100)) %>%
    dplyr::mutate(
      away_team = factor(away_team),
      home_team = factor(home_team)
    )

  # bind 2017 and 2018 together
  dtf <- dplyr::bind_rows(
    sheet17 %>%
      dplyr::select(
        week,
        away_team,
        home_team,
        home_adv,
        five38_away_rating,
        five38_home_rating,
        sagarin_away_rating,
        sagarin_home_rating,
        massey_away_rating,
        massey_home_rating,
        score_away,
        score_home,
        home_win
      ) %>%
      dplyr::mutate(season = 2017),
    sheet18 %>%
      dplyr::select(
        week,
        away_team,
        home_team,
        home_adv,
        five38_away_rating,
        five38_home_rating,
        sagarin_away_rating,
        sagarin_home_rating,
        massey_away_rating,
        massey_home_rating,
        scorex_away_rating,
        scorex_home_rating,
        vegas_spread,
        score_away,
        score_home,
        home_win
      ) %>%
      dplyr::mutate(
        five38_home_rating = five38_home_rating + 65,
        season = 2018
      )
  ) %>%
    dplyr::mutate(week = factor(week))

  return(dtf)
}

dtf <- load_data()


fit_scorex <- function(.dtf) {
  fitdf <- dplyr::bind_rows(
    .dtf %>%
      dplyr::select(
        scorex_away_rating,
        scorex_home_rating,
        home_win,
        home_adv
      ),
    .dtf %>%
      dplyr::select(
        scorex_home_rating = scorex_away_rating,
        scorex_away_rating = scorex_home_rating,
        home_win,
        home_adv
      ) %>%
      dplyr::mutate(
        home_adv = -home_adv,
        home_win = 1 - home_win
      )
  ) %>%
    dplyr::filter(home_win == 0.0 | home_win == 1.0)

  fit <- glm(
    as.factor(home_win) ~ scorex_away_rating + scorex_home_rating + home_adv - 1,
    data = fitdf %>%
      dplyr::filter(home_win == 0.0 | home_win == 1.0),
    family = binomial()
  )

  return(fit)
}

fit_vegas <- function(.dtf) {
  fitdf <- dplyr::bind_rows(
    .dtf %>%
      dplyr::filter(!is.na(home_win), !is.na(vegas_spread)) %>%
      dplyr::select(
        vegas_spread,
        home_win,
        home_adv
      ) %>%
      dplyr::mutate(home_adv = 1),
    .dtf %>%
      dplyr::filter(!is.na(home_win), !is.na(vegas_spread)) %>%
      dplyr::select(
        vegas_spread,
        home_win,
        home_adv
      ) %>%
      dplyr::mutate(
        home_adv = -home_adv,
        vegas_spread = -vegas_spread,
        home_win = 1 - home_win
      )
  ) %>%
    dplyr::filter(home_win == 0.0 | home_win == 1.0)

  fit <- glm(
    as.factor(home_win) ~ vegas_spread - 1,
    data = fitdf %>%
      dplyr::filter(home_win == 0.0 | home_win == 1.0),
    family = binomial()
  )

  return(fit)
}

vegas_forecast <- function(.dtf) {
  fitdf <- dplyr::bind_rows(
    .dtf %>%
      dplyr::filter(!is.na(vegas_spread)) %>%
      dplyr::select(
        away_team,
        home_team,
        vegas_spread,
        home_adv
      ),
    .dtf %>%
      dplyr::filter(!is.na(vegas_spread)) %>%
      dplyr::select(
        away_team = home_team,
        home_team = away_team,
        vegas_spread,
        home_adv
      ) %>%
      dplyr::mutate(
        home_adv = -home_adv,
        vegas_spread = -vegas_spread
      )
  )

  X <- sparse.model.matrix(
    vegas_spread ~ away_team + home_team + home_adv,
    data = fitdf,
    contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
  )

  fit <- lm(
    vegas_spread ~ away_team + home_team + home_adv,
    data = fitdf,
    contrasts = list(away_team = "contr.sum", home_team = "contr.sum")
  )

  return(fit)
}




fit_poisson <- function(.dtf) {
  fitdf <- dplyr::bind_rows(
    .dtf %>%
      dplyr::filter(!is.na(home_win)) %>%
      dplyr::select(
        home_adv,
        defense = away_team,
        offense = home_team,
        points_scored = score_home
      ),
    .dtf %>%
      dplyr::filter(!is.na(home_win)) %>%
      dplyr::select(
        home_adv,
        defense = home_team,
        offense = away_team,
        points_scored = score_away
      ) %>%
      dplyr::mutate(home_adv = -home_adv)
  )

  X <-
    sparse.model.matrix(points_scored ~ defense + offense + home_adv,
      data = fitdf
    )
  fit <-
    cv.glmnet(
      x = X[, -1],
      y = dtf$points_scored,
      family = "poisson",
      nfolds = nrow(X),
      alpha = 0
    )
  return(fit)
}

fit <- dtf %>%
  dplyr::filter(season == 2018) %>%
  fit_scorex()

fit <- dtf %>%
  dplyr::filter(season == 2018) %>%
  fit_vegas()

fit <- dtf %>%
  dplyr::filter(season == 2018) %>%
  vegas_forecast()
