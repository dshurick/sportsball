
library(tidyverse)
library(magrittr)
library(Rglpk)
library(googlesheets)

worksheet17 <- googlesheets::gs_title("NFL 2017 Expected Wins")
worksheet18 <- googlesheets::gs_title("NFL 2018 Expected Wins")

sheet17 <- gs_read(
  worksheet17,
  ws = "Season2017",
  col_types = cols(
    .default = col_number(),
    away_team = col_character(),
    home_team = col_character()
  )
) %>%
  dplyr::mutate_at(.vars = vars(ends_with("_prob")), .funs = funs(. / 100)) %>%
  dplyr::mutate(
    away_team = factor(away_team),
    home_team = factor(home_team),
    week = factor(week)
  )

sheet18 <- gs_read(
  worksheet18,
  ws = "Season2018",
  col_types = cols(
    .default = col_number(),
    away_team = col_character(),
    home_team = col_character()
  )
) %>%
  dplyr::mutate_at(.vars = vars(ends_with("_prob")), .funs = funs(. / 100)) %>%
  dplyr::mutate(
    away_team = factor(away_team),
    home_team = factor(home_team),
    week = factor(week)
  )

dtf <- dplyr::bind_rows(
  sheet17 %>%
    dplyr::select(
      away_team,
      home_team,
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
    dplyr::filter(!is.na(home_win)),
  sheet18 %>%
    dplyr::select(
      away_team,
      home_team,
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
    dplyr::filter(!is.na(home_win))
) %>%
  dplyr::mutate(five38_home_rating = five38_home_rating + 65)

dtf <- dplyr::bind_rows(
  sheet17 %>%
    dplyr::select(
      away_team,
      home_team,
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
    dplyr::filter(!is.na(home_win))
) %>%
  dplyr::mutate(five38_home_rating = five38_home_rating + 65)

dtf <- dplyr::bind_rows(
  dtf %>%
    dplyr::select(
      five38_home_rating,
      five38_away_rating,
      massey_home_rating,
      massey_away_rating,
      sagarin_home_rating,
      sagarin_away_rating,
      home_win
    ) %>%
    dplyr::mutate(home_adv = 1),
  dtf %>%
    dplyr::select(
      five38_away_rating = five38_home_rating,
      five38_home_rating = five38_away_rating,
      massey_away_rating = massey_home_rating,
      massey_home_rating = massey_away_rating,
      sagarin_away_rating = sagarin_home_rating,
      sagarin_home_rating = sagarin_away_rating,
      home_win
    ) %>%
    dplyr::mutate(
      home_adv = -1,
      home_win = 1 - home_win
    )
)

fit1 <-
  glm(
    as.factor(home_win) ~ home_adv + five38_away_rating + five38_home_rating + massey_away_rating + massey_home_rating + sagarin_away_rating + sagarin_home_rating - 1,
    data = dtf,
    family = binomial()
  )

fit2 <-
  glm(
    as.factor(home_win) ~ five38_away_rating + five38_home_rating + massey_away_rating + massey_home_rating + sagarin_away_rating + sagarin_home_rating - 1,
    data = dtf %>%
      dplyr::filter(home_win == 0.0 | home_win == 1.0),
    family = binomial()
  )

sheet18$custom_home_prob <-
  predict(fit2, sheet18, type = "response")

sheet18$custom_away_prob <- 1 - sheet18$custom_home_prob

gs_edit_cells(
  worksheet18,
  ws = 'Season2018',
  input = sheet18 %>%
    dplyr::select(custom_away_prob, custom_home_prob),
  anchor = "G1",
  col_names = TRUE
)

sheet18 %>%
  dplyr::select(
    week,
    away_team,
    home_team,
    custom_home_prob,
    custom_home_prob_1,
    custom_home_prob_2
  ) %>%
  View()

dtf <- dplyr::bind_rows(
  sheet18 %>%
    dplyr::filter(!is.na(home_win), !is.na(scorex_away_rating)) %>%
    dplyr::select(
      scorex_away_rating,
      scorex_home_rating,
      home_win
    ) %>%
    dplyr::mutate(home_adv = 1),
  sheet18 %>%
    dplyr::filter(!is.na(home_win), !is.na(scorex_away_rating)) %>%
    dplyr::select(
      scorex_home_rating = scorex_away_rating,
      scorex_away_rating = scorex_home_rating,
      home_win
    ) %>%
    dplyr::mutate(
      home_adv = -1,
      home_win = 1 - home_win
    )
)

scorex_fit <-
  glm(
    as.factor(home_win) ~ home_adv + scorex_away_rating + scorex_home_rating - 1,
    data = dtf,
    family = binomial()
  )

sheet18$scorex_home_prob <-
  predict(scorex_fit, sheet18, type = "response")
