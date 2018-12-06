
library(tidyverse)
library(caret)
library(doParallel)
library(ggplot2)
library(Matrix)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

load_data <- function() {
  
  worksheet17 <- googlesheets::gs_title("NFL 2017 Expected Wins")
  worksheet18 <- googlesheets::gs_title("NFL 2018 Expected Wins")
  
  # Read from 2017 Worksheet
  
  sheet17 <- googlesheets::gs_read(
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
    dplyr::mutate(away_team = factor(away_team),
                  home_team = factor(home_team))
  
  # Read from 2018 Worksheet
  
  sheet18 <- googlesheets::gs_read(
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
    dplyr::mutate(away_team = factor(away_team),
                  home_team = factor(home_team))
  
  # bind 2017 and 2018 together
  dtf <- dplyr::bind_rows(
    sheet18 %>%
      dplyr::select(
        week,
        ots,
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
        vegas_moneyline_away,
        vegas_moneyline_home,
        vegas_spread,
        score_away,
        score_home,
        home_win
      ) %>%
      dplyr::mutate(# five38_home_rating = five38_home_rating + 65,
        season = 2018)
  ) %>%
    dplyr::mutate(
      week = as.integer(week),
      season = factor(season),
      gameid = 1:n()
    )
  
  return(dtf)
}
