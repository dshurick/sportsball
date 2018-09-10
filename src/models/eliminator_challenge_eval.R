
library(tidyverse)
library(magrittr)
library(Rglpk)


exp_wins <- googlesheets::gs_title("NFL 2017 Expected Wins") %>%
  googlesheets::gs_read(ws = "Season2017") %>%
  dplyr::mutate(
    Away = factor(Away),
    Home = factor(Home),
    Week = factor(Week),
    
    SgrnAwayWinProb = as.numeric(sub("%", "", SgrnAwayWinProb)) / 100,
    SgrnHomeWinProb = as.numeric(sub("%", "", SgrnHomeWinProb)) / 100,
    
    MassAwayWinProb = as.numeric(sub("%", "", MassAwayWinProb)) / 100,
    MassHomeWinProb = as.numeric(sub("%", "", MassHomeWinProb)) / 100
  )

dtf <- exp_wins %>%
  dplyr::filter(!is.na(HomeWin))

dtf <- dplyr::bind_rows(
  dtf %>%
    dplyr::mutate(HomeAdv = 1),
  dtf %>%
    dplyr::select(
      AwayRating = HomeRating,
      HomeRating = AwayRating,
      MassAway = MassHome,
      MassHome = MassAway,
      SgrnAway = SgrnHome,
      SgrnHome = SgrnAway,
      HomeWin
    ) %>%
    dplyr::mutate(HomeAdv = -1,
                  HomeWin = 1 - HomeWin)
)

fit <-
  glm(
    as.factor(HomeWin) ~ HomeAdv + AwayRating + HomeRating + MassAway + MassHome + SgrnAway + SgrnHome - 1,
    data = dtf,
    family = binomial()
  )
