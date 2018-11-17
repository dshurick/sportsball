
library(magrittr)
library(Rglpk)
library(googlesheets)
library(tidyverse)

nfl_2018 <- googlesheets::gs_title("NFL 2018 Expected Wins")

exp_wins <- gs_read(
  nfl_2018,
  ws = "Season2018",
  col_types = cols(
    .default = col_number(),
    away_team = col_character(),
    home_team = col_character()
  )
) %>%
  dplyr::filter(week > 8) %>%
  dplyr::mutate_at(.vars = vars(ends_with("_prob")), .funs = funs(. / 100)) %>%
  dplyr::mutate(
    away_team = factor(away_team),
    home_team = factor(home_team),
    week = factor(week)
  )

# picked_teams <- c("1" = "Detroit", "2" = "New Orleans")


prep_MILP <- function(df, picked_teams = c()) {
  X_away_teams <-
    Matrix::sparse.model.matrix(~away_team - 1, data = df)
  X_home_teams <-
    Matrix::sparse.model.matrix(~home_team - 1, data = df)
  colnames(X_away_teams) <- NULL
  colnames(X_home_teams) <- NULL

  X_week <- Matrix::sparse.model.matrix(~week - 1, data = df)

  Nteams <- ncol(X_away_teams)
  Nweeks <- ncol(X_week)

  # mat must be a numeric vector or a (sparse) matrix of constraint coefficients
  mat <- Matrix::t(cbind(
    rbind(X_away_teams, X_home_teams),
    rbind(X_week, X_week)
  ))
  obj <- log(c(exp_wins$vegas_away_prob, exp_wins$vegas_home_prob))

  picked_indx <- which(levels(exp_wins$away_team) %in% picked_teams)

  dir <- c(rep("<=", Nteams), rep("==", Nweeks))
  dir[picked_indx] <- "=="

  rhs <- rep(1, length(dir))
  rhs[picked_indx] <- 0

  types <- rep("I", length(dir))

  return(list(mat = mat, dir = dir, rhs = rhs, types = types))
}

milp_args <-
  prep_MILP(
    exp_wins,
    picked_teams = c(
      "1" = "Detroit",
      "2" = "New Orleans",
      "3" = "Minnesota",
      "4" = "L.A. Chargers",
      "5" = "New England",
      "6" = "Green Bay",
      "7" = "Indianapolis",
      "8" = "Pittsburgh"
    )
  )
obj <- log(c(exp_wins$vegas_away_prob, exp_wins$vegas_home_prob))

result <- Rglpk_solve_LP(
  obj = obj,
  mat = milp_args$mat,
  dir = milp_args$dir,
  rhs = milp_args$rhs,
  types = milp_args$types,
  max = TRUE
)

awayind <- which(result$solution[1:NROW(exp_wins)] == 1)
homeind <- which(result$solution[-c(1:NROW(exp_wins))] == 1)

exp_wins[awayind, ] %>% dplyr::select(week, away_team, home_team, vegas_away_prob, vegas_home_prob)
exp_wins[homeind, ] %>% dplyr::select(week, away_team, home_team, vegas_away_prob, vegas_home_prob)

exp(result$optimum)
