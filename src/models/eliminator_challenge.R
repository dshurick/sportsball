
library(magrittr)
library(Rglpk)

nfl_2018 <- googlesheets::gs_title("NFL 2018 Expected Wins")

exp_wins <- gs_read(
  nfl_2018,
  ws = 'Season2018',
  col_types = cols(
    .default = col_number(),
    away_team = col_factor(levels = NULL),
    home_team = col_factor(levels = NULL),
    week = col_factor(levels = NULL)
  )
) %>%
  dplyr::mutate_at(.vars = vars(ends_with("_prob")), .funs = funs(. / 100)) %>%
  dplyr::mutate(away_team = factor(away_team, levels = sort(levels(away_team))),
                home_team = factor(home_team, levels = sort(levels(home_team))))

exp_wins$WinProb <- predict(fit, exp_wins, type = "response")

googlesheets::gs_edit_cells(nfl_2018,
                            ws = "Season2018",
                            input = exp_wins %>%
                              dplyr::select(WinProb),
                            col_names = FALSE,
                            anchor = 'G2')

# picked_teams <- c("1" = "Buffalo", "2" = "Seattle")
picked_teams <- c()

X1 <- Matrix::sparse.model.matrix(~ away_team - 1, data = exp_wins)
X2 <- Matrix::sparse.model.matrix(~ home_team - 1, data = exp_wins)
colnames(X1) <- NULL
colnames(X2) <- NULL

week <- Matrix::sparse.model.matrix(~ week - 1, data = exp_wins)

mat <- Matrix::t(cbind(rbind(X1, X2),
                       rbind(week, week)))

obj <- log(c(exp_wins$massey_away_prob, exp_wins$massey_home_prob))

picked_indx <- which(levels(exp_wins$away_team) %in% picked_teams)

dir <- c(rep("<=", 32), rep("==", 17))
dir[picked_indx] <- "=="

rhs <- rep(1, 49)
rhs[picked_indx] <- 0

result <- Rglpk_solve_LP(
  obj = obj,
  mat = mat,
  dir = dir,
  rhs = rhs,
  types = rep("I", 48),
  max = TRUE
)

awayind <- which(result$solution[1:NROW(exp_wins)] == 1)
homeind <- which(result$solution[-c(1:NROW(exp_wins))] == 1)

exp_wins[awayind,] %>% dplyr::select(week, away_team, home_team, massey_away_prob, massey_home_prob)
exp_wins[homeind,] %>% dplyr::select(week, away_team, home_team, massey_away_prob, massey_home_prob)
