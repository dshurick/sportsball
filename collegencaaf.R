
teams_ncaaf <- 'https://www.masseyratings.com/scores.php?s=300937&sub=11604&all=1&mode=3&format=2'
teams_nba <- 'https://www.masseyratings.com/scores.php?s=305191&sub=305191&all=1&mode=3&format=2'
teams_ncaab <- 'https://www.masseyratings.com/scores.php?s=305972&sub=11590&all=1&mode=3&format=2'

teams <-
  readr::read_csv(teams_ncaab,
                  col_names = c('team_id', 'team_name'))

data_ncaaf <- 'https://www.masseyratings.com/scores.php?s=300937&sub=11604&all=1&mode=3&format=1'
data_nba <- 'https://www.masseyratings.com/scores.php?s=305191&sub=305191&all=1&mode=3&format=1'
data_ncaab <- 'https://www.masseyratings.com/scores.php?s=305972&sub=11590&all=1&mode=3&format=1'

data <-
  readr::read_fwf(
    'https://www.masseyratings.com/scores.php?s=305972&sub=11590&all=1&mode=3&format=0',
    readr::fwf_widths(
      widths = c(10, 2, 24, 3, 2, 24, 3, 3),
      col_names = c(
        "date",
        "loc1",
        "team1",
        "score1",
        "loc2",
        "team2",
        "score2",
        "overtime"
      )
    ),
    skip = 39
  )

data <-
  readr::read_csv(
    data_ncaab,
    col_names = c(
      'dayid',
      'date',
      'teamid1',
      'homeadv1',
      'score1',
      'teamid2',
      'homeadv2',
      'score2'
    ),
    col_types = cols(dayid = col_integer(),
                     date = col_date(format = "%Y%m%d"))
  ) %>%
  dplyr::left_join(teams %>%
                     dplyr::rename(away_team = team_name),
                   by = c('teamid1' = 'team_id')) %>%
  dplyr::left_join(teams %>%
                     dplyr::rename(home_team = team_name),
                   by = c('teamid2' = 'team_id'))


fitdf <-
  dplyr::bind_rows(
    data %>%
      dplyr::mutate(gameid = 1:n()),
    data %>% dplyr::select(
      dayid,
      date,
      teamid2 = teamid1,
      homeadv2 = homeadv1,
      score2 = score1,
      away_team = home_team,
      teamid1 = teamid2,
      homeadv1 = homeadv2,
      score1 = score2,
      home_team = away_team
    ) %>%
      dplyr::mutate(gameid = 1:n())
  ) %>%
  dplyr::arrange(gameid) %>%
  dplyr::mutate(weeknum = lubridate::week(date),
                home_adv = factor(homeadv2 == 1, labels = c("No", "Yes"))) %>%
  dplyr::select(dayid, date, weeknum, away_team, home_team, home_adv, score2) %>%
  dplyr::mutate_at(.vars = vars(away_team, home_team), .funs = factor)

foldid <-
  caret::groupKFold(fitdf$weeknum, k = length(unique(fitdf$weeknum)))

fitControl <- caret::trainControl(
  method = "cv",
  index = foldid,
  search = "grid",
  allowParallel = TRUE
)

glmnetGrid <-
  expand.grid(lambda = exp(seq(-8.9, -6.4, length.out = 11)),
              alpha = seq(0.2, 0.4, length.out = 11))

X <- Matrix::sparse.model.matrix(
  score2 ~ away_team + home_team + home_adv,
  data = fitdf,
  contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
)[,-1]

poisson_fit <-
  caret::train(
    x = X,
    y = fitdf$score2,
    method = "glmnet",
    family = "poisson",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    standardize = FALSE,
    intercept = TRUE
  )

plot(poisson_fit)
poisson_fit

coef(poisson_fit$finalModel, s = 0.0006112527611)[, 1]


newdf <- fitdf %>%
  dplyr::distinct(home_team, .keep_all = TRUE) %>%
  dplyr::mutate(away_team = factor("Abilene_Chr", levels = levels(home_team)),
                home_adv = factor('No', levels = c('No', 'Yes'))) %>%
  dplyr::arrange(home_team) %>%
  dplyr::select(away_team, home_team, home_adv)

X_new <- Matrix::sparse.model.matrix(
  ~ away_team + home_team + home_adv,
  data = newdf,
  contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
)[, -1]

X_new[, 1] <- 0

newdf$offense <- exp(predict(poisson_fit, newdata = X_new))

X_new <- Matrix::sparse.model.matrix(
  ~ away_team + home_team + home_adv,
  data = fitdf %>%
    dplyr::distinct(away_team, .keep_all = TRUE) %>%
    dplyr::mutate(
      home_team = factor("Abilene_Chr", levels = levels(home_team)),
      home_adv = factor('No', levels = c('No', 'Yes'))
    ) %>%
    dplyr::arrange(away_team) %>%
    dplyr::select(away_team, home_team, home_adv),
  contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
)[, -1]

X_new[, 30] <- 0

newdf$defense <- exp(predict(poisson_fit, newdata = X_new))

newdf$rating <- with(newdf, offense - defense)

