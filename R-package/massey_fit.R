
source('./R-package/utils.R')

worksheet18 <- googlesheets::gs_title("NFL 2018 Expected Wins")

sheet18 <- googlesheets::gs_read(
  worksheet18,
  ws = "Season2018",
  col_types = cols(
    .default = col_number(),
    week = col_integer(),
    away_team = col_character(),
    home_team = col_character()
  )
) %>%
  dplyr::mutate_at(.vars = vars(ends_with("_prob")), .funs = funs(. / 100)) %>%
  dplyr::mutate(away_team = factor(away_team),
                home_team = factor(home_team))

massey_ratings <- googlesheets::gs_read(worksheet18,
                                        ws = "massey") %>%
  dplyr::mutate(team = factor(team))

dtf <- dplyr::bind_rows(
  sheet18 %>%
    dplyr::select(week,
                  away_team,
                  home_team,
                  home_adv,
                  score_away,
                  score_home,
                  home_win) %>%
    dplyr::mutate(gameid = 1:n()) %>%
    dplyr::left_join(
      massey_ratings %>%
        dplyr::mutate_at(.vars = vars(rating:hfa), .funs = funs(. * 1)) %>%
        dplyr::rename_at(.vars = vars(-week),
                         .funs = funs(sprintf("away_%s", .)))
    ) %>%
    dplyr::left_join(massey_ratings %>%
                       dplyr::rename_at(
                         .vars = vars(-week),
                         .funs = funs(sprintf("home_%s", .))
                       )),
  sheet18 %>%
    dplyr::select(
      week,
      away_team = home_team,
      home_team = away_team,
      home_adv,
      score_away = score_home,
      score_home = score_away,
      home_win
    ) %>%
    dplyr::mutate(
      gameid = 1:n(),
      home_adv = -home_adv,
      home_win = 1 - home_win
    ) %>%
    dplyr::left_join(massey_ratings %>%
                       dplyr::rename_at(
                         .vars = vars(-week),
                         .funs = funs(sprintf("away_%s", .))
                       )) %>%
    dplyr::left_join(
      massey_ratings %>%
        dplyr::mutate_at(.vars = vars(rating:hfa), .funs = funs(. * 1)) %>%
        dplyr::rename_at(.vars = vars(-week),
                         .funs = funs(sprintf("home_%s", .)))
    )
) %>%
  dplyr::filter(home_win == 1 | home_win == 0) %>%
  dplyr::mutate(
    home_hfa = ifelse(home_adv == 1, home_hfa, 0),
    away_hfa = ifelse(home_adv == -1, away_hfa, 0),
    home_adv = factor(
      home_adv,
      levels = c(0, 1,-1),
      labels = c("neutral", "home",  "away")
    )
  )

foldid <-
  groupKFold(dtf$gameid, k = length(unique(dtf$gameid)))

fitControl <- trainControl(
  method = "cv",
  index = foldid,
  search = "grid",
  classProbs = TRUE,
  savePredictions = TRUE,
  summaryFunction = mnLogLoss,
  allowParallel = TRUE
)

glmnetGrid <-
  expand.grid(lambda = exp(seq(-4.26712, -3.73368, length.out = 11)),
              alpha = seq(0, 0.16, length.out = 11))

X <-
  Matrix::sparse.model.matrix(~ home_adv + home_adv:(away_hfa + home_hfa) + away_off + away_def + home_off + home_def,
                              data = dtf)

vegas_forecast_fit <-
  train(
    x = X,
    y = factor(dtf$home_win, labels = c("Away", "Home")),
    method = "glmnet",
    metric = "logLoss",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    intercept = FALSE
  )

plot(vegas_forecast_fit)
vegas_forecast_fit

min(vegas_forecast_fit$results$logLoss)
# 0.64601096265327

cffnts <- coef(vegas_forecast_fit$finalModel, s = 0.01931147114)[, 1]

mean(abs(cffnts[c('home_advhome', 'home_advaway')]))
mean(abs(cffnts[c('away_off', 'home_off')]))
mean(abs(cffnts[c('away_def', 'home_def')]))
mean(abs(cffnts[c('home_advaway:away_hfa', 'home_advhome:home_hfa')]))
