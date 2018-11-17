


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
    dplyr::select(
      week,
      away_team,
      home_team,
      home_adv,
      score_away,
      score_home,
      home_win,
      vegas_spread
    ) %>%
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
      home_win,
      vegas_spread
    ) %>%
    dplyr::mutate(
      gameid = 1:n(),
      home_adv = -home_adv,
      home_win = 1 - home_win,
      vegas_spread = -vegas_spread
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
  # dplyr::filter(home_win == 1 | home_win == 0) %>%
  dplyr::mutate(
    home_hfa = ifelse(home_adv == 1, home_hfa, 0),
    away_hfa = ifelse(home_adv == -1, away_hfa, 0),
    home_adv = factor(
      home_adv,
      levels = c(0, 1,-1),
      labels = c("neutral", "home",  "away")
    ),
    agnst_the_sprd = factor(
      case_when(
        score_home + vegas_spread > score_away ~ "Home",
        score_home + vegas_spread < score_away ~ "Away"
      )
    )
  )


fitdf <- dtf %>%
  dplyr::filter(!is.na(score_away))

foldid <-
  groupKFold(fitdf$gameid, k = length(unique(fitdf$gameid)))

fitControl <- trainControl(
  method = "cv",
  index = foldid,
  search = "grid",
  allowParallel = TRUE
)

glmnetGrid <-
  expand.grid(lambda = exp(seq(0.048, 0.278, length.out = 11)),
              alpha = seq(0.2, 0.36, length.out = 11))

spread_forecast_fit <-
  train(
    (score_away - score_home) ~ home_adv + home_adv:(away_hfa + home_hfa) + away_off + away_def + home_off + home_def,
    data = fitdf,
    method = "glmnet",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    intercept = FALSE
  )

spread_forecast_fit

min(spread_forecast_fit$results$RMSE)

cffnts <- coef(spread_forecast_fit$finalModel, s = 1.261119729)[, 1]

mean(abs(cffnts[c('home_advhome', 'home_advaway')]))
mean(abs(cffnts[c('away_off', 'home_off')]))
mean(abs(cffnts[c('away_def', 'home_def')]))
mean(abs(cffnts[c('home_advaway:away_hfa', 'home_advhome:home_hfa')]))

fitdf$predicted_spread <- predict(spread_forecast_fit, newdata = fitdf)


fitdf <- fitdf %>%
  tidyr::drop_na()

foldid <-
  groupKFold(fitdf$gameid, k = length(unique(fitdf$gameid)))

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
  expand.grid(lambda = exp(seq(-4.56, -3.6, length.out = 11)),
              alpha = 0)

beat_the_spread <-
  train(
    agnst_the_sprd ~ predicted_spread + vegas_spread,
    data = fitdf,
    method = "glmnet",
    metric = "logLoss",
    family = "binomial",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    intercept = FALSE
  )

coef(beat_the_spread$finalModel, s = 0.01690746565)

xxx <- dtf %>%
  dplyr::filter(week == "7") %>%
  dplyr::select(away_team, home_adv, home_team, vegas_spread, away_rating:home_hfa) %>%
  tidyr::drop_na()

xxx$predicted_spread <- predict(spread_forecast_fit, newdata = xxx)
xxx$chance_agnst_spread <- predict(beat_the_spread, newdata = xxx, type = "prob")


dtf %>%
  dplyr::filter((score_away - score_home) != vegas_spread) %>%
  dplyr::select(score_away, score_home, vegas_spread, predicted_spread) %>%
  dplyr::filter((score_away - score_home < vegas_spread) &
                  (predicted_spread < vegas_spread))

dtf %>%
  dplyr::filter((score_away - score_home) != vegas_spread) %>%
  dplyr::select(score_away, score_home, vegas_spread, predicted_spread) %>%
  dplyr::filter((score_away - score_home > vegas_spread) &
                  (predicted_spread > vegas_spread))

dtf %>%
  dplyr::filter((score_away - score_home) != vegas_spread) %>%
  dplyr::select(score_away, score_home, vegas_spread, predicted_spread) %>%
  dplyr::filter((score_away - score_home < vegas_spread) &
                  (predicted_spread > vegas_spread))

dtf %>%
  dplyr::filter((score_away - score_home) != vegas_spread) %>%
  dplyr::select(score_away, score_home, vegas_spread, predicted_spread) %>%
  dplyr::filter((score_away - score_home > vegas_spread) &
                  (predicted_spread < vegas_spread))



sum(with(
  dtf,
  (score_away - score_home < vegas_spread) &
    (predicted_spread < vegas_spread)
))
sum(with(
  dtf,
  (score_away - score_home > vegas_spread) &
    (predicted_spread > vegas_spread)
))
