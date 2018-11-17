
dtf <- load_data()

# dtf <- dtf %>%
#   dplyr::mutate(
#     vegas_moneyline_odds_away = case_when(
#       vegas_moneyline_away < 0 ~ gtools::logit((-vegas_moneyline_away) / (-vegas_moneyline_away + 100)),
#       vegas_moneyline_away > 0 ~ gtools::logit(100 /
#                                     (vegas_moneyline_away + 100))
#     ),
#     vegas_moneyline_odds_home = case_when(
#       vegas_moneyline_home < 0 ~ gtools::logit((-vegas_moneyline_home) / (-vegas_moneyline_home + 100)),
#       vegas_moneyline_home > 0 ~ gtools::logit(100 /
#                                     (vegas_moneyline_home + 100))
#     )
#   )

customfit <- function(dtf) {
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::select(
        gameid,
        week,
        season,
        away_team,
        home_team,
        sagarin_away_rating,
        sagarin_home_rating,
        massey_away_rating,
        massey_home_rating,
        five38_away_rating,
        five38_home_rating,
        home_adv,
        home_win
      ),
    dtf %>%
      dplyr::select(
        gameid,
        week,
        season,
        away_team = home_team,
        home_team = away_team,
        sagarin_away_rating = sagarin_home_rating,
        sagarin_home_rating = sagarin_away_rating,
        massey_away_rating = massey_home_rating,
        massey_home_rating = massey_away_rating,
        five38_away_rating = five38_home_rating,
        five38_home_rating = five38_away_rating,
        home_adv,
        home_win
      ) %>%
      dplyr::mutate(home_adv = -home_adv,
                    home_win = 1 - home_win)
  ) %>%
    tidyr::drop_na() %>%
    dplyr::filter(home_win == 0 |
                    home_win == 1) %>%
    dplyr::mutate(home_win = factor(home_win, labels = c("Away", "Home")))
  
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
    expand.grid(lambda = exp(seq(-6.4, -4.2, length.out = 11)),
                alpha = seq(0.28, 0.42, length.out = 11))
  
  custom_fit <-
    train(
      home_win ~ season * (
        home_adv +
          sagarin_away_rating +
          sagarin_home_rating +
          massey_away_rating +
          massey_home_rating +
          five38_away_rating +
          five38_home_rating
      ),
      data = fitdf,
      method = "glmnet",
      metric = "logLoss",
      trControl = fitControl,
      tuneGrid = glmnetGrid,
      intercept = FALSE
    )
  
  min(custom_fit$results$logLoss)
  formatC(coef(custom_fit$finalModel, s = 0.004991594)[, 1], format = "e", digits = 10)
  
  dfvegas$pred <-
    predict(vegas_fit, newdata = dfvegas, type = "prob")[, 2]
}

five38fit <- function(dtf) {
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::select(
        gameid,
        season,
        away_team,
        home_team,
        # sagarin_away_rating,
        # sagarin_home_rating,
        # massey_away_rating,
        # massey_home_rating,
        five38_away_rating,
        five38_home_rating,
        home_adv,
        home_win
      ),
    dtf %>%
      dplyr::select(
        gameid,
        season,
        away_team = home_team,
        home_team = away_team,
        # sagarin_away_rating = sagarin_home_rating,
        # sagarin_home_rating = sagarin_away_rating,
        # massey_away_rating = massey_home_rating,
        # massey_home_rating = massey_away_rating,
        five38_away_rating = five38_home_rating,
        five38_home_rating = five38_away_rating,
        home_adv,
        home_win
      ) %>%
      dplyr::mutate(home_adv = -home_adv,
                    home_win = 1 - home_win)
  ) %>%
    tidyr::drop_na() %>%
    dplyr::filter(home_win == 0 |
                    home_win == 1) %>%
    dplyr::mutate(home_win = factor(home_win, labels = c("Away", "Home")))
  
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
    expand.grid(lambda = exp(seq(-6, -1.6, length.out = 121)),
                alpha = 0)
  
  custom_fit <-
    train(
      home_win ~ season * (home_adv +
                             five38_away_rating +
                             five38_home_rating) - season,
      data = fitdf,
      method = "glmnet",
      metric = "logLoss",
      trControl = fitControl,
      tuneGrid = glmnetGrid,
      intercept = FALSE
    )
  
  plot(custom_fit)
  
  min(custom_fit$results$logLoss)
  
  coef(custom_fit$finalModel, s = 0.02787569826)[, 1]
  
  fitdf$pred <-
    predict(custom_fit, newdata = fitdf, type = "prob")[, 2]
}

sagarinfit <- function(dtf) {
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::select(
        gameid,
        season,
        away_team,
        home_team,
        sagarin_away_rating,
        sagarin_home_rating,
        # massey_away_rating,
        # massey_home_rating,
        # five38_away_rating,
        # five38_home_rating,
        home_adv,
        home_win
      ),
    dtf %>%
      dplyr::select(
        gameid,
        season,
        away_team = home_team,
        home_team = away_team,
        sagarin_away_rating = sagarin_home_rating,
        sagarin_home_rating = sagarin_away_rating,
        # massey_away_rating = massey_home_rating,
        # massey_home_rating = massey_away_rating,
        # five38_away_rating = five38_home_rating,
        # five38_home_rating = five38_away_rating,
        home_adv,
        home_win
      ) %>%
      dplyr::mutate(home_adv = -home_adv,
                    home_win = 1 - home_win)
  ) %>%
    tidyr::drop_na() %>%
    dplyr::filter(home_win == 0 |
                    home_win == 1, season == '2018') %>%
    dplyr::mutate(home_win = factor(home_win, labels = c("Away", "Home")))
  
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
    expand.grid(lambda = exp(seq(-6, -2, length.out = 121)),
                alpha = 0)
  
  sagarin_fit <-
    train(
      home_win ~ home_adv +
          sagarin_away_rating +
          sagarin_home_rating,
      data = fitdf,
      method = "glmnet",
      metric = "logLoss",
      trControl = fitControl,
      tuneGrid = glmnetGrid,
      intercept = FALSE
    )
  
  coef(sagarin_fit$finalModel, s = 0.03450446073)[, 1]
  
  dfvegas$pred <-
    predict(vegas_fit, newdata = dfvegas, type = "prob")[, 2]
}

masseyfit <- function(dtf) {
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::select(
        gameid,
        season,
        away_team,
        home_team,
        massey_away_rating,
        massey_home_rating,
        home_adv,
        home_win
      ),
    dtf %>%
      dplyr::select(
        gameid,
        season,
        away_team = home_team,
        home_team = away_team,
        massey_away_rating = massey_home_rating,
        massey_home_rating = massey_away_rating,
        home_adv,
        home_win
      ) %>%
      dplyr::mutate(home_adv = -home_adv,
                    home_win = 1 - home_win)
  ) %>%
    tidyr::drop_na() %>%
    dplyr::filter(home_win == 0 | home_win == 1) %>%
    dplyr::mutate(home_win = factor(home_win, labels = c("Away", "Home")))
  
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
    expand.grid(lambda = exp(seq(-7.0, -5.68, length.out = 11)),
                alpha = seq(0, 1, length.out = 11))
  
  massey_fit <-
    train(
      home_win ~ season * (home_adv +
                             massey_away_rating +
                             massey_home_rating),
      data = fitdf,
      method = "glmnet",
      metric = "logLoss",
      trControl = fitControl,
      tuneGrid = glmnetGrid,
      intercept = FALSE
    )
  
  coef(massey_fit$finalModel, s = 0.001546130378)[, 1]
  
  fitdf$pred <-
    predict(massey_fit, newdata = fitdf, type = "prob")[, 2]
}

scorexfit <- function(dtf) {
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::select(
        gameid,
        season,
        away_team,
        home_team,
        scorex_away_rating,
        scorex_home_rating,
        home_adv,
        home_win
      ),
    dtf %>%
      dplyr::select(
        gameid,
        season,
        away_team = home_team,
        home_team = away_team,
        scorex_away_rating = scorex_home_rating,
        scorex_home_rating = scorex_away_rating,
        home_adv,
        home_win
      ) %>%
      dplyr::mutate(home_adv = -home_adv,
                    home_win = 1 - home_win)
  ) %>%
    tidyr::drop_na() %>%
    dplyr::filter(home_win == 0 | home_win == 1) %>%
    dplyr::mutate(home_win = factor(home_win, labels = c("Away", "Home")))
  
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
    expand.grid(lambda = exp(seq(-6, -3, length.out = 121)),
                alpha = 0)
  
  scorex_fit <-
    train(
      home_win ~ (home_adv +
                    scorex_away_rating +
                    scorex_home_rating),
      data = fitdf,
      method = "glmnet",
      metric = "logLoss",
      trControl = fitControl,
      tuneGrid = glmnetGrid,
      intercept = FALSE
    )
  
  plot(scorex_fit)
  min(scorex_fit$results$logLoss)
  coef(scorex_fit$finalModel, s = 0.01290681258)[, 1]
  
  fitdf$pred <-
    predict(massey_fit, newdata = fitdf, type = "prob")[, 2]
}

vegas_moneyline_fit <- function(dtf) {
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::select(
        week,
        gameid,
        season,
        away_team,
        home_team,
        vegas_moneyline_odds_away,
        vegas_moneyline_odds_home,
        home_adv,
        home_win
      ),
    dtf %>%
      dplyr::select(
        week,
        gameid,
        season,
        away_team = home_team,
        home_team = away_team,
        vegas_moneyline_odds_away = vegas_moneyline_odds_home,
        vegas_moneyline_odds_home = vegas_moneyline_odds_away,
        vegas_spread,
        home_adv,
        home_win
      ) %>%
      dplyr::mutate(
        home_adv = -home_adv,
        home_win = 1 - home_win,
        vegas_spread = -vegas_spread
      )
  ) %>%
    tidyr::drop_na() %>%
    dplyr::filter(home_win == 0 | home_win == 1) %>%
    dplyr::mutate(home_win = factor(home_win, labels = c("Away", "Home")))
  
  Xnew <- Matrix::sparse.model.matrix(
    ~ away_team + home_team + home_adv,
    data = fitdf,
    contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
  )
  
  # fitdf$pred_vegas_spread <- -predict(vegas_forecast_fit, Xnew)
  
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
    expand.grid(lambda = exp(seq(-4, -1, length.out = 121)),
                alpha = 0)
  
  X <- Matrix::sparse.model.matrix(
    home_win ~ vegas_moneyline_odds_away + vegas_moneyline_odds_home,
    data = fitdf,
    drop.unused.levels = TRUE
  )
  
  vegas_fit <-
    train(
      x = X,
      y = fitdf$home_win,
      method = "glmnet",
      metric = "logLoss",
      trControl = fitControl,
      tuneGrid = glmnetGrid,
      intercept = FALSE
    )
  
  plot(vegas_fit) 
  min(vegas_fit$results$logLoss)
  coef(vegas_fit$finalModel, s = 0.09301448921)[, 1]
  
  dfvegas$pred <-
    predict(vegas_fit, newdata = dfvegas, type = "prob")[, 2]
}

vegasfit <- function(dtf) {
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::select(
        week,
        gameid,
        season,
        away_team,
        home_team,
        vegas_spread,
        home_adv,
        home_win
      ),
    dtf %>%
      dplyr::select(
        week,
        gameid,
        season,
        away_team = home_team,
        home_team = away_team,
        vegas_spread,
        home_adv,
        home_win
      ) %>%
      dplyr::mutate(
        home_adv = -home_adv,
        home_win = 1 - home_win,
        vegas_spread = -vegas_spread
      )
  ) %>%
    tidyr::drop_na() %>%
    dplyr::filter(home_win == 0 | home_win == 1) %>%
    dplyr::mutate(home_win = factor(home_win, labels = c("Away", "Home")))
  
  Xnew <- Matrix::sparse.model.matrix(
    ~ away_team + home_team + home_adv,
    data = fitdf,
    contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
  )
  
  # fitdf$pred_vegas_spread <- -predict(vegas_forecast_fit, Xnew)
  
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
    expand.grid(lambda = exp(seq(-10, -2.3, length.out = 121)),
                alpha = 0)
  
  X <- Matrix::sparse.model.matrix(
    home_win ~ vegas_spread,
    data = fitdf,
    drop.unused.levels = TRUE
  )
  
  vegas_fit <-
    train(
      x = X, y = fitdf$home_win,
      method = "glmnet",
      metric = "logLoss",
      trControl = fitControl,
      tuneGrid = glmnetGrid,
      intercept = FALSE
    )
  
  plot(vegas_fit) 
  min(vegas_fit$results$logLoss)
  coef(vegas_fit$finalModel, s = 0.01890491508)[, 1]
  
  dfvegas$pred <-
    predict(vegas_fit, newdata = dfvegas, type = "prob")[, 2]
}

vegasforecastfit <- function() {
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::select(week,
                    gameid,
                    season,
                    away_team,
                    home_team,
                    vegas_spread,
                    home_adv) %>%
      dplyr::mutate(vegas_spread = -vegas_spread),
    dtf %>%
      dplyr::select(
        week,
        gameid,
        season,
        away_team = home_team,
        home_team = away_team,
        vegas_spread,
        home_adv
      ) %>%
      dplyr::mutate(home_adv = -home_adv)
  ) %>%
    tidyr::drop_na()
  
  foldid <-
    groupKFold(fitdf$gameid, k = length(unique(fitdf$gameid)))
  
  fitControl <- trainControl(
    method = "cv",
    index = foldid,
    search = "grid",
    allowParallel = TRUE
  )
  
  glmnetGrid <-
    expand.grid(lambda = exp(seq( -6, -3.9, length.out = 121)),
                alpha = 0.022)
  
  X <- Matrix::sparse.model.matrix(
     ~ away_team + home_team + home_adv,
    data = fitdf,
    contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
  )
  
  vegas_forecast_fit <-
    train(
      x = X,
      y = fitdf$vegas_spread,
      method = "glmnet",
      trControl = fitControl,
      tuneGrid = glmnetGrid,
      standardize = FALSE,
      intercept = FALSE
    )
  
  min(vegas_forecast_fit$results$RMSE)
  
  coef(vegas_forecast_fit$finalModel, s = 0.01116468062)
  
  dfnew <-
    expand.grid(
      away_team = factor("Arizona", levels = levels(fitdf$away_team)),
      home_team = sort(unique(fitdf$home_team)),
      home_adv = 0
    )
  
  Xnew <- Matrix::sparse.model.matrix(
    ~ away_team + home_team + home_adv,
    data = dfnew,
    contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
  )
  Xnew[, 2] <- 0
  
  dfnew$value <-
    (Xnew %*% coef(vegas_forecast_fit$finalModel, s = 0.01499558)[-1, ])[, 1]
  
}


# Massey Fit Coefficients -------------------------------------------------

head(predict(
  vegas_forecast_fit$finalModel,
  newx = X,
  s = 0.108175540105189,
  type = "response"
)[, 1])

fn <- function(par) {
  rounded_elo_prob <- psych::logistic(X %*% par)[, 1]
  elo_brier <-
    (rounded_elo_prob - dtf$home_win) * (rounded_elo_prob - dtf$home_win)
  elo_points <- 25 - (100 * elo_brier)
  return(-sum(elo_points))
}

library(optimr)
argmax <-
  optimr(par = coef(vegas_forecast_fit$finalModel, s = 0.09827359)[-1, 1],
         fn = fn)




glmnetGrid <-
  expand.grid(lambda = exp(seq(-2.5,-1.8, length.out = 101)),
              alpha = 0)

X <- Matrix::sparse.model.matrix(~ home_adv + home_off + away_def + away_off + home_def - 1,
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

min(vegas_forecast_fit$results$logLoss)

coef(vegas_forecast_fit$finalModel, s = 0.1132678)[, 1]

head(predict(
  vegas_forecast_fit$finalModel,
  newx = X,
  s = 0.1249302,
  type = "link"
)[, 1])

head(predict(
  vegas_forecast_fit$finalModel,
  newx = X,
  s = 0.1249302,
  type = "response"
)[, 1])




fitControl <- trainControl(
  method = "cv",
  index = list(which(fitdf$week != 5)),
  search = "grid",
  allowParallel = TRUE
)

glmnetGrid <-
  expand.grid(lambda = exp(seq(-4.7, -4.1, length.out = 11)),
              alpha = seq(0, 1, length.out = 11))

X <- Matrix::sparse.model.matrix(
  ~ away_team + home_team + home_adv,
  data = fitdf,
  contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
)


myfn <- wghtfn(p = 4.0)

vegas_forecast_fit <-
  train(
    x = X,
    y = fitdf$vegas_spread,
    weights = myfn(as.numeric(as.character(fitdf$week))),
    method = "glmnet",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    standardize = FALSE,
    intercept = FALSE
  )

vegas_forecast_fit

min(vegas_forecast_fit$results$RMSE)

coef(vegas_forecast_fit$finalModel, s = 0.01227734)





