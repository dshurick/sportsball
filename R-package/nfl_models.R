


library(tidyverse)
library(caret)
library(doParallel)
cl <- makePSOCKcluster(5)
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
    dplyr::mutate(
      away_team = factor(away_team),
      home_team = factor(home_team)
    )

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
      dplyr::mutate( # five38_home_rating = five38_home_rating + 65,
        season = 2018
      )
  ) %>%
    dplyr::mutate(
      week = factor(week),
      season = factor(season),
      gameid = 1:n()
    )

  return(dtf)
}

dtf <- load_data()

five38challenge <- function() {
  fitdf <- dtf %>%
    dplyr::filter(!is.na(vegas_spread)) %>%
    dplyr::select(
      gameid,
      away_team,
      home_team, vegas_spread,
      home_win
    ) %>%
    tidyr::drop_na()

  logistic <- function(xx) {
    1 / (1 + exp(-xx))
  }

  fn <- function(cffcnts) {
    rounded_elo_prob <- logistic(cffcnts * fitdf$vegas_spread)
    # rounded_elo_prob <- pnorm(-fitdf$vegas_spread, sd = cffcnts)
    elo_brier <-
      (rounded_elo_prob - fitdf$home_win) * (rounded_elo_prob - fitdf$home_win)
    elo_points <- 25 - (100 * elo_brier)
    return(sum(elo_points))
  }

  argmax <- optimise(
    fn,
    interval = c(-0.5, 0.5),
    maximum = TRUE,
    tol = .Machine$double.eps^0.5
  )


  # probs <- logistic(argmax$maximum * fitdf$vegas_spread)

  fitdf <- dtf %>%
    dplyr::filter(!is.na(vegas_spread)) %>%
    dplyr::select(
      gameid,
      away_team,
      home_team,
      vegas_spread
    )

  fitdf$predict_prob <-
    round(100 * logistic(argmax$maximum * fitdf$vegas_spread))

  return(fitdf)
}

xx <- five38challenge()

poissonfit <- function(dtf) {
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::filter(season == 2018, !is.na(home_win)) %>%
      dplyr::select(gameid,
        away_team,
        home_team,
        points = score_home,
        home_adv
      ),
    dtf %>%
      dplyr::filter(season == 2018, !is.na(home_win)) %>%
      dplyr::select(
        gameid,
        away_team = home_team,
        home_team = away_team,
        points = score_away,
        home_adv
      ) %>%
      dplyr::mutate(home_adv = -home_adv)
  )

  foldid <-
    caret::groupKFold(fitdf$gameid, k = length(unique(fitdf$gameid)))

  fitControl <- caret::trainControl(
    method = "cv",
    index = foldid,
    search = "grid",
    allowParallel = TRUE
  )

  glmnetGrid <-
    expand.grid(
      lambda = exp(seq(-.72, -.4, length.out = 11)),
      alpha = seq(0, 1, length.out = 11)
    )

  X <- Matrix::sparse.model.matrix(
    points ~ away_team + home_team + home_adv,
    data = fitdf,
    contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
  )[, -1]

  poisson_fit <-
    caret::train(
      x = X,
      y = fitdf$points,
      method = "glmnet",
      family = "poisson",
      trControl = fitControl,
      tuneGrid = glmnetGrid,
      standardize = FALSE,
      intercept = TRUE
    )

  round(coef(poisson_fit$finalModel, s = 0.6287636)[, 1], 4)
}

customfit <- function(dtf) {
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::select(
        gameid,
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
      dplyr::mutate(
        home_adv = -home_adv,
        home_win = 1 - home_win
      )
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
    expand.grid(
      lambda = exp(seq(-7, -1, length.out = 11)),
      alpha = seq(0, 1, length.out = 11)
    )

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

  round(coef(custom_fit$finalModel, s = 0.005516564)[, 1] * 1000, 6) / 1000

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
      dplyr::mutate(
        home_adv = -home_adv,
        home_win = 1 - home_win
      )
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
    expand.grid(
      lambda = exp(seq(-6.4, -5.68, length.out = 11)),
      alpha = seq(0, 1, length.out = 11)
    )

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

  round(coef(massey_fit$finalModel, s = 0.002381559)[, 1] * 1000, 6) / 1000

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
        vegas_spread,
        home_adv,
        home_win
      ),
    dtf %>%
      dplyr::select(
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
    expand.grid(
      lambda = exp(seq(-2.4, -1.12, length.out = 11)),
      alpha = seq(0, 1, length.out = 11)
    )

  vegas_fit <-
    train(
      home_win ~ home_adv + vegas_spread,
      data = fitdf,
      method = "glmnet",
      metric = "logLoss",
      trControl = fitControl,
      tuneGrid = glmnetGrid,
      intercept = FALSE
    )

  round(coef(vegas_fit$finalModel, s = 0.2222394)[, 1] * 1000, 6) / 1000

  dfvegas$pred <-
    predict(vegas_fit, newdata = dfvegas, type = "prob")[, 2]
}



vegasforecastfit <- function(){
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::filter(!is.na(vegas_spread)) %>%
      dplyr::select(
        gameid,
        away_team,
        home_team,
        vegas_spread,
        home_adv
      ) %>%
      dplyr::mutate(
        vegas_spread = -vegas_spread
      ),
    dtf %>%
      dplyr::filter(!is.na(vegas_spread)) %>%
      dplyr::select(
        gameid,
        away_team = home_team,
        home_team = away_team,
        vegas_spread,
        home_adv
      ) %>%
      dplyr::mutate(
        home_adv = -home_adv)
  )
  
  foldid <-
    groupKFold(fitdf$gameid, k = length(unique(fitdf$gameid)))
  
  fitControl <- trainControl(
    method = "cv",
    index = foldid,
    search = "grid",
    allowParallel = TRUE
  )
  
  glmnetGrid <-  expand.grid(lambda = exp(seq(-4.2, -3.24, length.out = 11)),
                             alpha = seq(0.005, 0.05, length.out = 11))
  
  X <- Matrix::sparse.model.matrix(
    vegas_spread ~ away_team + home_team + home_adv,
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
  
  coef(vegas_forecast_fit$finalModel, s = 0.02423397)
  
  dfnew <- expand.grid(away_team = factor("Arizona", levels = levels(fitdf$away_team)),
                       home_team = sort(unique(fitdf$home_team)),
                       home_adv = 0)
  
  Xnew <- Matrix::sparse.model.matrix(
     ~ away_team + home_team + home_adv,
    data = dfnew,
    contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
  )
  Xnew[, 2] <- 0
  
  dfnew$value <- (Xnew %*% coef(vegas_forecast_fit$finalModel, s = 0.01499558)[-1,])[,1]
  
}

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
  dplyr::mutate(
    away_team = factor(away_team),
    home_team = factor(home_team)
  )

massey_ratings <- googlesheets::gs_read(worksheet18,
                                        ws = "massey") %>%
  dplyr::mutate(team = factor(team))


dtf <- dplyr::bind_rows(
  sheet18 %>%
    dplyr::select(week,
                  away_team,
                  home_team,
                  home_adv,
                  home_win) %>%
    dplyr::mutate(gameid = 1:n()),
  sheet18 %>%
    dplyr::select(
      week,
      away_team = home_team,
      home_team = away_team,
      home_adv,
      home_win
    ) %>%
    dplyr::mutate(
      gameid = 1:n(),
      home_adv = -home_adv,
      home_win = 1 - home_win
    )
) %>%
  dplyr::left_join(massey_ratings %>%
                     dplyr::rename_at(.vars = vars(-week),
                                      .funs = funs(sprintf("away_%s", .)))) %>%
  dplyr::left_join(massey_ratings %>%
                     dplyr::rename_at(.vars = vars(-week),
                                      .funs = funs(sprintf("home_%s", .)))) %>%
  dplyr::filter(home_win == 1 | home_win == 0)

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

glmnetGrid <-  expand.grid(lambda = exp(seq(-1.8, -1.4, length.out = 11)),
                           alpha = seq(0, 1, length.out = 11))

X <- Matrix::sparse.model.matrix(
   ~ home_adv:away_hfa + home_adv:home_hfa + away_off + away_def + home_off + home_def,
  data = dtf
)

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
# 0.6736977

coef(vegas_forecast_fit$finalModel, s = 0.19398)[,1]



glmnetGrid <-  expand.grid(lambda = exp(seq(-2.5, -1.8, length.out = 101)),
                           alpha = 0)

X <- Matrix::sparse.model.matrix(
  ~ home_adv + home_off + away_def + away_off + home_def - 1,
  data = dtf
)

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

coef(vegas_forecast_fit$finalModel, s = 0.1132678)[,1]

head(predict(vegas_forecast_fit$finalModel, newx = X, s = 0.1249302, type = "link")[,1])

head(predict(vegas_forecast_fit$finalModel, newx = X, s = 0.1249302, type = "response")[,1])


