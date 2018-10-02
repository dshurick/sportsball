
library(tidyverse)
library(magrittr)
library(Rglpk)
library(googlesheets)
library(Matrix)
library(glmnet)
library(caret)

worksheet18 <- googlesheets::gs_title("NFL 2018 Expected Wins")

# pctwin_by_pntsprd <- gs_read(
#   worksheet18,
#   ws = "pctwin_by_pntsprd",
#   col_types = cols(
#     .default = col_number()
#   )
# )
# 
# logistic <- function(x) {
#   1 / (1 + exp(-x))
# }
# 
# fit <-
#   glm(cbind(wins, games - wins) ~ spread,
#       data = pctwin_by_pntsprd,
#       family = binomial())
# 
# pctwin_by_pntsprd$estimated_prob <- predict(fit, pctwin_by_pntsprd, type = "response")

load_data <- function() {
  worksheet17 <- googlesheets::gs_title("NFL 2017 Expected Wins")
  worksheet18 <- googlesheets::gs_title("NFL 2018 Expected Wins")

  # Read from 2017 Worksheet

  sheet17 <- gs_read(
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

  sheet18 <- gs_read(
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
      dplyr::mutate(
        # five38_home_rating = five38_home_rating + 65,
        season = 2018
      )
  ) %>%
    dplyr::mutate(week = factor(week),
                  gameid = 1:n())

  return(dtf)
}

dtf <- load_data()

fit_scorex <- function(.dtf) {
  fitdf <- dplyr::bind_rows(
    .dtf %>%
      dplyr::select(
        scorex_away_rating,
        scorex_home_rating,
        home_win,
        home_adv
      ),
    .dtf %>%
      dplyr::select(
        scorex_home_rating = scorex_away_rating,
        scorex_away_rating = scorex_home_rating,
        home_win,
        home_adv
      ) %>%
      dplyr::mutate(
        home_adv = -home_adv,
        home_win = 1 - home_win
      )
  ) %>%
    dplyr::filter(home_win == 0.0 | home_win == 1.0,
                  !is.na(scorex_away_rating))

  fit <- glm(
    as.factor(home_win) ~ scorex_away_rating + scorex_home_rating + home_adv - 1,
    data = fitdf,
    family = binomial()
  )
  
  return(fit)
}

fit_vegas <- function(.dtf) {
  fitdf <- dplyr::bind_rows(
    .dtf %>%
      dplyr::filter(!is.na(home_win), !is.na(vegas_spread)) %>%
      dplyr::select(
        gameid,
        vegas_spread,
        home_win,
        home_adv
      ) %>%
      dplyr::mutate(home_adv = 1),
    .dtf %>%
      dplyr::filter(!is.na(home_win), !is.na(vegas_spread)) %>%
      dplyr::select(
        gameid,
        vegas_spread,
        home_win,
        home_adv
      ) %>%
      dplyr::mutate(
        home_adv = -home_adv,
        vegas_spread = -vegas_spread,
        home_win = 1 - home_win
      )
  ) %>%
    dplyr::filter(home_win == 0.0 | home_win == 1.0)

  fit <- glm(
    as.factor(home_win) ~ vegas_spread - 1,
    data = fitdf %>%
      dplyr::filter(home_win == 0.0 | home_win == 1.0),
    family = binomial()
  )

  return(fit)
}

vegas_forecast <- function(.dtf) {
  fitdf <- dplyr::bind_rows(
    .dtf %>%
      dplyr::filter(!is.na(vegas_spread)) %>%
      dplyr::select(
        away_team,
        home_team,
        vegas_spread,
        home_adv
      ) %>%
      dplyr::mutate(
        vegas_spread = -vegas_spread
      ),
    .dtf %>%
      dplyr::filter(!is.na(vegas_spread)) %>%
      dplyr::select(
        away_team = home_team,
        home_team = away_team,
        vegas_spread,
        home_adv
      ) %>%
      dplyr::mutate(
        home_adv = -home_adv
      )
  )

  X <- sparse.model.matrix(
    vegas_spread ~ away_team + home_team + home_adv,
    data = fitdf,
    contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
  )

  fit <- lm(
    vegas_spread ~ away_team + home_team + home_adv,
    data = fitdf,
    contrasts = list(away_team = "contr.sum", home_team = "contr.sum")
  )

  return(fit)
}

fit_poisson <- function(.dtf) {
  fitdf <- dplyr::bind_rows(
    .dtf %>%
      dplyr::filter(!is.na(home_win)) %>%
      dplyr::select(
        home_adv,
        defense = away_team,
        offense = home_team,
        points_scored = score_home
      ),
    .dtf %>%
      dplyr::filter(!is.na(home_win)) %>%
      dplyr::select(
        home_adv,
        defense = home_team,
        offense = away_team,
        points_scored = score_away
      ) %>%
      dplyr::mutate(home_adv = -home_adv)
  )

  X <-
    sparse.model.matrix(points_scored ~ defense + offense + home_adv,
      data = fitdf
    )
  fit <-
    cv.glmnet(
      x = X[, -1],
      y = dtf$points_scored,
      family = "poisson",
      nfolds = nrow(X),
      alpha = 0
    )
  return(fit)
}

fit1 <- dtf %>%
  dplyr::filter(season == 2018) %>%
  fit_scorex()

fit2 <- dtf %>%
  dplyr::filter(season == 2018) %>%
  fit_vegas()

fit3 <- dtf %>%
  dplyr::filter(season == 2018) %>%
  vegas_forecast()

fit4 <- dtf %>%
  dplyr::filter(season == 2018) %>%
  fit_poisson()






fitdf <- dplyr::bind_rows(
  dtf %>%
    dplyr::filter(season == 2018, !is.na(home_win)) %>%
    dplyr::select(
      gameid,
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
    dplyr::mutate(
      home_adv = -home_adv)
)

library(caret)

foldid <-
  groupKFold(fitdf$gameid, k = length(unique(fitdf$gameid)))

fitControl <- trainControl(
  method = "cv",
  index = foldid,
  search = "grid",
  allowParallel = TRUE
)

glmnetGrid <-  expand.grid(lambda = exp(seq(-1.2, 0.4, length.out = 11)),
                           alpha = seq(0, 1, length.out = 11))

X <- sparse.model.matrix(
  points ~ away_team + home_team + home_adv,
  data = fitdf,
  contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
)[, -1]

poisson_fit <-
  train(
    x = X,
    y = fitdf$points,
    method = "glmnet",
    family="poisson",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    standardize = FALSE,
    intercept = TRUE
  )

round(coef(poisson_fit$finalModel, s = 0.67032)[,1] * 1000, 6) / 1000


fitdf <- dplyr::bind_rows(
  dtf %>%
    dplyr::filter(season == 2018, home_win == 1.0 |
                    home_win == 0.0) %>%
    dplyr::select(gameid,
                  away_team,
                  home_team,
                  home_win,
                  home_adv),
  dtf %>%
    dplyr::filter(season == 2018, home_win == 1.0 |
                    home_win == 0.0) %>%
    dplyr::select(
      gameid,
      away_team = home_team,
      home_team = away_team,
      home_win,
      home_adv
    ) %>%
    dplyr::mutate(home_adv = -home_adv,
                  home_win = 1 - home_win)
)

library(caret)

foldid <-
  groupKFold(fitdf$gameid, k = length(unique(fitdf$gameid)))

fitControl <- trainControl(
  method = "cv",
  index = foldid,
  search = "grid",
  classProbs = TRUE,
  savePredictions = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

glmnetGrid <-  expand.grid(lambda = exp(seq(-8, 2, length.out = 11)),
                           alpha = 0)

X <- sparse.model.matrix(
  home_win ~ away_team + home_team + home_adv,
  data = fitdf,
  contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
)[, -1]

binomial_fit <-
  train(
    x = X,
    y = factor(fitdf$home_win, labels = c("no", "yes")),
    method = "glmnet",
    family = "binomial",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    standardize = FALSE,
    intercept = FALSE
  )

round(coef(binomial_fit$finalModel, s = 0.08374323)[,1] * 1000, 6) / 1000






fitdf <- dplyr::bind_rows(
  dtf %>%
    dplyr::filter(!is.na(home_win)) %>%
    dplyr::select(
      gameid,
      season,
      sagarin_away_rating,
      sagarin_home_rating,
      five38_away_rating,
      five38_home_rating,
      massey_away_rating,
      massey_home_rating,
      scorex_away_rating,
      scorex_home_rating,
      vegas_spread,
      home_win,
      home_adv
    ) %>%
    dplyr::mutate(home_adv = 1),
  dtf %>%
    dplyr::filter(!is.na(home_win)) %>%
    dplyr::select(
      gameid,
      season,
      sagarin_away_rating = sagarin_home_rating,
      sagarin_home_rating = sagarin_away_rating,
      five38_away_rating = five38_home_rating,
      five38_home_rating = five38_away_rating,
      massey_away_rating = massey_home_rating,
      massey_home_rating = massey_away_rating,
      scorex_away_rating = scorex_home_rating,
      scorex_home_rating = scorex_away_rating,
      vegas_spread,
      home_win,
      home_adv
    ) %>%
    dplyr::mutate(
      home_adv = -home_adv,
      home_win = 1 - home_win,
      vegas_spread = -vegas_spread
    )
) %>%
  dplyr::filter(home_win == 0.0 | home_win == 1.0) %>%
  dplyr::mutate(home_win = factor(home_win, labels = c("Away", "Home")),
                season = factor(season))


dfvegas <- dtf %>%
  dplyr::filter(!is.na(home_win)) %>%
  dplyr::select(gameid, vegas_spread, home_win) %>%
  tidyr::drop_na() %>%
  dplyr::filter(home_win == 0.0 | home_win == 1.0) %>%
  dplyr::mutate(home_win = factor(home_win, labels = c("Away", "Home")))

foldid <-
  groupKFold(dfvegas$gameid, k = length(unique(dfvegas$gameid)))

fitControl <- trainControl(
  method = "cv",
  index = foldid,
  search = "grid",
  allowParallel = TRUE
)

glmnetGrid <-  expand.grid(lambda = exp(seq(-2, 1, length.out = 11)),
                           alpha = seq(0, 1, length.out = 11))

vegas_fit <-
  train(
    home_win ~ vegas_spread,
    data = dfvegas,
    method = "glmnet",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    intercept = FALSE
  )


vegas_fit <-
  train(
    home_win ~ vegas_spread,
    data = dfvegas,
    method = "glmnet",
    metric = "logLoss",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    intercept = FALSE
  )

round(coef(vegas_fit$finalModel, s = 0.2231302)[,1] * 1000, 6) / 1000

dfvegas$pred <- predict(vegas_fit, newdata = dfvegas, type = "prob")[,2]



dfscorex <- fitdf %>%
  dplyr::select(gameid, home_adv, scorex_away_rating, scorex_home_rating, home_win) %>%
  tidyr::drop_na()

foldid <-
  groupKFold(dfscorex$gameid, k = length(unique(dfscorex$gameid)))

fitControl <- trainControl(
  method = "cv",
  index = foldid,
  search = "grid",
  classProbs = TRUE,
  savePredictions = TRUE,
  summaryFunction = mnLogLoss,
  allowParallel = TRUE
)

glmnetGrid <-  expand.grid(lambda = exp(seq(-4, -1.245696, length.out = 11)),
                           alpha = seq(0, 1, length.out = 11))

scorex_fit <-
  train(
    home_win ~ home_adv + scorex_away_rating + scorex_home_rating,
    data = dfscorex,
    method = "glmnet",
    metric = "logLoss",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    intercept = FALSE
  )

round(coef(scorex_fit$finalModel, s = 0.07259582)[,1] * 1000, 6) / 1000

dfvegas$pred <- predict(vegas_fit, newdata = dfvegas, type = "prob")[,2]



dfmassey <- fitdf %>%
  dplyr::select(gameid, season, home_adv, massey_away_rating, massey_home_rating, home_win) %>%
  tidyr::drop_na()

foldid <-
  groupKFold(dfmassey$gameid, k = length(unique(dfmassey$gameid)))

fitControl <- trainControl(
  method = "cv",
  index = foldid,
  search = "grid",
  classProbs = TRUE,
  savePredictions = TRUE,
  summaryFunction = mnLogLoss,
  allowParallel = TRUE
)

glmnetGrid <-  expand.grid(lambda = exp(seq(-7, -5.6, length.out = 11)),
                           alpha = seq(0, 1, length.out = 11))

massey_fit <-
  train(
    home_win ~ season * (home_adv + massey_away_rating + massey_home_rating),
    data = dfmassey,
    method = "glmnet",
    metric = "logLoss",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    intercept = FALSE
  )

round(coef(massey_fit$finalModel, s = 0.001836305)[,1] * 1000, 6) / 1000

dfvegas$pred <- predict(vegas_fit, newdata = dfvegas, type = "prob")[,2]








scorex_df <- h2o::as.h2o(fitdf, destination_frame = "scorex_df")
scorex_df$home_win = as.factor(scorex_df$home_win)
scorex_df$gameid = as.factor(scorex_df$gameid)
scorex_df$season = as.factor(scorex_df$season)

# GBM hyperparamters
glm_params1 <- list(alpha = seq(0, 1, length.out = 11))

# Train and validate a cartesian grid of GBMs
glm_grid1 <- h2o.grid(
  "glm",
  x = c("season", "massey_away_rating", "massey_home_rating", "home_adv"),
  y = "home_win",
  grid_id = "glm_grid1",
  family = "binomial",
  fold_column = "gameid",
  intercept = FALSE,
  lambda_search = TRUE,
  training_frame = scorex_df,
  hyper_params = glm_params1
)

fit <-
  h2o::h2o.glm(
    x = c(
      "season",
      "massey_away_rating",
      "massey_home_rating",
      "home_adv"
    ),
    y = "home_win",
    training_frame = scorex_df,
    family = "binomial",
    fold_column = "gameid",
    alpha = 0,
    lambda_search = TRUE
  )



dffive38 <- fitdf %>%
  dplyr::select(
    gameid,
    season,
    home_adv,
    five38_away_rating,
    five38_home_rating,
    home_win
  ) %>%
  tidyr::drop_na()

foldid <-
  groupKFold(dffive38$gameid, k = length(unique(dffive38$gameid)))

fitControl <- trainControl(
  method = "cv",
  index = foldid,
  search = "grid",
  classProbs = TRUE,
  savePredictions = TRUE,
  summaryFunction = mnLogLoss,
  allowParallel = TRUE
)

glmnetGrid <-  expand.grid(lambda = exp(seq(-3.78, -2.52, length.out = 11)),
                           alpha = seq(0, 1, length.out = 11))

custom_fit <-
  train(
    home_win ~ season * (
      home_adv +
        five38_away_rating +
        five38_home_rating
    ),
    data = dffive38,
    method = "glmnet",
    metric = "logLoss",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    intercept = FALSE
  )

round(coef(custom_fit$finalModel, s = 0.05513338)[,1] * 1000, 6) / 1000

dfvegas$pred <- predict(vegas_fit, newdata = dfvegas, type = "prob")[,2]




dfsagarin <- fitdf %>%
  dplyr::select(
    gameid,
    season,
    home_adv,
    sagarin_away_rating,
    sagarin_home_rating,
    home_win
  ) %>%
  tidyr::drop_na()

foldid <-
  groupKFold(dfsagarin$gameid, k = length(unique(dfsagarin$gameid)))

fitControl <- trainControl(
  method = "cv",
  index = foldid,
  search = "grid",
  classProbs = TRUE,
  savePredictions = TRUE,
  summaryFunction = mnLogLoss,
  allowParallel = TRUE
)

glmnetGrid <-  expand.grid(lambda = exp(seq(-6, 0, length.out = 11)),
                           alpha = seq(0, 1, length.out = 11))

sagarin_fit <-
  train(
    home_win ~ season * (
      home_adv +
        sagarin_away_rating +
        sagarin_home_rating
    ),
    data = dfsagarin,
    method = "glmnet",
    metric = "logLoss",
    trControl = fitControl,
    tuneGrid = glmnetGrid,
    intercept = FALSE
  )

round(coef(sagarin_fit$finalModel, s = 0.01499558)[,1] * 1000, 6) / 1000

dfvegas$pred <- predict(vegas_fit, newdata = dfvegas, type = "prob")[,2]
