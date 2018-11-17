
source('./utils.R')

dtf <- load_data()

poissonfit <- function(dtf) {
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::filter(season == "2018", !is.na(home_win)) %>%
      dplyr::select(week, gameid,
                    away_team,
                    home_team,
                    points = score_home,
                    home_adv),
    dtf %>%
      dplyr::filter(season == "2018", !is.na(home_win)) %>%
      dplyr::select(
        week,
        gameid,
        away_team = home_team,
        home_team = away_team,
        points = score_away,
        home_adv
      ) %>%
      dplyr::mutate(home_adv = -home_adv)
  ) %>%
    mutate(home_adv2 = factor(home_adv == 1, labels = c("No", "Yes")))
  
  foldid <-
    caret::groupKFold(fitdf$gameid, k = length(unique(fitdf$gameid)))
  
  fitControl <- caret::trainControl(
    method = "cv",
    index = foldid,
    search = "grid",
    allowParallel = TRUE
  )
  
  library(brms)
  
  fit <- brm(
    points ~ away_team + home_team + home_adv2,
    family = negbinomial(),
    data = fitdf,
    cores = 7
  )

  x <- marginal_effects(fit)
  
  glmnetGrid <-
    expand.grid(lambda = exp(seq(-2.22, -0.78, length.out = 11)),
                alpha = seq(0, 1, length.out = 11))
  
  X <- Matrix::sparse.model.matrix(
    points ~ away_team + home_team + home_adv,
    data = fitdf,
    contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
  )[,-1]
  
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
  
  poisson_fit
  plot(poisson_fit)
  
  coef(poisson_fit$finalModel, s = 0.4584060113)[, 1]
  
  newdf <- fitdf %>%
    dplyr::distinct(home_team, .keep_all = TRUE) %>%
    dplyr::mutate(away_team = factor("Arizona", levels = levels(home_team)),
                  home_adv = 0) %>%
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
        home_team = factor("Arizona", levels = levels(home_team)),
        home_adv = 0
      ) %>%
      dplyr::arrange(away_team) %>%
      dplyr::select(away_team, home_team, home_adv),
    contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
  )[, -1]
  
  X_new[, 32] <- 0
  
  newdf$defense <- exp(predict(poisson_fit, newdata = X_new))
  
  newdf$rating <- with(newdf, offense - defense)
}

worksheet18 <- googlesheets::gs_title("NFL 2018 Expected Wins")

poisson_ratings <- googlesheets::gs_read(worksheet18,
                                         ws = "poisson")

poisson_ratings <- dplyr::bind_rows(
  poisson_ratings,
  newdf %>%
    dplyr::mutate(week = 11) %>%
    dplyr::select(week, home_team, offense, defense, rating)
) %>%
  dplyr::distinct(week, home_team, .keep_all = TRUE) %>%
  dplyr::arrange(week, home_team)

worksheet18 <- googlesheets::gs_edit_cells(
  worksheet18,
  ws = "poisson",
  input = poisson_ratings,
  anchor = "A1"
)
