
source('./R-package/utils.R')

dtf <- load_data()

poissonfit <- function(dtf) {
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::filter(season == "2018", !is.na(home_win)) %>%
      dplyr::select(week, ots, gameid,
                    away_team,
                    home_team,
                    points = score_home,
                    home_adv),
    dtf %>%
      dplyr::filter(season == "2018", !is.na(home_win)) %>%
      dplyr::select(
        week,
        ots,
        gameid,
        away_team = home_team,
        home_team = away_team,
        points = score_away,
        home_adv
      ) %>%
      dplyr::mutate(home_adv = -home_adv)
  ) %>%
    mutate(home_adv2 = factor(home_adv == 1, labels = c("No", "Yes")),
           minutes_played = 60 + 5 * ots) %>%
    filter(week < 4)
  
  foldid <-
    caret::groupKFold(fitdf$week, k = length(unique(fitdf$week)))
  
  fitControl <- caret::trainControl(
    method = "cv",
    index = foldid,
    search = "grid"
  )
  
  # library(brms)
  # 
  # fit <- brms::brm(
  #   brmsformula(
  #     points ~ away_team + home_team + home_adv2 + offset(log(minutes_played))
  #   ),
  #   family = negbinomial(),
  #   data = fitdf,
  #   cores = 6
  # )
  # 
  # x <-
  #   marginal_effects(fit, conditions = data.frame(minutes_played = 60))
  # 
  # library(plotly)
  # 
  # pp <- plot(x, plot = FALSE)[[1]]
  # 
  # ggplotly(pp)
  # 
  # pp <- plot(x, plot = FALSE)[[2]]
  # 
  # ggplotly(pp)
  # 
  # pp <- plot(x, plot = FALSE)[[3]]
  # 
  # ggplotly(pp)
  

  glmnetGrid <-
    expand.grid(lambda = exp(seq(2.6, 3.4, length.out = 41)),
                alpha = 0)
  
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
      standardize = FALSE
    )
  
  plot(poisson_fit)
  poisson_fit
  
  coef(poisson_fit$finalModel, s = 21.32755716)[, 1]
  
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
  poisson_ratings %>%
    dplyr::filter(week != 5),
  newdf %>%
    dplyr::mutate(week = 5) %>%
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
