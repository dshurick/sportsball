
source('./R-package/utils.R')

wk <- 17
dtf <- load_data()

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
  tidyr::drop_na() %>%
  dplyr::filter(as.numeric(as.character(week)) < wk)

trainind <- which(fitdf$week <= as.numeric(as.character(wk - 3)))

X <- Matrix::sparse.model.matrix(
  ~ away_team + home_team + home_adv,
  data = fitdf,
  contrasts.arg = list(away_team = "contr.sum", home_team = "contr.sum")
)

y <- fitdf$vegas_spread

X_train <- X[trainind, ]
X_test <- X[-trainind, ]
y_train <- y[trainind]
y_test <- y[-trainind]

week_train <- as.numeric(as.character(fitdf$week[trainind]))

wghtfn <- function(p = 0.0) {
  ret <- function(x) {
    return(((1 + x - min(x)) / (1 + max(x) - min(x))) ^ p)
  }
  return(ret)
}


optfn <- function(paramsvec) {
  
  alpha <- psych::logistic(paramsvec[1])
  lambda <- exp(paramsvec[2])
  pp <- exp(paramsvec[3])
  
  fit <-
    glmnet(
      x = X_train,
      y = y_train,
      alpha = alpha,
      lambda = lambda,
      intercept = FALSE,
      weights = wghtfn(p = pp)(week_train)
    )
  y_pred <- predict(fit,
                    newx = X_test,
                    s = lambda)[, 1]
  
  return(MLmetrics::RMSE(y_pred, y_test))
}

library(glmnet)
library(optimr)

argmax <-
  opm(
    par = c(39.118981778,  -3.482057366, 2.487703140),
    fn = optfn,
    method = c('ALL'),
    control = list(all.methods = TRUE,
                   save.failures = TRUE)
  )

argmax <-
  optimr(
    par = c(40.72489567, -4.087894007, 2.660873856),
    fn = optfn,
    method = 'Nelder-Mead',
    control = list(maxit = 2000)
  )

fit <-
  glmnet(
    x = X,
    y = y,
    alpha = psych::logistic(argmax$par[1]),
    lambda = exp(argmax$par[2]),
    intercept = FALSE,
    weights = wghtfn(p = exp(argmax$par[3]))(as.numeric(as.character(fitdf$week)))
  )

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
  (Xnew %*% coef(fit)[-1, ])[, 1]


worksheet18 <- googlesheets::gs_title("NFL 2018 Expected Wins")

vegas_ratings <- googlesheets::gs_read(worksheet18,
                                         ws = "vegas")

vegas_ratings <- dplyr::bind_rows(
  vegas_ratings %>%
    dplyr::filter(week != wk),
  dfnew %>%
    dplyr::mutate(week = wk) %>%
    dplyr::select(team = home_team, week, rating = value)
) %>%
  dplyr::distinct(week, team, .keep_all = TRUE) %>%
  dplyr::arrange(week, team)

worksheet18 <- googlesheets::gs_edit_cells(
  worksheet18,
  ws = "vegas",
  input = vegas_ratings,
  anchor = "A1"
)
