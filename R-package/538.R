
source('./R-package/utils.R')

dtf <- load_data()

five38challenge <- function() {
  
  # fitdf <- dtf %>%
  #   dplyr::filter(!is.na(vegas_spread)) %>%
  #   dplyr::select(gameid,
  #                 home_adv,
  #                 away_team,
  #                 home_team,
  #                 vegas_spread,
  #                 home_win) %>%
  #   tidyr::drop_na()
  # 
  # fitdf <- dplyr::bind_rows(
  #   fitdf %>%
  #     dplyr::select(home_adv, vegas_spread, home_win),
  #   fitdf %>%
  #     dplyr::select(home_adv, vegas_spread, home_win) %>%
  #     dplyr::mutate(
  #       home_adv = -home_adv,
  #       vegas_spread = -vegas_spread,
  #       home_win = 1 - home_win
  #     )
  # )
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::select(
        gameid,
        season,
        away_team,
        home_team,
        scorex_away_rating,
        scorex_home_rating,
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
        scorex_away_rating = scorex_home_rating,
        scorex_home_rating = scorex_away_rating,
        vegas_spread,
        home_adv,
        home_win
      ) %>%
      dplyr::mutate(home_adv = -home_adv,
                    home_win = 1 - home_win,
                    vegas_spread = -vegas_spread)
  ) %>%
    tidyr::drop_na()
  
  logistic <- function(xx) {
    1 / (1 + exp(-xx))
  }
  
  X <-
    sparse.model.matrix(~ vegas_spread + home_adv - 1,
                        fitdf)
  
  cffcnts <- c(0, 0)
  
  fn <- function(cffcnts) {
    rounded_elo_prob <- logistic((X %*% cffcnts)[, 1])
    elo_brier <-
      (rounded_elo_prob - fitdf$home_win) * (rounded_elo_prob - fitdf$home_win)
    elo_points <- 25 - (100 * elo_brier)
    return(-sum(elo_points))
  }
  
  argmax <-
    optimr::optimr(
      par = c(0, 0),
      fn = fn,
      method = "BFGS",
      control = list(trace = 1)
    )
  
  
  # probs <- logistic(argmax$maximum * fitdf$vegas_spread)
  
  fitdf <- dplyr::bind_rows(
    dtf %>%
      dplyr::select(
        gameid,
        week,
        season,
        away_team,
        home_team,
        scorex_away_rating,
        scorex_home_rating,
        vegas_spread,
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
        scorex_away_rating = scorex_home_rating,
        scorex_home_rating = scorex_away_rating,
        vegas_spread,
        home_adv,
        home_win
      ) %>%
      dplyr::mutate(home_adv = -home_adv,
                    home_win = 1 - home_win,
                    vegas_spread = -vegas_spread)
  ) %>%
    dplyr::filter(week == 19)
  
  X <-
    sparse.model.matrix(~ vegas_spread + home_adv - 1,
                        fitdf)
  
  fitdf$predict_prob <-
    round(100 * logistic((X %*% argmax$par)[, 1]))
  
  return(fitdf)
}

xx <- five38challenge()



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

fitdf <- dplyr::bind_rows(
  sheet18 %>%
    dplyr::select(week,
                  away_team,
                  home_team,
                  home_adv,
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
  dplyr::mutate(
    home_hfa = ifelse(home_adv == 1, home_hfa, 0),
    away_hfa = ifelse(home_adv == -1, away_hfa, 0),
    home_adv = factor(
      home_adv,
      levels = c(0, 1,-1),
      labels = c("neutral", "home",  "away")
    )
  ) %>%
  drop_na()

traindf <- fitdf %>%
  dplyr::filter(week < 12)

testdf <- fitdf %>%
  dplyr::filter(week >= 12)

X <-
  Matrix::sparse.model.matrix(
    ~ home_adv + home_adv:(away_hfa + home_hfa) + away_off + away_def + home_off + home_def,
    data = fitdf
  )[, -1]

linpredict <- function(cffcnts, type = c("link", "prob")) {
  type <- match.arg(type)
  
  home_adv <- cffcnts[1]
  off <- cffcnts[2]
  def <- cffcnts[3]
  hfa <- cffcnts[4]
  
  link <-
    ((
      X %*% c(home_adv, -home_adv, -off, def, off, -def, 0, 0, hfa, 0, -hfa, 0)
    )[, 1])
  
  if (type == "prob")
    return(psych::logistic(link))
  else
    return(link)
}

fn <- function(cffcnts) {
  rounded_elo_prob <- linpredict(cffcnts, type = "prob")
  elo_brier <-
    (rounded_elo_prob - fitdf$home_win) * (rounded_elo_prob - fitdf$home_win)
  elo_points <- 25 - (100 * elo_brier)
  return(-sum(elo_points))
}


fitbrier <-
  function(data,
           lambda = 0,
           weights,
           subset,
           start = NULL) {
    X <-
      Matrix::sparse.model.matrix(
        ~ home_adv + home_adv:(away_hfa + home_hfa) + away_off + away_def + home_off + home_def,
        data = data
      )[, c(2, 3, 4, 5, 6, 7, 10, 12)]
    
    X_ <- scale(X)
    y <- data$home_win
    
    linpredict <- function(cffcnts, type = c("link", "prob")) {
      type <- match.arg(type)
      
      home_adv <- cffcnts[1]
      off <- cffcnts[2]
      def <- cffcnts[3]
      hfa <- cffcnts[4]
      
      link <-
        ((X_ %*% c(
          home_adv,-home_adv,-off, def, off,-def, hfa, -hfa
        ))[, 1])
      
      if (type == "prob")
        return(psych::logistic(link))
      else
        return(link)
    }
    
    fn <- function(cffcnts) {
      rounded_elo_prob <- linpredict(cffcnts, type = "prob")
      
      elo_brier <-
        (rounded_elo_prob - y) * (rounded_elo_prob - y)
      elo_points <- 25 - (100 * elo_brier)
      
      l2norm <- sum(cffcnts ^ 2)
      l1norm <- sum(abs(cffcnts))
      
      return(-sum(elo_points) + lambda * l2norm)
    }
    
    argmax <-
      optimr::optimr(par = rep(0, 4),
                     fn = fn,
                     method = "BFGS")
    
    ret <- list(
      coefficients = argmax$par,
      "scaled:center" = attr(X_, "scaled:center"),
      "scaled:scale" = attr(X_, "scaled:scale")
    )
    class(ret) <- "brier"
    return(ret)
  }

predict.brier <- function(object, newdata, type = c("link", "prob")) {
  type <- match.arg(type)
  X <-
    Matrix::sparse.model.matrix(
      ~ home_adv + home_adv:(away_hfa + home_hfa) + away_off + away_def + home_off + home_def,
      data = newdata
    )[, c(2, 3, 4, 5, 6, 7, 10, 12)]
  
  X_ <-
    scale(X,
          center = object$`scaled:center`,
          scale = object$`scaled:scale`)
  
  linpredict <- function(cffcnts, type = c("link", "prob")) {
    type <- match.arg(type)
    
    home_adv <- cffcnts[1]
    off <- cffcnts[2]
    def <- cffcnts[3]
    hfa <- cffcnts[4]
    
    link <-
      ((X_ %*% c(
        home_adv,-home_adv,-off, def, off,-def, hfa, -hfa
      ))[, 1])
    
    if (type == "prob")
      return(psych::logistic(link))
    else
      return(link)
  }
  
  return(linpredict(object$coefficients, type = type))
}


# 
# brierscore(testdf$home_win, testdf$predictions)

foldid <-
  caret::groupKFold(fitdf$gameid, k = length(unique(fitdf$gameid)))

# allcoefs <- foldid %>%
#   purrr::map_df(function(x) {
#     fit <- fitbrier(fitdf[x, ], lambda = exp(6.049115))
#     predictions <- predict(fit, fitdf[-x,], type = "prob")
#     return(data.frame(home_win = fitdf$home_win[-x], y_hat = predictions))
#   })

brierscore <- function(y_true, y_pred) {
  return(sum(25 - (100 * (y_pred - y_true)^2)))
}

# brierscore(allcoefs$home_win, allcoefs$y_hat)
# 3.538996995

fn <- function(lmbda) {
  elmbda <- exp(lmbda)
  allcoefs <- foldid %>%
    purrr::map_df(function(x) {
      fit <- fitbrier(fitdf[x,], lambda = elmbda)
      predictions <- predict(fit, fitdf[-x, ], type = "prob")
      return(data.frame(home_win = fitdf$home_win[-x], y_hat = predictions))
    })
  ret <- brierscore(allcoefs$home_win, allcoefs$y_hat)
  print(sprintf("%.3f - %.3f", elmbda, ret))
  return(ret)
}

argmax <- optimise(fn, interval = c(-2, 10), maximum = TRUE)

fit <- fitbrier(fitdf, lambda = exp(6.207603))
fitdf$predictions <- predict(fit, fitdf, type = "prob")

# argmax <-
#   optimr::optimr(
#     par = rep(0, 4),
#     fn = fn,
#     method = "BFGS",
#     control = list(trace = 1)
#   )

# fit <- lm(home_win ~ home_adv + home_adv:(away_hfa + home_hfa) + away_off + away_def + home_off + home_def,
#           data = fitdf)

fitdf <- dplyr::bind_rows(
  sheet18 %>%
    dplyr::select(week,
                  away_team,
                  home_team,
                  home_adv,
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
  dplyr::mutate(
    home_hfa = ifelse(home_adv == 1, home_hfa, 0),
    away_hfa = ifelse(home_adv == -1, away_hfa, 0),
    home_adv = factor(
      home_adv,
      levels = c(0, 1,-1),
      labels = c("neutral", "home",  "away")
    )
  ) %>%
  dplyr::select(-home_win) %>%
  tidyr::drop_na()

X <-
  Matrix::sparse.model.matrix(
    ~ home_adv + home_adv:(away_hfa + home_hfa) + away_off + away_def + home_off + home_def,
    data = fitdf
  )[, -1]

fitdf$predict_prob <- predict.brier(fit, fitdf, type = "prob")

fitdf %>%
  dplyr::filter(week == 14) %>%
  dplyr::arrange(gameid) %>%
  dplyr::select(away_team, home_team, predict_prob) %>%
  dplyr::mutate(predict_prob = round(100 * predict_prob)) %>%
  as.data.frame()

fn(argmax$par)
