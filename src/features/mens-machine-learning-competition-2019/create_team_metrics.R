#!/usr/local/bin/ Rscript

suppressWarnings({
  suppressMessages({
    library(dplyr)
    library(readr)
    library(stringr)
    library(Matrix)
    library(glmnet)
    library(checkmate)
  })
})

parse_args <- function() {
  option_list <- list(
    optparse::make_option(
      c("--year"),
      type = "integer",
      default = NULL,
      help = "Which year to produce predictions.",
      metavar = "integer"
    ),
    optparse::make_option(
      c("--detailed"),
      type = "character",
      default = NULL,
      help = "Location of file containing detailed game data.",
      metavar = "filepath"
    ),
    optparse::make_option(
      c("--outfile"),
      type = "character",
      default = NULL,
      help = "Location of output file.",
      metavar = "filepath"
    )
  )
  opt_parser = optparse::OptionParser(option_list = option_list)
  opt = optparse::parse_args(opt_parser)
  
  return(opt)
}

adjust_metrics <-
  function(X,
           y,
           reg_season_indices,
           ncaa_tourney_indices,
           param_mask_off,
           param_mask_def,
           varname) {
    fit <-
      glmnet(
        X[reg_season_indices, ],
        y[reg_season_indices],
        alpha = 0,
        standardize = FALSE,
        lambda = c(exp(seq(
          1, -20, length.out = 99
        )), 0)
      )
    
    y_hat <- predict(fit,
                     X[-reg_season_indices, ],
                     s = NULL,
                     type = "response")
    
    rmses <-
      apply(y_hat, 2, function(x) {
        sqrt(mean((x - y[-reg_season_indices]) ^ 2))
      })
    best_lambda <- fit$lambda[which.min(rmses)]
    
    full_fit <-
      glmnet(
        X[-ncaa_tourney_indices,],
        y[-ncaa_tourney_indices],
        alpha = 0,
        standardize = FALSE,
        lambda = c(exp(seq(
          1, -20, length.out = 99
        )), 0)
      )
    
    cffcnts_off <- predict(full_fit,
                           param_mask_off,
                           s = c(best_lambda, 0),
                           type = "response")
    
    cffcnts_def <- predict(full_fit,
                           param_mask_def,
                           s = c(best_lambda, 0),
                           type = "response")
    
    colnames(cffcnts_off) <-
      paste0(varname, c("_Off_rglzd", "_Off"))
    colnames(cffcnts_def) <-
      paste0(varname, c("_Def_rglzd", "_Def"))
    
    
    return(tibble::as_tibble(cbind(cffcnts_off, cffcnts_def)))
  }


create_adjusted_metrics <- function(games_df) {
  games_df <- droplevels(games_df)
  
  data_train <- games_df %>%
    filter(CRType == 'Regular')
  
  # data_test <- games_df %>%
  #   filter(CRType != 'Regular')
  
  X <-   sparse.model.matrix(
    ~ TeamSeason1 + TeamSeason2 + team1loc,
    data = games_df,
    contrasts = list(TeamSeason1 = 'contr.sum', TeamSeason2 = 'contr.sum')
  )
  
  X_train <-   sparse.model.matrix(
    ~ TeamSeason1 + TeamSeason2 + team1loc,
    data = data_train,
    contrasts = list(TeamSeason1 = 'contr.sum', TeamSeason2 = 'contr.sum')
  )
  
  # X_test <-   sparse.model.matrix(
  #   ~ TeamSeason1 + TeamSeason2 + team1loc,
  #   data = data_test,
  #   contrasts = list(TeamSeason1 = 'contr.sum', TeamSeason2 = 'contr.sum')
  # )
  
  teams_df <- games_df %>%
    mutate(TeamID1 = as.character(TeamID1),
           TeamID2 = as.character(TeamID2)) %>%
    distinct(Season, TeamID1, TeamSeason1) %>%
    arrange(Season, TeamID1)
  
  param_mask <-
    sparse.model.matrix( ~ TeamSeason1,
                         data = teams_df,
                         contrasts = list(TeamSeason1 = 'contr.sum'))
  
  param_mask_off <-
    cbind(param_mask, Matrix(
      data = 0,
      nrow = nrow(param_mask),
      ncol = ncol(X_train) - ncol(param_mask)
    ))
  
  param_mask_def <-
    cbind(1, Matrix(
      data = 0,
      nrow = nrow(param_mask),
      ncol = ncol(param_mask) - 1
    ), param_mask[, -1], 0)
  
  # reg_season_indices <- which(games_df$CRType == 'Regular')
  # ncaa_tourney_indices <- which(games_df$CRType == 'NCAA')
  
  adjoe <-
    adjust_metrics(
      X,
      y = games_df$oe1,
      reg_season_indices = which(games_df$CRType == 'Regular'),
      ncaa_tourney_indices = which(games_df$CRType == 'NCAA'),
      param_mask_off = param_mask_off,
      param_mask_def = param_mask_def,
      varname = "AdjEff"
    )
  
  adjtempo <-
    adjust_metrics(
      X,
      y = games_df$tempo,
      reg_season_indices = which(games_df$CRType == 'Regular'),
      ncaa_tourney_indices = which(games_df$CRType == 'NCAA'),
      param_mask_off = param_mask_off,
      param_mask_def = param_mask_def,
      varname = "Adj_Tempo"
    )
  
  adjeFG <-
    adjust_metrics(
      X,
      y = games_df$eFG1,
      reg_season_indices = which(games_df$CRType == 'Regular'),
      ncaa_tourney_indices = which(games_df$CRType == 'NCAA'),
      param_mask_off = param_mask_off,
      param_mask_def = param_mask_def,
      varname = "Adj_eFGPct"
    )
  
  adjtopct <-
    adjust_metrics(
      X,
      y = games_df$topct1,
      reg_season_indices = which(games_df$CRType == 'Regular'),
      ncaa_tourney_indices = which(games_df$CRType == 'NCAA'),
      param_mask_off = param_mask_off,
      param_mask_def = param_mask_def,
      varname = "Adj_TOpct"
    )
  
  adjorpct <-
    adjust_metrics(
      X,
      y = games_df$orpct1,
      reg_season_indices = which(games_df$CRType == 'Regular'),
      ncaa_tourney_indices = which(games_df$CRType == 'NCAA'),
      param_mask_off = param_mask_off,
      param_mask_def = param_mask_def,
      varname = "Adj_ORpct"
    )
  
  adjftrate <-
    adjust_metrics(
      X,
      y = games_df$ftrate1,
      reg_season_indices = which(games_df$CRType == 'Regular'),
      ncaa_tourney_indices = which(games_df$CRType == 'NCAA'),
      param_mask_off = param_mask_off,
      param_mask_def = param_mask_def,
      varname = "Adj_FTRate"
    )
  
  adjshotopp <-
    adjust_metrics(
      X,
      y = games_df$shotopp1,
      reg_season_indices = which(games_df$CRType == 'Regular'),
      ncaa_tourney_indices = which(games_df$CRType == 'NCAA'),
      param_mask_off = param_mask_off,
      param_mask_def = param_mask_def,
      varname = "Adj_ShotOpp"
    )
  
  adjtspct <-
    adjust_metrics(
      X,
      y = games_df$tspct1,
      reg_season_indices = which(games_df$CRType == 'Regular'),
      ncaa_tourney_indices = which(games_df$CRType == 'NCAA'),
      param_mask_off = param_mask_off,
      param_mask_def = param_mask_def,
      varname = "Adj_TSPct"
    )
  
  team_metrics <- bind_cols(
    teams_df,
    adjoe,
    adjtempo,
    adjeFG,
    adjtopct,
    adjorpct,
    adjftrate,
    adjshotopp,
    adjtspct
  )
  
  return(team_metrics)
  
}

main <- function() {
  opt <- parse_args()
  
  if (!checkmate::testPathForOutput(opt$outfile))
    dir.create(dirname(opt$outfile), recursive = TRUE)
  
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_number(opt$year,
                           lower = 2003,
                           upper = 2999,
                           add = coll)
  checkmate::assert_file(opt$detailed,
                         'r',
                         extension = c('csv'),
                         add = coll)
  checkmate::assert_path_for_output(opt$outfile, add = coll)
  checkmate::reportAssertions(coll)
  
  detailed <- readr::read_csv(
    opt$detailed,
    col_types = cols(
      .default = col_double(),
      Loc1 = col_character(),
      CRType = col_character(),
      gameid = col_character(),
      ConfAbbrev = col_character(),
      TeamSeason1 = col_character(),
      TeamSeason2 = col_character(),
      TeamID1 = col_character(),
      TeamID2 = col_character()
    )
  ) %>%
    mutate_at(
      .vars = vars(TeamSeason1, TeamSeason2, TeamID1, TeamID2),
      .funs = funs(factor(.))
    )
  
  team_metrics <-
    create_adjusted_metrics(detailed %>% filter(Season <= opt$year))
  
  team_metrics %>% readr::write_csv(opt$outfile,
                                    na = "")
  
}

main()
