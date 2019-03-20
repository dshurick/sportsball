#!/usr/bin/env Rscript

suppressWarnings({
  suppressMessages({
    library(dplyr)
    library(h2o)
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
      metavar = "FILEPATH"
    ),
    optparse::make_option(
      c("--metrics"),
      type = "character",
      default = NULL,
      help = "Location of file containing adjusted team metrics.",
      metavar = "FILEPATH"
    ),
    optparse::make_option(
      c("--outfile"),
      type = "character",
      default = NULL,
      help = "Location of output file.",
      metavar = "FILEPATH"
    )
  )
  
  opt_parser = optparse::OptionParser(option_list = option_list)
  opt = optparse::parse_args(opt_parser)
  
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
  checkmate::assert_file(opt$metrics,
                         'r',
                         extension = c('csv'),
                         add = coll)
  checkmate::assert_path_for_output(opt$outfile, add = coll)
  checkmate::reportAssertions(coll)
  
  return(opt)
}

main <- function() {
  opt <- parse_args()
  
  detailed <- readr::read_csv(
    opt$detailed,
    col_types = readr::cols(
      .default = readr::col_double(),
      Loc1 = readr::col_character(),
      CRType = readr::col_character(),
      gameid = readr::col_character(),
      ConfAbbrev = readr::col_character(),
      TeamSeason1 = readr::col_character(),
      TeamSeason2 = readr::col_character(),
      TeamID1 = readr::col_character(),
      TeamID2 = readr::col_character()
    )
  )
  
  team_metrics <- readr::read_csv(
    opt$metrics,
    col_types = readr::cols(
      .default = readr::col_double(),
      TeamID1 = readr::col_character(),
      TeamSeason1 = readr::col_character()
    )
  ) %>%
    rename(TeamID = TeamID1, TeamSeason = TeamSeason1)
  
  games <- detailed %>%
    filter(Season <= opt$year) %>%
    select(Season:NumOT,
           TeamSeason1,
           TeamSeason2,
           CRType,
           team1loc,
           gameid) %>%
    mutate(team1win = 1 * (Score1 > Score2)) %>%
    left_join(team_metrics %>%
                rename_at(.vars = vars(-Season), .funs = funs(sprintf('%s1', .)))) %>%
    left_join(team_metrics %>%
                rename_at(.vars = vars(-Season), .funs = funs(sprintf('%s2', .)))) %>%
    mutate(is_ncaa_tourney = factor(CRType == 'NCAA', labels = c('No', 'Yes')))
  
  ## Trying to find "Regular Season" games that are equally competitive to games
  ## found in the NCAA tournament
  
  h2o::h2o.init(
    nthreads = -1,
    enable_assertions = FALSE,
    max_mem_size = "16g"
  )
  
  h2o::h2o.removeAll()
  
  train_data <- games %>%
    filter(Season < opt$year)
  
  test_data <- games %>%
    filter(Season == opt$year)
  
  train_hdf <-
    h2o::as.h2o(
      train_data %>%
        select(
          Season,
          Score1,
          Score2,
          team1win,
          is_ncaa_tourney,
          contains("_rglzd")
        ) %>%
        mutate(
          Season = Season - min(Season),
          Spread = Score1 - Score2,
          team1win = factor(team1win)
        ),
      destination_frame = "train_data"
    )
  
  test_hdf <- h2o::as.h2o(
    test_data %>%
      select(
        Season,
        Score1,
        Score2,
        team1win,
        is_ncaa_tourney,
        contains("_rglzd")
      ) %>%
      mutate(
        Season = Season - min(Season),
        Spread = Score1 - Score2,
        team1win = factor(team1win)
      ),
    destination_frame = "test_data"
  )
  
  predictors <- train_data %>%
    select(contains('_rglzd')) %>%
    colnames()
  
  response <- "is_ncaa_tourney"
  
  fit_tourney_games <-
    h2o::h2o.automl(
      x = predictors,
      y = response,
      project_name = "AutoML_Efficiency_Metrics",
      training_frame = train_hdf,
      fold_column = "Season",
      exclude_algos = c("DeepLearning",
                        "GBM",
                        "XGBoost",
                        "DRF",
                        "StackedEnsemble"),
      max_runtime_secs = 1200,
      stopping_rounds = 5,
      keep_cross_validation_predictions = TRUE,
      stopping_metric = "AUC",
      sort_metric = "AUC"
    )
  
  model <-
    h2o::h2o.getModel(tibble::as_tibble(fit_tourney_games@leaderboard)$model_id[1])
  
  threshold <-
    h2o::h2o.find_threshold_by_max_metric(h2o::h2o.performance(model, xval = TRUE), "absolute_mcc")
  
  submission_out <-
    bind_cols(
      train_data %>%
        select(Season, DayNum, TeamID1, TeamID2),
      h2o::h2o.cross_validation_holdout_predictions(model) %>%
        tibble::as_tibble() %>%
        mutate(predict = factor(
          case_when(Yes >= threshold ~ 'Yes',
                    TRUE ~ 'No')
        ))
    )
  
  submission_out %>%
    readr::write_csv(opt$outfile)
  
  try(h2o::h2o.shutdown(prompt = FALSE), silent = TRUE)
  
}

main()
