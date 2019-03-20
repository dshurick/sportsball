#!/usr/local/bin/ Rscript

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
      c("-y", "--year"),
      type = "integer",
      default = NULL,
      help = "Which year to produce predictions.",
      metavar = "integer"
    ),
    optparse::make_option(
      c("--detailed"),
      type = "character",
      dest = "detailed",
      default = NULL,
      help = "Location of file containing detailed game data.",
      metavar = "filepath"
    ),
    optparse::make_option(
      c("--metrics"),
      type = "character",
      dest = "metrics",
      default = NULL,
      help = "Location of file containing adjusted team metrics.",
      metavar = "filepath"
    ),
    optparse::make_option(
      c("--tourney_similarity"),
      type = "character",
      dest = "tourney_similarity",
      default = NULL,
      help = "Location of file containing adjusted team metrics.",
      metavar = "filepath"
    ),
    optparse::make_option(
      c("--subm"),
      type = "character",
      default = NULL,
      help = "Location of sample submission.",
      metavar = "filepath"
    ),
    optparse::make_option(
      c("--outfile"),
      type = "character",
      default = NULL,
      help = "Location of output file.",
      metavar = "filepath"
    ),
    optparse::make_option(
      c("-i", "--interactions"),
      action = "store_true",
      type = "logical",
      default = FALSE,
      help = "Include all pairwise interaction terms?",
      metavar = "logical"
    ),
    optparse::make_option(
      c("--spread"),
      action = "store_true",
      type = "logical",
      default = FALSE,
      help = "Include all pairwise interaction terms?",
      metavar = "logical"
    ),
    optparse::make_option(
      c("--tourneyprob"),
      type = "double",
      default = NULL,
      help = "Will include non-NCAA games if similarity is greater than this threshhold.",
      metavar = "double"
    )
  )
  
  opt_parser = optparse::OptionParser(option_list = option_list)
  opt = optparse::parse_args(opt_parser)
  
  return(opt)
}

load_submission_sample_stage1 <- function(file) {
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_file(file, 'r', extension = c('csv'), add = coll)
  checkmate::reportAssertions(coll)
  
  SampleSubmissionStage1 <-
    readr::read_csv(file,
                    col_types = readr::cols(
                      ID = readr::col_character(),
                      Pred = readr::col_double()
                    ))
  
  ret <- bind_cols(SampleSubmissionStage1 %>%
                     select(ID),
                   SampleSubmissionStage1 %>%
                     do({
                       tibble::as_tibble(stringr::str_split(.$ID,
                                                            pattern = "_",
                                                            simplify = TRUE)) %>%
                         rename(Season = V1,
                                TeamID1 = V2,
                                TeamID2 = V3) %>%
                         mutate(Season = as.numeric(Season))
                     }))
  
  return(ret)
}

checkOptionalFile = function(x, ...) {
  if (is.null(x))
    return(TRUE)
  return(checkmate::check_file_exists(x, ...))
}

assert_optional_file <-
  assertOptionalFile <-
  checkmate::makeAssertionFunction(checkOptionalFile)

load_game_data <-
  function(year,
           file_detailed,
           file_metrics,
           file_tourney_similarity = NULL) {
    
    coll <- checkmate::makeAssertCollection()
    checkmate::assert_number(year, lower = 2003, upper = 2999, add = coll)
    checkmate::assert_file(file_detailed,
                           'r',
                           extension = c('csv'),
                           add = coll)
    checkmate::assert_file(file_metrics,
                           'r',
                           extension = c('csv'),
                           add = coll)
    assert_optional_file(file_tourney_similarity,
                         'r',
                         extension = c('csv'),
                         add = coll)
    checkmate::reportAssertions(coll)
    
    detailed <- readr::read_csv(
      file_detailed,
      col_types = readr::cols_only(
        Season = readr::col_double(),
        DayNum = readr::col_double(),
        TeamID1 = readr::col_character(),
        Score1 = readr::col_double(),
        TeamID2 = readr::col_character(),
        Score2 = readr::col_double(),
        Loc1 = readr::col_character(),
        CRType = readr::col_character(),
        gameid = readr::col_character(),
        team1loc = readr::col_double()
      )
    )
    
    team_metrics <- readr::read_csv(
      file_metrics,
      col_types = readr::cols(
        .default = readr::col_double(),
        TeamID1 = readr::col_character(),
        TeamSeason1 = readr::col_character()
      )
    ) %>%
      rename(TeamID = TeamID1, TeamSeason = TeamSeason1) %>%
      select(-Adj_Tempo_Def, -Adj_Tempo_Def_rglzd, -TeamSeason)
    
    if (!is.null(file_tourney_similarity)) {
      tourney_like_games <- readr::read_csv(
        file_tourney_similarity,
        col_types = readr::cols(
          TeamID1 = readr::col_character(),
          TeamID2 = readr::col_character()
        )
      )
      
      detailed <- detailed %>%
        left_join(
          tourney_like_games %>%
            rename(is_like_tourney = predict),
          by = c("Season", "DayNum", "TeamID1", "TeamID2")
        )
    }
    
    games <- detailed %>%
      mutate(team1win = 1 * (Score1 > Score2)) %>%
      left_join(team_metrics %>%
                  rename_at(.vars = vars(-Season), .funs = funs(sprintf('%s1', .))),
                by = c("Season", "TeamID1")) %>%
      left_join(team_metrics %>%
                  rename_at(.vars = vars(-Season), .funs = funs(sprintf('%s2', .))),
                by = c("Season", "TeamID2"))
    
    return(list(games = games, team_metrics = team_metrics))
    
  }

nonzeroCoef <- function(object) {
  cffnts <- h2o::h2o.coef(object)
  return(setdiff(names(cffnts)[cffnts != 0], "Intercept"))
}

main <- function() {
  opt <- parse_args()
  # 
  # opt <-
  #   list(
  #     outfile = "data/mens-machine-learning-competition-2019/processed/Stage2/submissions/model03/submission_2019.csv",
  #     tourney_similarity = "data/mens-machine-learning-competition-2019/processed/Stage2/tourney_like_games/tourney_like_games_2019.csv",
  #     subm = "data/mens-machine-learning-competition-2019/raw/Stage2DataFiles/SampleSubmissionStage2.csv",
  #     metrics = "data/mens-machine-learning-competition-2019/processed/Stage2/team_metrics/team_metrics_2019.csv",
  #     detailed = "data/mens-machine-learning-competition-2019/processed/Stage2/games_detailed.csv",
  #     year = 2019,
  #     interactions = TRUE,
  #     spread = TRUE,
  #     tourneyprob = 0.0327
  #   )
  
  if (!checkmate::testPathForOutput(opt$outfile))
    dir.create(dirname(opt$outfile), recursive = TRUE)
  
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_number(
    opt$tourneyprob,
    null.ok = TRUE,
    lower = 0,
    upper = 1,
    add = coll
  )
  checkmate::assert_number(opt$year,
                           lower = 2003,
                           upper = 2999,
                           add = coll)
  checkmate::assert_logical(opt$interactions, add = coll)
  checkmate::assert_logical(opt$spread, add = coll)
  checkmate::assert_path_for_output(opt$outfile, add = coll)
  checkmate::reportAssertions(coll)
  
  submission_file <- load_submission_sample_stage1(opt$subm)
  
  gmdata <- load_game_data(
    year = opt$year,
    file_detailed = opt$detailed,
    file_metrics = opt$metrics,
    file_tourney_similarity = opt$tourney_similarity
  )
  
  games <- gmdata$games
  team_metrics <- gmdata$team_metrics
  
  if (!is.null(opt$tourneyprob)) {
    train_data <- games %>%
      filter(Season < opt$year, is_like_tourney == 'Yes' |
               CRType == 'NCAA')
  } else {
    train_data <- games %>%
      filter(Season < opt$year, CRType == 'NCAA')
  }
  
  predictors <- games %>%
    select(contains("_rglzd")) %>%
    colnames()
  
  # if (sum(train_data$team1loc != 0) > 0)
  #   predictors <- c(predictors, "team1loc")
  
  submission <- submission_file %>%
    mutate(team1loc = 0) %>%
    filter(Season == opt$year) %>%
    left_join(team_metrics %>%
                rename_at(.vars = vars(-Season), .funs = funs(sprintf('%s1', .))),
              by = c("Season", "TeamID1")) %>%
    left_join(team_metrics %>%
                rename_at(.vars = vars(-Season), .funs = funs(sprintf('%s2', .))),
              by = c("Season", "TeamID2"))
  
  if (opt$interactions) {
    fmla <-
      as.formula(sprintf("~ team1loc + (%s)^2 - 1", paste(predictors, collapse = " + ")))
    
    train_data <-
      bind_cols(train_data,
                tibble::as_tibble(model.matrix(fmla, train_data)) %>%
                  select(contains(":")))
    
    submission <-
      bind_cols(submission,
                tibble::as_tibble(model.matrix(fmla, submission)) %>%
                  select(contains(":")))
  }
  
  h2o::h2o.init(max_mem_size = "16g")
  
  train_hdf <-
    h2o::as.h2o(
      train_data %>%
        select(
          Season,
          Score1,
          Score2,
          team1loc,
          team1win,
          contains("_rglzd")
        ) %>%
        mutate(
          Season = Season - min(Season),
          Spread = Score1 - Score2,
          team1win = factor(team1win)
        ),
      destination_frame = "train_data"
    )
  
  submission_hdf <-
    h2o::as.h2o(submission, destination_frame = "submission_hdf")
  
  predictors <- train_data %>%
    select(contains("_rglzd"), contains("team1loc")) %>%
    colnames()
  
  if (opt$spread) {
    glm_spread_lasso <- h2o::h2o.glm(
      "glm",
      x = predictors,
      y = "Spread",
      family = "gaussian",
      fold_column = "Season",
      intercept = FALSE,
      alpha = 1.0,
      training_frame = train_hdf,
      lambda_search = TRUE
    )
    
    glm_spread_ridge_grid <- h2o::h2o.grid(
      "glm",
      x = nonzeroCoef(glm_spread_lasso),
      y = "Spread",
      family = "gaussian",
      fold_column = "Season",
      intercept = FALSE,
      keep_cross_validation_predictions = TRUE,
      training_frame = train_hdf,
      lambda_search = TRUE,
      hyper_params = list(alpha = seq(0, 1, by = 0.2))
    )
    
    glm_spread_ridge <-
      h2o::h2o.getModel(glm_spread_ridge_grid@model_ids[[1]])
    
    train_hdf <-
      h2o.cbind(train_hdf,
                h2o::h2o.cross_validation_holdout_predictions(glm_spread_ridge))
    
    glm_binomial_grid <- h2o::h2o.grid(
      "glm",
      x = 'predict',
      y = 'team1win',
      family = "binomial",
      fold_column = "Season",
      intercept = FALSE,
      keep_cross_validation_predictions = TRUE,
      training_frame = train_hdf,
      lambda_search = TRUE,
      hyper_params = list(alpha = seq(0, 1, by = 0.2))
    )
    
    glm_final <- h2o::h2o.getModel(glm_binomial_grid@model_ids[[1]])
    
    submission_hdf <-
      h2o::h2o.cbind(submission_hdf,
                     predict(glm_spread_ridge, submission_hdf))
    
  } else {
    glm_lasso <- h2o::h2o.glm(
      "glm",
      x = predictors,
      y = "team1win",
      family = "binomial",
      fold_column = "Season",
      intercept = FALSE,
      alpha = 1.0,
      training_frame = train_hdf,
      lambda_search = TRUE
    )
    
    glm_ridge_grid <- h2o::h2o.grid(
      "glm",
      x = nonzeroCoef(glm_lasso),
      y = "team1win",
      family = "binomial",
      fold_column = "Season",
      intercept = FALSE,
      keep_cross_validation_predictions = TRUE,
      training_frame = train_hdf,
      lambda_search = TRUE,
      hyper_params = list(alpha = seq(0, 1, by = 0.2))
    )
    
    glm_final <- h2o::h2o.getModel(glm_ridge_grid@model_ids[[1]])
    
  }
  
  y_hat <- predict(glm_final, submission_hdf)
  
  submission_out <-
    bind_cols(submission, tibble::as_tibble(y_hat)) %>%
    select(ID, p1) %>%
    rename(Pred = p1)
  
  submission_out %>%
    readr::write_csv(path = opt$outfile)
  
  try(h2o.shutdown(prompt = FALSE), silent = TRUE)
  
}

main()

