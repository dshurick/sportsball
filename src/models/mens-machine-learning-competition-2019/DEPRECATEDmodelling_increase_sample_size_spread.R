#!/usr/local/bin/ Rscript

library(dplyr)
library(h2o)

# option_list <- list(
#   optparse::make_option(
#     c("-y", "--year"),
#     type = "integer",
#     default = NULL,
#     help = "Which year to produce predictions.",
#     metavar = "integer"
#   ),
#   optparse::make_option(
#     c("-d", "--detailed"),
#     type = "character",
#     default = NULL,
#     help = "Location of file containing detailed game data.",
#     metavar = "character"
#   ),
#   optparse::make_option(
#     c("-m", "--metrics"),
#     type = "character",
#     default = NULL,
#     help = "Location of file containing adjusted team metrics.",
#     metavar = "character"
#   ),
#   optparse::make_option(
#     c("-s", "--subm"),
#     type = "character",
#     default = NULL,
#     help = "Location of sample submission.",
#     metavar = "character"
#   ),
#   optparse::make_option(
#     c("-o", "--out"),
#     type = "character",
#     default = NULL,
#     help = "Location of output file.",
#     metavar = "character"
#   )
# )
# 
# opt_parser = optparse::OptionParser(option_list = option_list)
# opt = optparse::parse_args(opt_parser)

opt <- list(
  year = 2018,
  detailed = file.path(
    "data",
    "mens-machine-learning-competition-2019",
    "processed",
    "games_detailed.csv"
  ),
  metrics = file.path(
    "data",
    "mens-machine-learning-competition-2019",
    "processed",
    "team_metrics",
    "team_metrics_2018.csv"
  ),
  subm = file.path(
    "data",
    "mens-machine-learning-competition-2019",
    "raw",
    "DataFiles",
    "SampleSubmissionStage1.csv"
  ),
  out = file.path(
    "data",
    "mens-machine-learning-competition-2019",
    "processed",
    "submissions",
    "model05",
    "submission_2018.csv"
  )
)

SampleSubmissionStage1 <-
  readr::read_csv(opt$subm)

submission_file <-
  bind_cols(SampleSubmissionStage1 %>%
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

tourney_like_games <- readr::read_csv(
  file.path(
    "data",
    "mens-machine-learning-competition-2019",
    "processed",
    "tourney_like_games",
    "tourney_like_games_2018.csv"
  ),
  col_types = readr::cols(
    TeamID1 = readr::col_character(),
    TeamID2 = readr::col_character()
  )
)

detailed <- detailed %>%
  left_join(tourney_like_games %>%
              rename(is_like_tourney = predict))

games <- detailed %>%
  select(Season:NumOT,
         TeamSeason1,
         TeamSeason2,
         team1loc,
         CRType,
         is_like_tourney,
         Yes) %>%
  mutate(team1win = 1 * (Score1 > Score2)) %>%
  left_join(team_metrics %>%
              select(-Adj_Tempo_Def, -Adj_Tempo_Def_rglzd) %>%
              rename_at(.vars = vars(-Season), .funs = funs(sprintf('%s1', .)))) %>%
  left_join(team_metrics %>%
              select(-Adj_Tempo_Def, -Adj_Tempo_Def_rglzd) %>%
              rename_at(.vars = vars(-Season), .funs = funs(sprintf('%s2', .))))

predictors <- games %>%
  select(contains("_rglzd")) %>%
  colnames()

fmla <-
  as.formula(sprintf("~ team1loc + (%s)^2 - 1", paste(predictors, collapse = " + ")))

train_data <- games %>%
  filter(Season < opt$year, is_like_tourney == 'Yes' | CRType == 'NCAA')

# train_data <- games %>%
#   filter(Season < opt$year, CRType == 'NCAA')

X <- tibble::as_tibble(model.matrix(fmla, train_data)) %>%
  select(contains(":"))

train_data <- bind_cols(train_data, X)

test_data <- games %>%
  filter(Season == opt$year,
         as.numeric(TeamID1) < as.numeric(TeamID2),
         DayNum >= 136)

X <- tibble::as_tibble(model.matrix(fmla, test_data)) %>%
  select(contains(":"))

test_data <- bind_cols(test_data, X)

submission <- submission_file %>%
  mutate(team1loc = 0) %>%
  filter(Season == opt$year) %>%
  left_join(team_metrics %>%
              rename_at(.vars = vars(-Season), .funs = funs(sprintf('%s1', .)))) %>%
  left_join(team_metrics %>%
              rename_at(.vars = vars(-Season), .funs = funs(sprintf('%s2', .))))

submission <-
  bind_cols(submission,
            tibble::as_tibble(model.matrix(fmla, submission)) %>%
              select(contains(":")))

h2o::h2o.init(max_mem_size = "16g")

# h2o::h2o.shutdown()

# train_data %>%
#   select(Season, team1win, contains("_rglzd")) %>%
#   mutate(Season = Season - min(Season))

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

test_hdf <- h2o::as.h2o(
  test_data %>%
    select(Season, Score1, Score2, team1loc, team1win, contains("_rglzd")) %>%
    mutate(
      Season = Season - min(Season),
      Spread = Score1 - Score2,
      team1win = factor(team1win)
    ),
  destination_frame = "test_data"
)

predictors <- train_data %>%
  select(contains("_rglzd"), contains("team1loc")) %>%
  colnames()

response <- "Spread"

# fit <-
#   h2o.automl(
#     x = predictors,
#     y = response,
#     training_frame = train_hdf,
#     fold_column = "Season",
#     max_runtime_secs = 600,
#     stopping_rounds = 5,
#     sort_metric = "logloss"
#   )

# fit2 <-
#   h2o.automl(
#     x = predictors,
#     y = response,
#     training_frame = train_hdf,
#     fold_column = "Season",
#     max_runtime_secs = 1200,
#     stopping_rounds = 5,
#     keep_cross_validation_predictions = TRUE,
#     sort_metric = "logloss"
#   )

# glm_params1 <- list(alpha = seq(0, 1, length.out = 11))

print("Running initial grid search...")

glm_fit1 <- h2o::h2o.glm(
  "glm",
  x = predictors,
  y = response,
  family = "gaussian",
  fold_column = "Season",
  intercept = FALSE,
  alpha = 1.0,
  training_frame = train_hdf,
  lambda_search = TRUE
)

# glm_grid1 <- h2o::h2o.grid(
#   "glm",
#   x = predictors,
#   y = response,
#   family = "binomial",
#   fold_column = "Season",
#   grid_id = "glm_grid1",
#   training_frame = train_hdf,
#   lambda_search = TRUE,
#   hyper_params = glm_params1
# )

# glm_gridperf1 <- h2o::h2o.getGrid(grid_id = "glm_grid1",
#                                   sort_by = "logloss",
#                                   decreasing = FALSE)
#  
# best_glm1 <- h2o::h2o.getModel(glm_gridperf1@model_ids[[1]])

cffnts <- h2o::h2o.coef(glm_fit1)

cffnts.nonZero <- setdiff(names(cffnts)[cffnts != 0], "Intercept")

print("Running 2nd grid search...")

glm_grid2 <- h2o::h2o.grid(
  "glm",
  x = cffnts.nonZero,
  y = response,
  family = "gaussian",
  fold_column = "Season",
  intercept = FALSE,
  grid_id = "glm_grid2",
  keep_cross_validation_predictions = TRUE,
  training_frame = train_hdf,
  lambda_search = TRUE,
  hyper_params = list(alpha = seq(0, 1, by = 0.2))
)

glm_gridperf2 <-
  h2o::h2o.getGrid(grid_id = "glm_grid2",
                   sort_by = "residual_deviance",
                   decreasing = FALSE)

# glm_gridperf2 <- h2o::h2o.getGrid(grid_id = "glm_grid3",
#                                   sort_by = "logloss",
#                                   decreasing = FALSE)

best_glm2 <- h2o::h2o.getModel(glm_gridperf2@model_ids[[1]])

cffnts <- h2o::h2o.coef(best_glm2)

train_hdf <- h2o.cbind(train_hdf, h2o::h2o.cross_validation_holdout_predictions(best_glm2))

# fit3 <-
#   h2o.automl(
#     x = 'predict',
#     y = 'team1win',
#     training_frame = train_hdf,
#     fold_column = "Season",
#     max_runtime_secs = 300,
#     stopping_rounds = 5,
#     keep_cross_validation_predictions = TRUE,
#     sort_metric = "logloss"
#   )
# 
# leader <- fit3@leader

glm_grid3 <- h2o::h2o.grid(
  "glm",
  x = 'predict',
  y = 'team1win',
  family = "binomial",
  fold_column = "Season",
  intercept = FALSE,
  grid_id = "glm_grid3",
  keep_cross_validation_predictions = TRUE,
  training_frame = train_hdf,
  lambda_search = TRUE,
  hyper_params = list(alpha = seq(0, 1, by = 0.2))
)

glm_gridperf3 <-
  h2o::h2o.getGrid(grid_id = "glm_grid3",
                   sort_by = "logloss",
                   decreasing = FALSE)

best_glm3 <- h2o::h2o.getModel(glm_gridperf3@model_ids[[1]])

test_hdf <- h2o.cbind(test_hdf, h2o::h2o.predict(best_glm2, test_hdf))

best_glm_perf2 <- h2o::h2o.performance(model = best_glm3,
                                       newdata = test_hdf)

submission_hdf <-
  h2o::as.h2o(submission, destination_frame = "submission_hdf")

submission_hdf <- h2o::h2o.cbind(submission_hdf, predict(best_glm2, submission_hdf))
y_hat <- predict(best_glm3, submission_hdf)

submission_out <-
  bind_cols(submission, tibble::as_tibble(y_hat)) %>%
  select(ID, p1) %>%
  rename(Pred = p1)

dir.create(dirname(opt$out),
           showWarnings = FALSE,
           recursive = TRUE)

submission_out %>%
  readr::write_csv(path = opt$out)

h2o.shutdown(prompt = FALSE)
