


suppressWarnings({
  suppressMessages({
    library(dplyr)
    library(checkmate)
  })
})

parse_args <- function() {
  option_list <- list(
    optparse::make_option(
      c("--submissionpath"),
      type = "character",
      default = NULL,
      help = "TODO",
      metavar = "directory"
    ),
    optparse::make_option(
      c("--outfile"),
      type = "character",
      default = NULL,
      help = "TODO",
      metavar = "filepath"
    ),
    optparse::make_option(
      c("--ncaatourney"),
      type = "character",
      default = NULL,
      help = "TODO",
      metavar = "filepath"
    ),
    optparse::make_option(
      c("--metricsfile"),
      type = "character",
      default = NULL,
      help = "TODO",
      metavar = "filepath"
    )
  )
  
  opt_parser = optparse::OptionParser(option_list = option_list)
  opt = optparse::parse_args(opt_parser)
  
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_directory_exists(opt$submissionpath, access = 'w', add = coll)
  checkmate::assert_file_exists(opt$ncaatourney, access = 'r', add = coll)
  checkmate::assert_path_for_output(opt$outfile, overwrite = TRUE, add = coll)
  checkmate::assert_path_for_output(opt$metricsfile, overwrite = TRUE, add = coll)
  checkmate::reportAssertions(coll)
  
  return(opt)
}


main <- function() {
  
  opt <- parse_args()
  
  opt <-
    list(
      submissionpath = 'data/mens-machine-learning-competition-2019/processed/Stage2/submissions/model03',
      outfile = 'data/mens-machine-learning-competition-2019/processed/Stage2/submissions/model03/SubmissionStage2.csv',
      ncaatourney = "data/mens-machine-learning-competition-2019/raw/Stage2DataFiles/NCAATourneyDetailedResults.csv",
      metricsfile = 'data/mens-machine-learning-competition-2019/processed/Stage2/submissions/model01/eval.json'
    )
  
  submission_files <-
    list.files(
      path = opt$submissionpath,
      pattern = 'submission_\\d{4}.csv$',
      full.names = TRUE
    )
  
  SubmissionStage1 <-
    purrr::map_df(submission_files,
                  ~ readr::read_csv(
                    .x,
                    col_types = readr::cols(
                      ID = readr::col_character(),
                      Pred = readr::col_double()
                    )
                  ))
  
  SubmissionStage1 %>%
    readr::write_csv(opt$outfile,
                     na = '')
  
  NCAATourneyDetailedResults <-
    readr::read_csv(
      opt$ncaatourney,
      col_types = readr::cols_only(
        Season = readr::col_double(),
        DayNum = readr::col_double(),
        WTeamID = readr::col_double(),
        WScore = readr::col_double(),
        LTeamID = readr::col_double(),
        LScore = readr::col_double()
      )
    ) %>%
    filter(DayNum >= 136, Season >= 2014)
  
  game_results <- bind_rows(
    NCAATourneyDetailedResults %>%
      filter(WTeamID < LTeamID) %>%
      mutate(
        ID = sprintf('%d_%d_%d', Season, WTeamID, LTeamID),
        Result = 1
      ),
    NCAATourneyDetailedResults %>%
      filter(WTeamID > LTeamID) %>%
      mutate(
        ID = sprintf('%d_%d_%d', Season, LTeamID, WTeamID),
        Result = 0
      )
  ) %>%
    inner_join(SubmissionStage1, by = "ID")
  
  metrics <- with(game_results,
                  list(
                    LogLoss = MLmetrics::LogLoss(Pred, Result),
                    AUC = MLmetrics::AUC(Pred, Result)
                  ))
  
  jsonlite::write_json(metrics, opt$metricsfile)
  
}

main()
