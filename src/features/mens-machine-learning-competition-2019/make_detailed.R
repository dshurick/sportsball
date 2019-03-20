
suppressWarnings({
  suppressMessages({
    library(dplyr)
    library(readr)
    library(stringr)
    library(checkmate)
  })
})


parse_args <- function() {
  option_list <- list(
    optparse::make_option(
      c("--regssn"),
      type = "character",
      default = NULL,
      help = "File path containing RegularSeasonDetailedResults.csv",
      metavar = "filepath"
    ),
    optparse::make_option(
      c("--ncaatourney"),
      type = "character",
      default = NULL,
      help = "File path containing NCAATourneyDetailedResults.csv",
      metavar = "filepath"
    ),
    optparse::make_option(
      c("--conftourney"),
      type = "character",
      default = NULL,
      help = "File path containing ConferenceTourneyGames.csv",
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

main <- function() {
  opt <- parse_args()
  
  if (!checkmate::testPathForOutput(opt$outfile))
    dir.create(dirname(opt$outfile))
  
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_file(opt$regssn,
                         'r',
                         extension = c('csv'),
                         add = coll)
  checkmate::assert_file(opt$ncaatourney,
                         'r',
                         extension = c('csv'),
                         add = coll)
  checkmate::assert_file(opt$conftourney,
                         'r',
                         extension = c('csv'),
                         add = coll)
  checkmate::assert_path_for_output(opt$outfile, add = coll)
  checkmate::reportAssertions(coll)
  
  season_dresults <-
    readr::read_csv(opt$regssn,
                    col_types = cols(.default = col_double(),
                                     WLoc = col_character())) %>%
    mutate(CRType = 'Regular')
  
  tourney_dresults <-
    readr::read_csv(opt$ncaatourney,
                    col_types = cols(.default = col_double(),
                                     WLoc = col_character())) %>%
    mutate(CRType = 'NCAA')
  
  ConferenceTourneyGames <- readr::read_csv(opt$conftourney)
  
  .detailed <- bind_rows(season_dresults, tourney_dresults) %>%
    mutate(gameid = paste(
      Season,
      formatC(
        DayNum,
        width = 3,
        format = "d",
        flag = "0"
      ),
      pmin(WTeamID, LTeamID),
      pmax(WTeamID, LTeamID),
      sep = "_"
    )) %>%
    dplyr::left_join(ConferenceTourneyGames) %>%
    dplyr::mutate(
      WTeamID = as.character(WTeamID),
      LTeamID = as.character(LTeamID),
      CRType = case_when(is.na(ConfAbbrev) ~ CRType,
                         TRUE ~ 'ConfTourney')
    )
  
  
  detailed <- bind_rows(
    .detailed %>%
      rename_at(
        .vars = vars(starts_with("W"), -WLoc),
        .funs = funs(sprintf("%s1", str_sub(., 2)))
      ) %>%
      rename_at(
        .vars = vars(starts_with("L"), -WLoc),
        .funs = funs(sprintf("%s2", str_sub(., 2)))
      ) %>%
      rename_at(.vars = vars(WLoc), .funs = funs({
        "Loc1"
      })),
    .detailed %>%
      rename_at(
        .vars = vars(starts_with("W"), -WLoc),
        .funs = funs(sprintf("%s2", str_sub(., 2)))
      ) %>%
      rename_at(
        .vars = vars(starts_with("L"), -WLoc),
        .funs = funs(sprintf("%s1", str_sub(., 2)))
      ) %>%
      rename_at(.vars = vars(WLoc), .funs = funs({
        "Loc1"
      })) %>%
      mutate(Loc1 = case_when(
        Loc1 == 'H' ~ 'A',
        Loc1 == 'A' ~ 'H',
        TRUE ~ Loc1
      ))
  ) %>%
    mutate(
      Team1Win = 1 * (Score1 > Score2),
      Loc1 = factor(Loc1, levels = c('A', 'N', 'H')),
      TeamSeason1 = paste(Season, TeamID1, sep = "_"),
      TeamSeason2 = paste(Season, TeamID2, sep = "_"),
      poss = 0.96 * ((FGA1 + 0.475 * FTA1 - OR1 + TO1) / 2 + (FGA2 + 0.475 * FTA2 - OR2 + TO2) / 2),
      oe1 = 100 * Score1 / poss,
      oe2 = 100 * Score2 / poss,
      de1 = oe2,
      de2 = oe1,
      eFG1 = (.5 * FGM31 + FGM1) / FGA1,
      eFG2 = (.5 * FGM32 + FGM2) / FGA2,
      topct1 = TO1 / poss,
      topct2 = TO2 / poss,
      orpct1 = OR1 / (OR1 + DR2),
      orpct2 = OR2 / (OR2 + DR1),
      ftrate1 = FTA1 / FGA1,
      ftrate2 = FTA2 / FGA2,
      shotopp1 = (FGA1 + 0.475 * FTA1) / poss,
      shotopp2 = (FGA2 + 0.475 * FTA2) / poss,
      tspct1 = Score1 / (2 * (FGA1 + 0.475 * FTA1)),
      tspct2 = Score2 / (2 * (FGA2 + 0.475 * FTA2)),
      tempo = 40 * poss / (40 + 5 * NumOT),
      team1loc = as.numeric(Loc1) - 2
    )
  
  readr::write_csv(detailed,
                   opt$outfile,
                   na = "")
  
  print("Done!")
  
}

main()
