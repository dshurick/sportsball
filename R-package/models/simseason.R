
library(tidyverse)
library(googlesheets)
library(shurtools)

nfl_games_2018 <- read_csv("data/external/nfl_games_2018.csv")

sheet <- gs_title("NFL 2018 Expected Wins")

Season2018 <-
  gs_read(
    sheet,
    ws = "Season2018",
    col_types = cols(
      .default = col_number(),
      away_team = col_character(),
      home_team = col_character()
    )
  ) %>%
  dplyr::mutate_at(.vars = vars(ends_with("_prob")), .funs = funs(. / 100)) %>%
  dplyr::mutate_at(.vars = vars(away_team, home_team), .funs = factor)

sagarin_ratings <-
  gs_read(sheet,
          ws = "sagarin")

scorex <-
  gs_read(sheet,
          ws = "scorex")

FiveThirtyEight <-
  gs_read(sheet,
          ws = "FiveThirtyEight")

massey <-
  gs_read(sheet,
          ws = "massey")

teaminfo <-
  gs_read(sheet,
          ws = "teaminfo") %>%
  dplyr::mutate(team = factor(team, levels = levels(Season2018$away_team)))

dt <- Season2018

sim_season <- function(dt) {
  dt <- dt %>%
    dplyr::mutate(simresult = as.numeric(rbinom(
      length(custom_home_prob),
      1,
      custom_home_prob
    ))) %>%
    dplyr::mutate(home_win = case_when(is.na(home_win) ~ simresult,
                                       TRUE ~ home_win))
  dt
}

dt <- dt %>%
  dplyr::mutate(simresult = as.numeric(rbinom(
    length(custom_home_prob),
    1,
    custom_home_prob
  ))) %>%
  dplyr::mutate(home_win = case_when(is.na(home_win) ~ simresult,
                                     TRUE ~ home_win))

calculate_records <- function(.tbl) {
  dplyr::bind_rows(
    .tbl %>%
      dplyr::select(team = away_team, home_win) %>%
      dplyr::mutate(home_win = 1 - home_win),
    .tbl %>%
      dplyr::select(team = home_team, home_win)
  ) %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(nwins = sum(home_win),
                     nlosses = sum(1 - home_win)) %>%
    dplyr::mutate(win_pct = nwins / (nwins + nlosses))
}

nwinners_bydivision <- function(.tbl) {
  .tbl %>%
    dplyr::group_by(Division) %>%
    dplyr::summarise(nwinners = sum(win_division))
}

determine_division_winners <- function(.tbl) {
  
  records <- .tbl %>%
    calculate_records() %>%
    dplyr::left_join(teaminfo, by = "team") %>%
    dplyr::group_by(Division) %>%
    dplyr::mutate(win_division = nwins == max(nwins)) %>%
    dplyr::ungroup()
  
  num_division_winners <- records %>%
    nwinners_bydivision()
  
  division_winners <- records %>%
    dplyr::inner_join(num_division_winners %>%
                        dplyr::filter(nwinners == 1), by = "Division") %>%
    dplyr::filter(win_division == TRUE) %>%
    dplyr::select(team, Conference, Division)
  
  # Determine divisions with ties
  ties <- records %>%
    dplyr::inner_join(num_division_winners %>%
                        dplyr::filter(nwinners > 1), by = "Division") %>%
    dplyr::filter(win_division == TRUE) %>%
    dplyr::select(team, Conference, Division)
  
  if (nrow(ties) > 0) {
    head_to_head_records <- ties %>%
      dplyr::group_by(Division) %>%
      dplyr::do({
        champion_teams <- as.character(.$team)
        
        .tbl %>%
          dplyr::filter(
            as.character(away_team) %in% champion_teams,
            as.character(home_team) %in% champion_teams
          )
      }) %>%
      dplyr::ungroup() %>%
      calculate_records() %>%
      dplyr::left_join(teaminfo, by = "team") %>%
      dplyr::group_by(Division) %>%
      dplyr::mutate(win_division = nwins == max(nwins)) %>%
      dplyr::ungroup()
    
    num_division_winners <- head_to_head_records %>%
      nwinners_bydivision()
    
    division_winners <-
      dplyr::bind_rows(
        division_winners,
        head_to_head_records %>%
          dplyr::inner_join(num_division_winners %>%
                              dplyr::filter(nwinners == 1), by = "Division") %>%
          dplyr::filter(win_division == TRUE) %>%
          dplyr::select(team, Conference, Division)
      )
    
    # Determine divisions with ties
    ties <- head_to_head_records %>%
      dplyr::inner_join(num_division_winners %>%
                          dplyr::filter(nwinners > 1), by = "Division") %>%
      dplyr::filter(win_division == TRUE) %>%
      dplyr::select(team, Conference, Division)
    
    
    if (nrow(ties) > 0) {
      # Best won-lost-tied percentage in games played within the division.
      
      games_with_divisions <- .tbl %>%
        dplyr::select(away_team, home_team, home_win) %>%
        dplyr::left_join(teaminfo %>%
                           dplyr::select(away_team = team, away_division = Division),
                         by = "away_team") %>%
        dplyr::left_join(teaminfo %>%
                           dplyr::select(home_team = team, home_division = Division),
                         by = "home_team")
      
      division_records <- ties %>%
        dplyr::group_by(Division) %>%
        dplyr::do({
          .division <- unique(.$Division)
          .teams <- .$team
          games_with_divisions %>%
            dplyr::filter(
              away_division %in% .division,
              home_division %in% .division,
              away_team %in% .teams | home_team %in% .teams
            )
        }) %>%
        dplyr::ungroup() %>%
        calculate_records() %>%
        dplyr::inner_join(ties %>%
                            dplyr::select(team, Conference, Division), by = "team") %>%
        dplyr::group_by(Division) %>%
        dplyr::mutate(win_division = nwins == max(nwins)) %>%
        dplyr::ungroup()
      
      num_division_winners <- division_records %>%
        nwinners_bydivision()
      
      division_winners <-
        dplyr::bind_rows(
          division_winners,
          division_records %>%
            dplyr::inner_join(
              num_division_winners %>%
                dplyr::filter(nwinners == 1),
              by = "Division"
            ) %>%
            dplyr::filter(win_division == TRUE) %>%
            dplyr::select(team, Conference, Division)
        )
      
      ties <- division_records %>%
        dplyr::inner_join(num_division_winners %>%
                            dplyr::filter(nwinners > 1), by = "Division") %>%
        dplyr::filter(win_division == TRUE) %>%
        dplyr::select(team, Conference, Division)
      
      # Still ties????
      if (nrow(ties) > 0) {
        # Now just select randomly
        
        division_winners <-
          dplyr::bind_rows(division_winners,
                           ties %>%
                             dplyr::group_by(Division) %>%
                             dplyr::sample_n(1))
      }
    }
  }
  return(division_winners)
}

division_winners <- determine_division_winners(dt)

records <- dt %>%
  calculate_records() %>%
  dplyr::left_join(teaminfo, by = "team") %>%
  dplyr::group_by(Conference) %>%
  dplyr::mutate(win_division = nwins == max(nwins)) %>%
  dplyr::ungroup()

num_conference_winners <- records %>%
  dplyr::group_by(Conference) %>%
  dplyr::summarise(nwinners = sum(win_division))

conference_winners <- records %>%
  dplyr::inner_join(num_conference_winners %>%
                      dplyr::filter(nwinners <= 2), by = "Conference") %>%
  dplyr::filter(win_division == TRUE) %>%
  dplyr::select(team, Conference, Division)

# Determine divisions with ties
ties <- records %>%
  dplyr::inner_join(num_division_winners %>%
                      dplyr::filter(nwinners > 1), by = "Division") %>%
  dplyr::filter(win_division == TRUE) %>%
  dplyr::select(team, Conference, Division)
