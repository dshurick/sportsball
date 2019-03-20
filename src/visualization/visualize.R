


library(dplyr)

submission_2019 <-
  read_csv(
    "data/processed/mens-machine-learning-competition-2019/Stage2/submissions/model01/submission_2019.csv"
  )

Teams <-
  read_csv("data/raw/mens-machine-learning-competition-2019/Stage2DataFiles/Teams.csv") %>%
  select(TeamID, TeamName)

ret <- bind_cols(submission_2019,
                 submission_2019 %>%
                   do({
                     tibble::as_tibble(stringr::str_split(.$ID,
                                                          pattern = "_",
                                                          simplify = TRUE)) %>%
                       rename(Season = V1,
                              TeamID1 = V2,
                              TeamID2 = V3) %>%
                       mutate(Season = as.numeric(Season))
                   })) %>%
  mutate(TeamID1 = as.numeric(TeamID1),
         TeamID2 = as.numeric(TeamID2)) %>%
  left_join(Teams %>%
              rename(TeamID1 = TeamID, TeamName1 = TeamName)) %>%
  left_join(Teams %>%
              rename(TeamID2 = TeamID, TeamName2 = TeamName))

ret %>%
  readr::write_csv(
    './data/processed/mens-machine-learning-competition-2019/Stage2/submissions/model01.csv'
  )
