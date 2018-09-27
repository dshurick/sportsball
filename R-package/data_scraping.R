
# Scrape Massey ratings
library(rvest)

page <- read_html('./data/raw/massey_ratings.html')

nodes <- page %>% html_nodes('.tr1 .fShort:nth-child(8) , .tr0 .fShort:nth-child(8) , .frank:nth-child(7) .detail , .frank:nth-child(6) .detail , .sorted .detail , .tan > a') %>% html_text()

data <- nodes %>%
  matrix(nrow = 32, byrow = TRUE) %>%
  tibble::as_data_frame() %>%
  dplyr::mutate(
    team = V1,
    rating = as.numeric(V2),
    off = as.numeric(V3),
    def = as.numeric(V4),
    hfa = as.numeric(V4)
  ) %>%
  dplyr::select(team:hfa)

