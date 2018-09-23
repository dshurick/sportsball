
draftPlayer <- function(.tbl,
                        .playerName,
                        .owner = c(
                          "SHOW",
                          "BMB",
                          "FU",
                          "DIPS",
                          "LAD",
                          "ABIB",
                          "GIBA",
                          "ET",
                          "ADH",
                          "WALK",
                          "BBRC",
                          "KENN"
                        ),
                        .paid = 1) {
  .owner <- match.arg(.owner)

  user_exprs <- enquo(.tbl)

  .tbl[.tbl$`Player (Team)` == .playerName, "owner"] <-
    factor(.owner, levels = levels(.tbl$owner))
  .tbl[.tbl$`Player (Team)` == .playerName, "paid"] <- .paid

  assign(
    rlang::as_string(rlang::get_expr(user_exprs)),
    value = .tbl,
    envir = rlang::get_env(user_exprs)
  )
}

reset_draft <- function() {
  appdata <<- appdata %>%
    dplyr::mutate(
      paid = as.numeric(NA),
      owner = factor(
        x = character(n()),
        levels = c(
          "SHOW",
          "BMB",
          "FU",
          "DIPS",
          "LAD",
          "ABIB",
          "GIBA",
          "ET",
          "ADH",
          "WALK",
          "BBRC",
          "KENN"
        )
      )
    )
}

load_appdata <- function() {
  appdata <-
    readr::read_rds("../data/processed/SLO_appdata.rds")
  return(appdata)
}

findPlayer <- function(.tbl, .player) {
  sdist <-
    stringdist::stringdist(.player, .tbl$`Player (Team)`, method = "jw")

  .tbl[order(sdist), ] %>%
    dplyr::slice(1:5) %>%
    dplyr::select(id, `Player (Team)`) %>%
    dplyr::distinct()
}

vor_baseline <- c(
  QB = 20,
  RB = 30,
  WR_TE = 40,
  K = 10,
  DST = 10
)

add_vor <-
  function(.tbl,
             starting_slots = list(
               QB = 24,
               RB = 24,
               WR = 24,
               TE = 12,
               K = 12,
               DST = 12,
               flex = list(
                 RBWR = 12,
                 WRTE = 12
               )
             )) {
    user_exprs <- enquo(.tbl)

    clnames <- colnames(.tbl)

    .tbl <- .tbl %>%
      dplyr::select(one_of(setdiff(
        clnames, c("VOR", "VOR_Ceiling", "VOR_Floor")
      )))

    longtbl <- .tbl %>%
      dplyr::select(
        id,
        `Player (Team)`,
        avg_type,
        pos_equiv,
        owner,
        Points,
        Floor,
        Ceiling
      ) %>%
      tidyr::gather(key = "estimate", value = "points", Points, Floor, Ceiling)

    longtbl <- longtbl %>%
      dplyr::mutate(drafted = !is.na(owner)) %>%
      dplyr::group_by(avg_type, estimate, pos_equiv) %>%
      dplyr::arrange(desc(drafted), desc(points), .by_group = TRUE) %>%
      dplyr::mutate(pos_rank_new = 1:n()) %>%
      dplyr::do({
        pos <- as.character(unique(.$pos_equiv))
        ret <-
          dplyr::mutate(., willBeDrafted = pos_rank_new <= starting_slots[[pos]])
        ret
      }) %>%
      dplyr::ungroup()

    if ("flex" %in% names(starting_slots)) {
      flex <- starting_slots[["flex"]]

      rbwr <- longtbl %>%
        dplyr::filter(willBeDrafted == FALSE) %>%
        dplyr::filter(pos_equiv %in% c("RB", "WR")) %>%
        dplyr::group_by(avg_type, estimate) %>%
        dplyr::arrange(desc(points), .by_group = TRUE) %>%
        dplyr::do({
          dplyr::slice(., 1:12)
        }) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(willBeDrafted = TRUE)

      wrte <- longtbl %>%
        dplyr::filter(willBeDrafted == FALSE) %>%
        dplyr::filter(pos_equiv %in% c("WR", "TE")) %>%
        dplyr::group_by(avg_type, estimate) %>%
        dplyr::arrange(desc(points), .by_group = TRUE) %>%
        dplyr::do({
          dplyr::slice(., 1:12)
        }) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(willBeDrafted = TRUE)
      starting <- dplyr::bind_rows(longtbl %>%
        dplyr::filter(willBeDrafted == TRUE), rbwr, wrte)
    } else {
      starting <- dplyr::bind_rows(longtbl %>%
        dplyr::filter(willBeDrafted == TRUE))
    }

    bench <- longtbl %>%
      dplyr::select(-willBeDrafted) %>%
      dplyr::anti_join(starting, by = c("id", "Player (Team)", "avg_type", "pos_equiv", "owner", "estimate", "points", "drafted", "pos_rank_new")) %>%
      dplyr::filter(pos_equiv %in% c("QB", "RB", "WR", "TE")) %>%
      dplyr::group_by(avg_type, estimate) %>%
      dplyr::arrange(desc(points), .by_group = TRUE) %>%
      dplyr::do({
        dplyr::slice(., 1:84)
      }) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(willBeDrafted = TRUE)

    willBeDrafted <- bind_rows(
      starting %>%
        dplyr::mutate(starter = "starter"),
      bench %>%
        dplyr::mutate(starter = "bench")
    )

    longtbl <- longtbl %>%
      dplyr::select(
        id,
        `Player (Team)`,
        avg_type,
        estimate,
        pos_equiv,
        owner,
        estimate,
        points
      ) %>%
      dplyr::left_join(
        willBeDrafted %>%
          dplyr::select(id, avg_type, estimate, willBeDrafted, starter),
        by = c("id", "avg_type", "estimate")
      )

    vor_values <- longtbl %>%
      dplyr::left_join(
        longtbl %>%
          dplyr::filter(is.na(starter) |
            starter != "starter") %>%
          dplyr::group_by(avg_type, estimate, pos_equiv) %>%
          dplyr::summarise(repl_value = max(points, na.rm = TRUE)) %>%
          ungroup(),
        by = c("avg_type", "estimate", "pos_equiv")
      ) %>%
      dplyr::mutate(VOR = pmax(points - repl_value, 0)) %>%
      dplyr::distinct(id, avg_type, estimate, VOR) %>%
      tidyr::spread(
        key = estimate,
        value = VOR,
        sep = "_",
        fill = 0
      ) %>%
      dplyr::rename(
        VOR_Ceiling = estimate_Ceiling,
        VOR_Floor = estimate_Floor,
        VOR = estimate_Points
      )

    ret <- .tbl %>%
      dplyr::left_join(vor_values, by = c("id", "avg_type"))

    return(ret)
  }

add_costs <- function(.tbl,
                      numTeams = 12,
                      leagueCap = 200) {
  clnames <- colnames(.tbl)

  .tbl <- .tbl %>%
    dplyr::select(one_of(setdiff(
      clnames, c("Cost", "Cost_Ceiling", "Cost_Floor")
    )))

  ret <- .tbl %>%
    dplyr::select(id, avg_type, pos_equiv, owner, starts_with("VOR")) %>%
    tidyr::gather(
      key = "key",
      value = "vor",
      VOR,
      VOR_Floor,
      VOR_Ceiling,
      VOR_static
    )

  totalMoneyPool <- numTeams * leagueCap

  drafted <- .tbl %>%
    dplyr::filter(owner != "") %>%
    dplyr::distinct(id, owner, paid)

  totalMoneySpent <-
    numTeams * 18 + sum(drafted$paid - 1, na.rm = TRUE)

  totalMoneyLeft <- totalMoneyPool - totalMoneySpent

  moneySplit <-
    tibble::tibble(
      pos_equiv = factor(c("QB", "RB", "WR", "TE", "DST", "K")),
      wght = c(0.197, 0.388, 0.357, 0.0512, 0.00514, 0.00128)
    ) %>%
    dplyr::mutate(wght = totalMoneyLeft * wght / sum(wght))

  total_points <- ret %>%
    dplyr::filter(is.na(owner)) %>%
    dplyr::group_by(avg_type, pos_equiv, key) %>%
    dplyr::summarise(total_vor = sum(vor, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(moneySplit) %>%
    dplyr::mutate(costPerPoint = wght / total_vor)

  ret <- ret %>%
    dplyr::left_join(
      total_points %>%
        dplyr::select(avg_type, pos_equiv, key, costPerPoint),
      by = c("avg_type", "pos_equiv", "key")
    ) %>%
    dplyr::mutate(Cost = round(vor * costPerPoint) + 1)

  .tbl %>%
    dplyr::left_join(
      ret %>%
        dplyr::select(id, avg_type, key, Cost) %>%
        dplyr::mutate(
          key = case_when(
            key == "VOR" ~ "Cost",
            key == "VOR_Ceiling" ~ "Cost_Ceiling",
            key == "VOR_static" ~ "Cost_static",
            TRUE ~ "Cost_Floor"
          )
        ) %>%
        tidyr::spread(key, Cost),
      by = c("id", "avg_type")
    ) %>%
    dplyr::mutate_at(
      .vars = vars(Cost, Cost_Ceiling, Cost_Floor, Cost_static),
      .funs = funs(case_when(
        is.na(owner) ~ .,
        TRUE ~ paid
      ))
    )
}

optimizeLineup <- function(.tbl,
                           .owner = c(
                             "SHOW",
                             "BMB",
                             "FU",
                             "DIPS",
                             "LAD",
                             "ABIB",
                             "GIBA",
                             "ET",
                             "ADH",
                             "WALK",
                             "BBRC",
                             "KENN"
                           ),
                           .floorCeiling = c("neither", "floor", "ceiling", "static"),
                           .positions = c(
                             QB = 2,
                             RB = 2,
                             WR = 2,
                             TE = 2,
                             K = 1,
                             DST = 1
                           )) {
  .owner <- match.arg(.owner)
  .floorCeiling <- match.arg(.floorCeiling)

  if (!("VOR" %in% colnames(.tbl))) {
    .tbl <- .tbl %>% add_vor() %>% add_costs()
  } else if (!("Cost" %in% colnames(.tbl))) {
    .tbl <- .tbl %>% add_costs()
  }

  undrafted <- .tbl %>%
    dplyr::filter(is.na(owner) | owner == .owner)

  A <- rbind(
    as.numeric(undrafted$pos_equiv == "QB"),
    # num QB
    as.numeric(undrafted$pos_equiv == "QB"),
    # num QB
    as.numeric(undrafted$pos_equiv == "RB"),
    # num RB
    as.numeric(undrafted$pos_equiv == "RB"),
    # num RB
    as.numeric(undrafted$pos_equiv == "WR"),
    # num WR
    as.numeric(undrafted$pos_equiv == "WR"),
    # num TE
    as.numeric(undrafted$pos_equiv == "TE"),
    # num WR
    as.numeric(undrafted$pos_equiv == "TE"),
    # num WR
    as.numeric(undrafted$pos_equiv == "DST"),
    # num TE
    as.numeric(undrafted$pos_equiv == "K"),
    
    as.numeric(undrafted$pos_equiv %in% c("RB", "WR")),
    as.numeric(undrafted$pos_equiv %in% c("WR", "TE")),
    # num TE
    as.numeric(!is.na(undrafted$owner)),
    undrafted$Cost_static,
    # total cost
    rep(1, length(undrafted$pos_equiv))
  )

  numVars <- ncol(A)

  dir <- c(
    ">=",
    "<=",
    ">=",
    "<=",
    ">=",
    "<=",
    ">=",
    "<=",
    "==",
    "==",
    ">=",
    ">=",
    "==",
    "<=",
    "=="
  )

  b <- c(
    2, # QB
    3, # QB
    2, # RB
    9, # RB
    2, # WR
    9, # WR
    1, # TE
    3, # TE
    1, # DST
    1, # K
    1, # RBWR
    1, # WRTE
    sum(as.numeric(!is.na(undrafted$owner))),
    200,
    18
  )

  obj <- switch(
    .floorCeiling,
    neither = undrafted$points,
    floor = undrafted$Floor,
    ceiling = undrafted$Ceiling,
    static = undrafted$Points
  )

  sol <-
    Rglpk_solve_LP(
      obj = obj,
      mat = A,
      dir = dir,
      rhs = b,
      types = "I",
      max = TRUE,
      bounds = list(
        lower = list(ind = 1:numVars, val = rep(0, numVars)),
        upper = list(ind = 1:numVars, val = rep(1, numVars))
      )
    )

  optimalTeam <- undrafted[which(sol$solution == 1), ]

  return(optimalTeam)
}
