{{ config(
  materialized = 'view',
  schema = 'public'
) }}

WITH g AS (

  SELECT
    *
  FROM
    {{ ref('game_denorm') }}
),
dt1 AS (
  SELECT
    *
  FROM
    {{ source(
      'sports',
      'dim_team'
    ) }}
),
dt2 AS (
  SELECT
    *
  FROM
    {{ source(
      'sports',
      'dim_team'
    ) }}
),
wmr_a AS (
  SELECT
    *
  FROM
    {{ source(
      'sports',
      'weekly_massey_ratings'
    ) }}
),
wmr_h AS (
  SELECT
    *
  FROM
    {{ source(
      'sports',
      'weekly_massey_ratings'
    ) }}
),
w538r_a AS (
  SELECT
    *
  FROM
    {{ source(
      'sports',
      'weekly_538_ratings'
    ) }}
),
w538r_h AS (
  SELECT
    *
  FROM
    {{ source(
      'sports',
      'weekly_538_ratings'
    ) }}
),
wsr_a AS (
  SELECT
    *
  FROM
    {{ source(
      'sports',
      'weekly_sagarin_ratings'
    ) }}
),
wsr_h AS (
  SELECT
    *
  FROM
    {{ source(
      'sports',
      'weekly_sagarin_ratings'
    ) }}
)
SELECT
  g.game_id,
  g.season_week_id,
  g.season,
  g.week,
  dt1.name AS away_team_name,
  dt2.name AS home_team_name,
  wmr_a.rating_overall AS massey_rating_overall_away,
  wmr_h.rating_overall AS massey_rating_overall_home,
  wmr_a.rating_offense AS massey_rating_offense_away,
  wmr_h.rating_offense AS massey_rating_offense_home,
  wmr_a.rating_defense AS massey_rating_defense_away,
  wmr_h.rating_defense AS massey_rating_defense_home,
  wmr_a.rating_hfa AS massey_rating_hfa_away,
  wmr_h.rating_hfa AS massey_rating_hfa_home,
  w538r_a.rating AS "538_rating_away",
  w538r_h.rating AS "538_rating_home",
  wsr_a.rating AS sagarin_rating_away,
  wsr_h.rating AS sagarin_rating_home,
  CAST(
    CASE
      WHEN g.home_adv THEN 1
      ELSE 0
    END AS DOUBLE PRECISION
  ) AS home_adv,
  g.score_away,
  g.score_home,
  CASE
    WHEN g.score_away = g.score_home THEN 0.5
    WHEN g.score_away > g.score_home THEN 0.0
    WHEN g.score_away < g.score_home THEN 1.0
    ELSE NULL
  END AS game_outcome
FROM
  game_denorm g
  LEFT JOIN dt1 ON g.away_team_id = dt1.id
  LEFT JOIN dt2 ON g.home_team_id = dt2.id
  LEFT JOIN wmr_a ON g.away_team_id = wmr_a.team_id
  AND g.season_week_id = wmr_a.season_week_id
  LEFT JOIN wmr_h ON g.home_team_id = wmr_h.team_id
  AND g.season_week_id = wmr_h.season_week_id
  LEFT JOIN w538r_a ON g.away_team_id = w538r_a.team_id
  AND g.season_week_id = w538r_a.season_week_id
  LEFT JOIN w538r_h ON g.home_team_id = w538r_h.team_id
  AND g.season_week_id = w538r_h.season_week_id
  LEFT JOIN wsr_a ON g.away_team_id = wsr_a.team_id
  AND g.season_week_id = wsr_a.season_week_id
  LEFT JOIN wsr_h ON g.home_team_id = wsr_h.team_id
  AND g.season_week_id = wsr_h.season_week_id
WHERE
  g.score_away IS NOT NULL
  AND g.score_home IS NOT NULL
ORDER BY
  g.game_id
