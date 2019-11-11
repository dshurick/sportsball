{{ config(
  materialized = 'view',
  schema = 'public'
) }}

WITH g AS (

  SELECT
    *
  FROM
    {{ source(
      'sports',
      'game'
    ) }}
),
dsw AS (
  SELECT
    *
  FROM
    {{ source(
      'sports',
      'dim_season_week'
    ) }}
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
ds AS (
  SELECT
    *
  FROM
    {{ source(
      'sports',
      'dim_season'
    ) }}
)
SELECT
  g.id AS game_id,
  dsw.season,
  ds.id AS season_id,
  dsw.week,
  dt1.name AS away_team_name,
  dt2.name AS home_team_name,
  g.season_week_id,
  g.away_team_id,
  g.home_team_id,
  g.home_adv,
  g.score_away,
  g.score_home
FROM
  g
  LEFT JOIN dsw ON g.season_week_id = dsw.id
  LEFT JOIN dt1 ON g.away_team_id = dt1.id
  LEFT JOIN dt2 ON g.home_team_id = dt2.id
  LEFT JOIN ds ON dsw.season_id = ds.id
