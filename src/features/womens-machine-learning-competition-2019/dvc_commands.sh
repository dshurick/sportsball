#!/bin/bash

DVCROOT=$(dvc root)

# Set up empty directory structure
PROCESSEDPATH=${DVCROOT}/data/processed/womens-machine-learning-competition-2019/Stage2
STAGE2DATAFILESPATH=${DVCROOT}/data/raw/womens-machine-learning-competition-2019/Stage2WDataFiles

TEAMMETRICSPATH=${PROCESSEDPATH}/team_metrics/recency
TOURNEYGAMESPATH=${PROCESSEDPATH}/tourney_like_games/recency
SUBMISSIONSPATH=${PROCESSEDPATH}/submissions
SRCFEATURES=${DVCROOT}/src/features/womens-machine-learning-competition-2019

mkdir -p ${PROCESSEDPATH}
mkdir -p ${TEAMMETRICSPATH}
mkdir -p ${TOURNEYGAMESPATH}
mkdir -p ${SUBMISSIONSPATH}

dvc run -d ${STAGE2DATAFILESPATH}/WRegularSeasonDetailedResults.csv \
  -d ${STAGE2DATAFILESPATH}/WNCAATourneyDetailedResults.csv \
  -o ${PROCESSEDPATH}/games_detailed.csv \
  -f ${PROCESSEDPATH}/games_detailed.csv.dvc \
  Rscript ${SRCFEATURES}/make_detailed.R \
    --regssn=${STAGE2DATAFILESPATH}/WRegularSeasonDetailedResults.csv \
    --ncaatourney=${STAGE2DATAFILESPATH}/WNCAATourneyDetailedResults.csv \
    --outfile=${PROCESSEDPATH}/games_detailed.csv

for i in $(seq 2014 2019); do
  dvc run -d ${PROCESSEDPATH}/games_detailed.csv \
    -o ${TEAMMETRICSPATH}/team_metrics_${i}.csv \
    -f ${TEAMMETRICSPATH}/team_metrics_${i}.csv.dvc \
    Rscript ${SRCFEATURES}/create_team_metrics.R \
      --year=${i} \
      --detailed=${PROCESSEDPATH}/games_detailed.csv \
      --outfile=${TEAMMETRICSPATH}/team_metrics_${i}.csv
done

for i in $(seq 2014 2019); do
  dvc run -d ${PROCESSEDPATH}/games_detailed.csv \
    -d ${TEAMMETRICSPATH}/team_metrics_${i}.csv \
    -o ${TOURNEYGAMESPATH}/tourney_like_games_${i}.csv \
    -f ${TOURNEYGAMESPATH}/tourney_like_games_${i}.csv.dvc \
    Rscript ${SRCFEATURES}/predict_similar_tourney_games.R \
      --year=${i} \
      --detailed=${PROCESSEDPATH}/games_detailed.csv \
      --metrics=${TEAMMETRICSPATH}/team_metrics_${i}.csv \
      --outfile=${TOURNEYGAMESPATH}/tourney_like_games_${i}.csv
done
