#!/bin/bash

DVCROOT=$(dvc root)

PROCESSEDPATH=${DVCROOT}/data/processed/womens-machine-learning-competition-2019/Stage2
RAWSTAGE1PATH=${DVCROOT}/data/raw/womens-machine-learning-competition-2019/WDataFiles
RAWSTAGE2PATH=${DVCROOT}/data/raw/womens-machine-learning-competition-2019/Stage2WDataFiles

TEAMMETRICSPATH=${PROCESSEDPATH}/team_metrics
TOURNEYGAMESPATH=${PROCESSEDPATH}/tourney_like_games
SUBMISSIONSPATH=${PROCESSEDPATH}/submissions
SRCMODELS=${DVCROOT}/src/models/womens-machine-learning-competition-2019

MODELV="03"
MODELOUTPATH=${SUBMISSIONSPATH}/model${MODELV}
mkdir -p ${MODELOUTPATH}

for i in $(seq 2014 2018); do
  dvc run -d ${PROCESSEDPATH}/games_detailed.csv \
    -d ${TEAMMETRICSPATH}/team_metrics_${i}.csv \
    -d ${RAWSTAGE1PATH}/WSampleSubmissionStage1.csv \
    -d ${TOURNEYGAMESPATH}/tourney_like_games_${i}.csv \
    -o ${MODELOUTPATH}/submission_${i}.csv \
    -f ${MODELOUTPATH}/submission_${i}.csv.dvc \
    Rscript ${SRCMODELS}/modelling.R \
    --year=${i} \
    --detailed=${PROCESSEDPATH}/games_detailed.csv \
    --metrics=${TEAMMETRICSPATH}/team_metrics_${i}.csv \
    --subm=${RAWSTAGE1PATH}/WSampleSubmissionStage1.csv \
    --tourney_similarity=${TOURNEYGAMESPATH}/tourney_like_games_${i}.csv \
    --outfile=${MODELOUTPATH}/submission_${i}.csv \
    --spread --tourneyprob=0.0327
done

dvc run -d ${RAWSTAGE2PATH}/WNCAATourneyDetailedResults.csv \
  -d ${MODELOUTPATH}/submission_2014.csv \
  -d ${MODELOUTPATH}/submission_2015.csv \
  -d ${MODELOUTPATH}/submission_2016.csv \
  -d ${MODELOUTPATH}/submission_2017.csv \
  -d ${MODELOUTPATH}/submission_2018.csv \
  -o ${MODELOUTPATH}/SubmissionStage1.csv \
  -f ${MODELOUTPATH}/SubmissionStage1.csv.dvc \
  --metrics ${MODELOUTPATH}/eval.json \
  Rscript ${SRCMODELS}/evaluate.R \
  --submissionpath=${MODELOUTPATH} \
  --outfile=${MODELOUTPATH}/SubmissionStage1.csv \
  --ncaatourney=${RAWSTAGE2PATH}/WNCAATourneyDetailedResults.csv \
  --metricsfile=${MODELOUTPATH}/eval.json

dvc metrics modify -t json -x "LogLoss" ${MODELOUTPATH}/eval.json

dvc run -d ${PROCESSEDPATH}/games_detailed.csv \
  -d ${TEAMMETRICSPATH}/team_metrics_2019.csv \
  -d ${RAWSTAGE2PATH}/WSampleSubmissionStage2.csv \
  -d ${TOURNEYGAMESPATH}/tourney_like_games_2019.csv \
  -o ${MODELOUTPATH}/submission_2019.csv \
  -f ${MODELOUTPATH}/submission_2019.csv.dvc \
  Rscript ${SRCMODELS}/modelling.R --year=2019 \
  --detailed=${PROCESSEDPATH}/games_detailed.csv \
  --metrics=${TEAMMETRICSPATH}/team_metrics_2019.csv \
  --subm=${RAWSTAGE2PATH}/WSampleSubmissionStage2.csv \
  --tourney_similarity=${TOURNEYGAMESPATH}/tourney_like_games_2019.csv \
  --outfile=${MODELOUTPATH}/submission_2019.csv \
  --spread --tourneyprob=0.0327
