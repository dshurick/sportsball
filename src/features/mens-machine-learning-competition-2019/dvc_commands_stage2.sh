
PROCESSEDPATH=data/mens-machine-learning-competition-2019/processed/Stage2
DATAFILESPATH=data/mens-machine-learning-competition-2019/raw/Stage2DataFiles

mkdir ${PROCESSEDPATH}/team_metrics
mkdir ${PROCESSEDPATH}/tourney_like_games
mkdir ${PROCESSEDPATH}/submissions

dvc run -d ${DATAFILESPATH}/RegularSeasonDetailedResults.csv \
    -d ${DATAFILESPATH}/NCAATourneyDetailedResults.csv \
    -d ${DATAFILESPATH}/ConferenceTourneyGames.csv \
    -o ${PROCESSEDPATH}/games_detailed.csv \
    -f ${PROCESSEDPATH}/games_detailed.csv.dvc \
    Rscript make_detailed.R --regssn=${DATAFILESPATH}/RegularSeasonDetailedResults.csv \
        --ncaatourney=${DATAFILESPATH}/NCAATourneyDetailedResults.csv \
        --conftourney=${DATAFILESPATH}/ConferenceTourneyGames.csv \
        --outfile=${PROCESSEDPATH}/games_detailed.csv

for i in `seq 2014 2019`;
do
  dvc run -d ${PROCESSEDPATH}/games_detailed.csv \
      -o ${PROCESSEDPATH}/team_metrics/team_metrics_${i}.csv \
      -f ${PROCESSEDPATH}/team_metrics/team_metrics_${i}.csv.dvc \
      "Rscript create_team_metrics.R --year=${i} --detailed=${PROCESSEDPATH}/games_detailed.csv --outfile=${PROCESSEDPATH}/team_metrics/team_metrics_${i}.csv"
done

for i in `seq 2014 2019`;
do
  dvc run -d ${PROCESSEDPATH}/games_detailed.csv \
      -d ${PROCESSEDPATH}/team_metrics/team_metrics_${i}.csv \
      -o ${PROCESSEDPATH}/tourney_like_games/tourney_like_games_${i}.csv \
      -f ${PROCESSEDPATH}/tourney_like_games/tourney_like_games_${i}.csv.dvc \
      "Rscript predict_similar_tourney_games.R --year=${i} --detailed=${PROCESSEDPATH}/games_detailed.csv --metrics=${PROCESSEDPATH}/team_metrics/team_metrics_${i}.csv --outfile=${PROCESSEDPATH}/tourney_like_games/tourney_like_games_${i}.csv"
done

MODELV="01"
mkdir ${PROCESSEDPATH}/submissions/model${MODELV}

for i in `seq 2014 2018`;
do
  dvc run -d ${PROCESSEDPATH}/games_detailed.csv \
      -d ${PROCESSEDPATH}/team_metrics/team_metrics_${i}.csv \
      -d data/mens-machine-learning-competition-2019/raw/DataFiles/SampleSubmissionStage1.csv \
      -d ${PROCESSEDPATH}/tourney_like_games/tourney_like_games_${i}.csv \
      -o ${PROCESSEDPATH}/submissions/model${MODELV}/submission_${i}.csv \
      -f ${PROCESSEDPATH}/submissions/model${MODELV}/submission_${i}.csv.dvc \
      Rscript modelling.R --year=${i} \
        --detailed=${PROCESSEDPATH}/games_detailed.csv \
        --metrics=${PROCESSEDPATH}/team_metrics/team_metrics_${i}.csv \
        --subm=data/mens-machine-learning-competition-2019/raw/DataFiles/SampleSubmissionStage1.csv \
        --tourney_similarity=${PROCESSEDPATH}/tourney_like_games/tourney_like_games_${i}.csv \
        --outfile=${PROCESSEDPATH}/submissions/model${MODELV}/submission_${i}.csv \
        --interactions --spread --tourneyprob=0.0327
done

MODELOUTPATH=${PROCESSEDPATH}/submissions/model${MODELV}

dvc run -d ${DATAFILESPATH}/NCAATourneyDetailedResults.csv \
  -d ${MODELOUTPATH}/submission_2014.csv \
  -d ${MODELOUTPATH}/submission_2015.csv \
  -d ${MODELOUTPATH}/submission_2016.csv \
  -d ${MODELOUTPATH}/submission_2017.csv \
  -d ${MODELOUTPATH}/submission_2018.csv \
  -o ${MODELOUTPATH}/SubmissionStage1.csv \
  -f ${MODELOUTPATH}/SubmissionStage1.csv.dvc \
  --metrics ${MODELOUTPATH}/eval.json \
  Rscript evaluate.R \
    --submissionpath=${MODELOUTPATH} \
    --outfile=${MODELOUTPATH}/SubmissionStage1.csv \
    --ncaatourney=${DATAFILESPATH}/NCAATourneyDetailedResults.csv \
    --metricsfile=${MODELOUTPATH}/eval.json


      