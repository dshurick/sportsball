
PROCESSEDPATH=data/mens-machine-learning-competition-2019/processed/Stage2
DATAFILESPATH=data/mens-machine-learning-competition-2019/raw/Stage2DataFiles

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


      