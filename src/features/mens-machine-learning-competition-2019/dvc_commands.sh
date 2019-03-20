
dvc run -d data/mens-machine-learning-competition-2019/raw/DataFiles/RegularSeasonDetailedResults.csv \
    -d data/mens-machine-learning-competition-2019/raw/DataFiles/NCAATourneyDetailedResults.csv \
    -d data/mens-machine-learning-competition-2019/raw/DataFiles/ConferenceTourneyGames.csv \
    -o data/mens-machine-learning-competition-2019/processed/games_detailed.csv \
    Rscript make_detailed.R

dvc run -d data/mens-machine-learning-competition-2019/processed/games_detailed.csv \
    -o data/mens-machine-learning-competition-2019/processed/team_metrics/team_metrics_2014.csv \
    -o data/mens-machine-learning-competition-2019/processed/team_metrics/team_metrics_2015.csv \
    -o data/mens-machine-learning-competition-2019/processed/team_metrics/team_metrics_2016.csv \
    -o data/mens-machine-learning-competition-2019/processed/team_metrics/team_metrics_2017.csv \
    -o data/mens-machine-learning-competition-2019/processed/team_metrics/team_metrics_2018.csv \
    Rscript create_team_metrics.R

for i in `seq 2014 2018`;
do
  dvc run -d data/mens-machine-learning-competition-2019/processed/games_detailed.csv \
      -o data/mens-machine-learning-competition-2019/processed/team_metrics/team_metrics_${i}.csv \
      "Rscript create_team_metrics.R --year=${i} --detailed=data/mens-machine-learning-competition-2019/processed/games_detailed.csv --out=data/mens-machine-learning-competition-2019/processed/team_metrics/team_metrics_${i}.csv"
done
    
for i in `seq 2014 2018`;
do
  dvc run -d data/mens-machine-learning-competition-2019/processed/games_detailed.csv \
      -d data/mens-machine-learning-competition-2019/processed/team_metrics/team_metrics_${i}.csv \
      -o data/mens-machine-learning-competition-2019/processed/tourney_like_games/tourney_like_games_${i}.csv \
      "Rscript predict_similar_tourney_games.R --year=${i} --detailed=data/mens-machine-learning-competition-2019/processed/games_detailed.csv --metrics=data/mens-machine-learning-competition-2019/processed/team_metrics/team_metrics_${i}.csv --out=data/mens-machine-learning-competition-2019/processed/tourney_like_games/tourney_like_games_${i}.csv"
done

for i in `seq 2014 2018`;
do
  dvc run -d data/mens-machine-learning-competition-2019/processed/games_detailed.csv \
      -d data/mens-machine-learning-competition-2019/processed/team_metrics/team_metrics_${i}.csv \
      -d data/mens-machine-learning-competition-2019/raw/DataFiles/SampleSubmissionStage1.csv \
      -d data/mens-machine-learning-competition-2019/processed/tourney_like_games/tourney_like_games_${i}.csv \
      -o data/mens-machine-learning-competition-2019/processed/submissions/model06/submission_${i}.csv \
      Rscript modelling.R --year=${i} \
        --detailed=data/mens-machine-learning-competition-2019/processed/games_detailed.csv \
        --metrics=data/mens-machine-learning-competition-2019/processed/team_metrics/team_metrics_${i}.csv \
        --subm=data/mens-machine-learning-competition-2019/raw/DataFiles/SampleSubmissionStage1.csv \
        --tourney_similarity=data/mens-machine-learning-competition-2019/processed/tourney_like_games/tourney_like_games_${i}.csv \
        --outfile=data/mens-machine-learning-competition-2019/processed/submissions/model06/submission_${i}.csv \
        --interactions --spread --tourneyprob=0.0327
done


      