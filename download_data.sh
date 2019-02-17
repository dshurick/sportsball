#!/bin/bash

for i in `seq 2010 2018`;
do
    dvc run -o PlayByPlay_${i}.zip \
        -c data/mens-machine-learning-competition-2018/raw/PlayByPlay/zips \
        kaggle competitions download \
        --file PlayByPlay_${i}.zip mens-machine-learning-competition-2018
done

mkdir data/mens-machine-learning-competition-2018/raw/DataFiles/zips

dvc run -o DataFiles.zip \
    -c data/mens-machine-learning-competition-2018/raw/DataFiles/zips \
    kaggle competitions download \
    --file DataFiles.zip mens-machine-learning-competition-2018

mkdir data/mens-machine-learning-competition-2018/raw/PrelimData2018/zips

dvc run -o PrelimData2018.zip \
    -c data/mens-machine-learning-competition-2018/raw/PrelimData2018/zips \
    kaggle competitions download \
    --file PrelimData2018.zip mens-machine-learning-competition-2018

mkdir data/mens-machine-learning-competition-2018/raw/MasseyOrdinals/zips

dvc run -o MasseyOrdinals_thruSeason2018_Day128.zip \
    -c data/mens-machine-learning-competition-2018/raw/MasseyOrdinals/zips \
    kaggle competitions download \
    --file MasseyOrdinals_thruSeason2018_Day128.zip mens-machine-learning-competition-2018

dvc run -o Stage2UpdatedDataFiles.zip \
    -c data/mens-machine-learning-competition-2018/raw/DataFiles/zips \
    kaggle competitions download \
    --file Stage2UpdatedDataFiles.zip mens-machine-learning-competition-2018

dvc run -d zips/DataFiles.zip \
    -o Cities.csv \
    -o Conferences.csv \
    -o ConferenceTourneyGames.csv \
    -o GameCities.csv \
    -o NCAATourneyCompactResults.csv \
    -o NCAATourneyDetailedResults.csv \
    -o NCAATourneySeedRoundSlots.csv \
    -o NCAATourneySeeds.csv \
    -o NCAATourneySlots.csv \
    -o RegularSeasonCompactResults.csv \
    -o RegularSeasonDetailedResults.csv \
    -o Seasons.csv \
    -o SecondaryTourneyCompactResults.csv \
    -o SecondaryTourneyTeams.csv \
    -o TeamCoaches.csv \
    -o TeamConferences.csv \
    -o TeamSpellings.csv \
    -o Teams.csv \
    -f DataFiles.dvc \
    -c data/mens-machine-learning-competition-2018/raw/DataFiles \
    unzip zips/DataFiles.zip


dvc run -d zips/MasseyOrdinals_thruSeason2018_Day128.zip \
    -o MasseyOrdinals_thruSeason2018_Day128.csv \
    -f MasseyOrdinals.dvc \
    -c data/mens-machine-learning-competition-2018/raw/MasseyOrdinals \
    unzip zips/MasseyOrdinals_thruSeason2018_Day128.zip


dvc run -d zips/PrelimData2018.zip \
    -o Cities_Prelim2018.csv \
    -o Events_Prelim2018.csv \
    -o GameCities_Prelim2018.csv \
    -o MasseyOrdinals_Prelim2018.csv \
    -o Players_Prelim2018.csv \
    -o RegularSeasonCompactResults_Prelim2018.csv \
    -o RegularSeasonDetailedResults_Prelim2018.csv \
    -o TeamCoaches_Prelim2018.csv \
    -f PrelimData2018.dvc \
    -c data/mens-machine-learning-competition-2018/raw/PrelimData2018 \
    unzip zips/PrelimData2018.zip


dvc run -d zips/PlayByPlay_2010.zip \
    -o Events_2010.csv \
    -o Players_2010.csv \
    -f PlayByPlay_2010.dvc \
    -c data/mens-machine-learning-competition-2018/raw/PlayByPlay \
    unzip zips/PlayByPlay_2010.zip


for i in `seq 2010 2018`;
do
    dvc run -d zips/PlayByPlay_${i}.zip \
        -o Events_${i}.csv \
        -o Players_${i}.csv \
        -f PlayByPlay_${i}.dvc \
        -c data/mens-machine-learning-competition-2018/raw/PlayByPlay \
        unzip zips/PlayByPlay_${i}.zip
done
