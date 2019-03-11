#!/bin/bash

mkdir data/mens-machine-learning-competition-2019/raw/PlayByPlay/zips

for i in `seq 2010 2018`;
do
    dvc run -o PlayByPlay_${i}.zip \
        -w data/mens-machine-learning-competition-2019/raw/PlayByPlay/zips \
        kaggle competitions download \
        --file PlayByPlay_${i}.zip mens-machine-learning-competition-2019
done

mkdir data/mens-machine-learning-competition-2019/raw/DataFiles/zips

dvc run -o DataFiles.zip \
    -w data/mens-machine-learning-competition-2019/raw/DataFiles/zips \
    kaggle competitions download \
    --file DataFiles.zip mens-machine-learning-competition-2019

mkdir data/mens-machine-learning-competition-2019/raw/PrelimData2019/zips

dvc run -o PrelimData2019.zip \
    -w data/mens-machine-learning-competition-2019/raw/PrelimData2019/zips \
    kaggle competitions download \
    --file PrelimData2019.zip mens-machine-learning-competition-2019

mkdir data/mens-machine-learning-competition-2019/raw/MasseyOrdinals/zips

dvc run -o MasseyOrdinals.zip \
    -w data/mens-machine-learning-competition-2019/raw/MasseyOrdinals/zips \
    kaggle competitions download \
    --file MasseyOrdinals.zip mens-machine-learning-competition-2019

#dvc run -o Stage2UpdatedDataFiles.zip \
#    -w data/mens-machine-learning-competition-2019/raw/DataFiles/zips \
#    kaggle competitions download \
#    --file Stage2UpdatedDataFiles.zip mens-machine-learning-competition-2019

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
    -w data/mens-machine-learning-competition-2019/raw/DataFiles \
    unzip zips/DataFiles.zip


dvc run -d zips/MasseyOrdinals.zip \
    -o MasseyOrdinals.csv \
    -f MasseyOrdinals.dvc \
    -w data/mens-machine-learning-competition-2019/raw/MasseyOrdinals \
    unzip zips/MasseyOrdinals.zip


#dvc run -d zips/PrelimData2019.zip \
#    -o Cities_Prelim2019.csv \
#    -o Events_Prelim2019.csv \
#    -o GameCities_Prelim2019.csv \
#    -o MasseyOrdinals_Prelim2019.csv \
#    -o Players_Prelim2019.csv \
#    -o RegularSeasonCompactResults_Prelim2019.csv \
#    -o RegularSeasonDetailedResults_Prelim2019.csv \
#    -o TeamCoaches_Prelim2019.csv \
#    -f PrelimData2019.dvc \
#    -w data/mens-machine-learning-competition-2019/raw/PrelimData2019 \
#    unzip zips/PrelimData2019.zip


dvc run -d zips/PlayByPlay_2010.zip \
    -o Events_2010.csv \
    -o Players_2010.csv \
    -f PlayByPlay_2010.dvc \
    -w data/mens-machine-learning-competition-2019/raw/PlayByPlay \
    unzip zips/PlayByPlay_2010.zip


for i in `seq 2010 2018`;
do
    dvc run -d zips/PlayByPlay_${i}.zip \
        -o Events_${i}.csv \
        -o Players_${i}.csv \
        -f PlayByPlay_${i}.dvc \
        -w data/mens-machine-learning-competition-2019/raw/PlayByPlay \
        unzip zips/PlayByPlay_${i}.zip
done
