#!/bin/bash

# Set up empty directory structure
BASERAWDATA=$(dvc root)/data/raw/mens-machine-learning-competition-2019

mkdir ${BASERAWDATA}/zips
mkdir ${BASERAWDATA}/Stage2DataFiles
mkdir ${BASERAWDATA}/DataFiles
mkdir ${BASERAWDATA}/MasseyOrdinals
mkdir ${BASERAWDATA}/PlayByPlay

for i in $(seq 2015 2019); do
        dvc run -o PlayByPlay_${i}.zip \
                -w ${BASERAWDATA}/zips \
                -f ${BASERAWDATA}/zips/PlayByPlay_${i}.zip.dvc \
                kaggle competitions download --file PlayByPlay_${i}.zip mens-machine-learning-competition-2019
done

dvc run -o Stage2DataFiles.zip \
        --wdir ${BASERAWDATA}/zips \
        --file ${BASERAWDATA}/zips/Stage2DataFiles.zip.dvc \
        "kaggle competitions download --file Stage2DataFiles.zip mens-machine-learning-competition-2019"

dvc run -o DataFiles.zip \
        --wdir ${BASERAWDATA}/zips \
        --file ${BASERAWDATA}/zips/DataFiles.zip.dvc \
        "kaggle competitions download --file DataFiles.zip mens-machine-learning-competition-2019"

dvc run -o MasseyOrdinals.zip \
        -w ${BASERAWDATA}/zips \
        --file ${BASERAWDATA}/zips/MasseyOrdinals.zip.dvc \
        "kaggle competitions download --file MasseyOrdinals.zip mens-machine-learning-competition-2019"

dvc run -o SampleSubmissionStage2.csv \
        -w ${BASERAWDATA}/Stage2DataFiles \
        -f ${BASERAWDATA}/Stage2DataFiles/SampleSubmissionStage2.csv.dvc \
        "kaggle competitions download --file SampleSubmissionStage2.csv mens-machine-learning-competition-2019"

dvc run -d ${BASERAWDATA}/zips/DataFiles.zip \
        -o ${BASERAWDATA}/DataFiles/Cities.csv \
        -o ${BASERAWDATA}/DataFiles/Conferences.csv \
        -o ${BASERAWDATA}/DataFiles/ConferenceTourneyGames.csv \
        -o ${BASERAWDATA}/DataFiles/GameCities.csv \
        -o ${BASERAWDATA}/DataFiles/NCAATourneyCompactResults.csv \
        -o ${BASERAWDATA}/DataFiles/NCAATourneyDetailedResults.csv \
        -o ${BASERAWDATA}/DataFiles/NCAATourneySeedRoundSlots.csv \
        -o ${BASERAWDATA}/DataFiles/NCAATourneySeeds.csv \
        -o ${BASERAWDATA}/DataFiles/NCAATourneySlots.csv \
        -o ${BASERAWDATA}/DataFiles/RegularSeasonCompactResults.csv \
        -o ${BASERAWDATA}/DataFiles/RegularSeasonDetailedResults.csv \
        -o ${BASERAWDATA}/DataFiles/Seasons.csv \
        -o ${BASERAWDATA}/DataFiles/SecondaryTourneyCompactResults.csv \
        -o ${BASERAWDATA}/DataFiles/SecondaryTourneyTeams.csv \
        -o ${BASERAWDATA}/DataFiles/TeamCoaches.csv \
        -o ${BASERAWDATA}/DataFiles/TeamConferences.csv \
        -o ${BASERAWDATA}/DataFiles/TeamSpellings.csv \
        -o ${BASERAWDATA}/DataFiles/Teams.csv \
        -f ${BASERAWDATA}/DataFiles/DataFiles.dvc \
        unzip ${BASERAWDATA}/zips/DataFiles.zip -d ${BASERAWDATA}/DataFiles

dvc run -d ${BASERAWDATA}/zips/Stage2DataFiles.zip \
        -o ${BASERAWDATA}/Stage2DataFiles/Cities.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/Conferences.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/ConferenceTourneyGames.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/GameCities.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/NCAATourneyCompactResults.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/NCAATourneyDetailedResults.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/NCAATourneySeedRoundSlots.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/NCAATourneySeeds.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/NCAATourneySlots.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/RegularSeasonCompactResults.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/RegularSeasonDetailedResults.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/Seasons.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/SecondaryTourneyCompactResults.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/SecondaryTourneyTeams.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/TeamCoaches.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/TeamConferences.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/TeamSpellings.csv \
        -o ${BASERAWDATA}/Stage2DataFiles/Teams.csv \
        -f ${BASERAWDATA}/Stage2DataFiles/Stage2DataFiles.dvc \
        unzip ${BASERAWDATA}/zips/Stage2DataFiles.zip -d ${BASERAWDATA}/Stage2DataFiles

dvc run -d ${BASERAWDATA}/zips/MasseyOrdinals.zip \
        -o ${BASERAWDATA}/MasseyOrdinals/MasseyOrdinals.csv \
        -f ${BASERAWDATA}/MasseyOrdinals/MasseyOrdinals.dvc \
        "unzip ${BASERAWDATA}/zips/MasseyOrdinals.zip -d ${BASERAWDATA}/MasseyOrdinals"

for i in $(seq 2010 2019); do
        dvc run -d ${BASERAWDATA}/zips/PlayByPlay_${i}.zip \
                -o ${BASERAWDATA}/PlayByPlay/Events_${i}.csv \
                -o ${BASERAWDATA}/PlayByPlay/Players_${i}.csv \
                -f ${BASERAWDATA}/PlayByPlay/PlayByPlay_${i}.dvc \
                unzip ${BASERAWDATA}/zips/PlayByPlay_${i}.zip -d ${BASERAWDATA}/PlayByPlay
done

dvc add -f ${BASERAWDATA}/team_spellings.csv.dvc ${BASERAWDATA}/team_spellings.csv
