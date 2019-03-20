#!/bin/bash

# Set up empty directory structure
BASERAWDATA=$(dvc root)/data/raw/womens-machine-learning-competition-2019

mkdir ${BASERAWDATA}/zips
mkdir ${BASERAWDATA}/Stage2WDataFiles
mkdir ${BASERAWDATA}/WDataFiles

dvc run -o WDataFiles.zip \
    -w ${BASERAWDATA}/zips \
    -f ${BASERAWDATA}/zips/WDataFiles.zip.dvc \
    kaggle competitions download --file WDataFiles.zip womens-machine-learning-competition-2019

dvc run -o Stage2WDataFiles.zip \
    -w ${BASERAWDATA}/zips \
    -f ${BASERAWDATA}/zips/Stage2WDataFiles.zip.dvc \
    kaggle competitions download --file Stage2WDataFiles.zip womens-machine-learning-competition-2019

dvc run -o WSampleSubmissionStage1.csv \
    -w ${BASERAWDATA}/WDataFiles \
    -f ${BASERAWDATA}/WDataFiles/WSampleSubmissionStage1.csv.dvc \
    kaggle competitions download --file WSampleSubmissionStage1.csv womens-machine-learning-competition-2019

dvc run -o WSampleSubmissionStage2.csv \
    -w ${BASERAWDATA}/Stage2WDataFiles \
    -f ${BASERAWDATA}/Stage2WDataFiles/WSampleSubmissionStage2.csv.dvc \
    kaggle competitions download --file WSampleSubmissionStage2.csv womens-machine-learning-competition-2019

dvc run -o ${BASERAWDATA}/WDataFiles/WCities.csv \
    -o ${BASERAWDATA}/WDataFiles/WGameCities.csv \
    -o ${BASERAWDATA}/WDataFiles/WNCAATourneyCompactResults.csv \
    -o ${BASERAWDATA}/WDataFiles/WNCAATourneyDetailedResults.csv \
    -o ${BASERAWDATA}/WDataFiles/WNCAATourneySeeds.csv \
    -o ${BASERAWDATA}/WDataFiles/WNCAATourneySlots.csv \
    -o ${BASERAWDATA}/WDataFiles/WRegularSeasonCompactResults.csv \
    -o ${BASERAWDATA}/WDataFiles/WRegularSeasonDetailedResults.csv \
    -o ${BASERAWDATA}/WDataFiles/WSeasons.csv \
    -o ${BASERAWDATA}/WDataFiles/WTeams.csv \
    -o ${BASERAWDATA}/WDataFiles/WTeamSpellings.csv \
    -f ${BASERAWDATA}/WDataFiles/WDataFiles.dvc \
    unzip -o ${BASERAWDATA}/zips/WDataFiles.zip -d ${BASERAWDATA}/WDataFiles

dvc run -o ${BASERAWDATA}/Stage2WDataFiles/WCities.csv \
    -o ${BASERAWDATA}/Stage2WDataFiles/WGameCities.csv \
    -o ${BASERAWDATA}/Stage2WDataFiles/WNCAATourneyCompactResults.csv \
    -o ${BASERAWDATA}/Stage2WDataFiles/WNCAATourneyDetailedResults.csv \
    -o ${BASERAWDATA}/Stage2WDataFiles/WNCAATourneySeeds.csv \
    -o ${BASERAWDATA}/Stage2WDataFiles/WNCAATourneySlots.csv \
    -o ${BASERAWDATA}/Stage2WDataFiles/WRegularSeasonCompactResults.csv \
    -o ${BASERAWDATA}/Stage2WDataFiles/WRegularSeasonDetailedResults.csv \
    -o ${BASERAWDATA}/Stage2WDataFiles/WSeasons.csv \
    -o ${BASERAWDATA}/Stage2WDataFiles/WTeams.csv \
    -o ${BASERAWDATA}/Stage2WDataFiles/WTeamSpellings.csv \
    -f ${BASERAWDATA}/Stage2WDataFiles/WDataFiles.dvc \
    unzip -o ${BASERAWDATA}/zips/Stage2WDataFiles.zip -d ${BASERAWDATA}/Stage2WDataFiles

