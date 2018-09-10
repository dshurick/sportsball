import csv
import json
from urllib.request import urlretrieve

import dateparser as dateparser
import gspread
import pandas
from oauth2client.service_account import ServiceAccountCredentials

from src.nfl.nfl import Team, Season, Game


def main2():
    scope = [
        'https://spreadsheets.google.com/feeds',
        'https://www.googleapis.com/auth/drive'
    ]

    credentials = ServiceAccountCredentials.from_json_keyfile_name(
        '/Users/dshurick/Downloads/dshurick-rdb-test-cb506c785735.json',
        scope)

    spreadsheet = gspread.authorize(credentials)

    worksheet = spreadsheet.open("NFL 2018 Expected Wins")

    # Season2018 = pandas.DataFrame(wks.get_worksheet(0).get_all_records())
    # sagarin_ratings = pandas.DataFrame(worksheet.get_worksheet(1).get_all_records())
    # scorex = pandas.DataFrame(worksheet.get_worksheet(2).get_all_records())
    # FiveThirtyEight = pandas.DataFrame(worksheet.get_worksheet(3).get_all_records())
    # massey = pandas.DataFrame(worksheet.get_worksheet(4).get_all_records())

    teaminfo = pandas.DataFrame(worksheet.get_worksheet(6).get_all_records())

    print(teaminfo)


def main():
    with open('../../data/raw/nfl_teams.json') as f:
        teams_json = json.load(f)
    teams = {
        team.get('team_id'): Team(
            team_id=team.get('team_id'),
            name=team.get('name'),
            conference=team.get('conference'),
            division=team.get('division'),
        )
        for team in teams_json
    }
    print(teams)

    season = Season(year=2018, teamfile='../../data/raw/nfl_teams.json')

    season.teams = teams

    file = "../../data/raw/nfl_games.csv"
    file_2018 = "../../data/raw/nfl_games_2018.csv"

    games = [item for item in csv.DictReader(open(file))]

    urlretrieve(
        "https://projects.fivethirtyeight.com/nfl-api/2018/nfl_games_2018.csv",
        file_2018)

    games += [item for item in csv.DictReader(open(file_2018))]

    for game in games:
        game['season'], game['neutral'], game['playoff'] = int(
            game['season']), int(game['neutral']), int(game['playoff'])
        game['score1'], game['score2'] = int(
            game['score1']) if game['score1'] != '' else None, int(
                game['score2']) if game['score2'] != '' else None
        game['elo_prob1'], game['result1'] = float(
            game['elo_prob1']) if game['elo_prob1'] != '' else None, float(
                game['result1']) if game['result1'] != '' else None

    for game in filter(lambda x: x['season'] == 2018, games):
        season.games.add(
            Game(
                date=dateparser.parse(game['date']),
                season=game['season'],
                neutral=game['neutral'] == 1,
                playoff=game['playoff'] == 1,
                team1=season.teams.get(game.get('team1')),
                team2=season.teams.get(game.get('team2')),
                score1=game['score1'],
                score2=game['score2'],
            ))
    print(season.games)


if __name__ == '__main__':
    main2()
