import json

import gspread
from oauth2client.service_account import ServiceAccountCredentials


class Team(object):
    def __init__(self, team_id, name, conference, division):
        self.team_id = team_id
        self.name = name
        self.conference = conference
        self.division = division
        self.elo_rating = None
        self.sagarin_rating = None
        self.massey_rating = None

    def __repr__(self):
        return '''{}: {}'''.format(self.team_id, self.name)


class Game(object):
    def __init__(self, date, season, neutral, playoff, team1, team2, score1,
                 score2):
        self.date = date
        self.season = season
        self.neutral = neutral
        self.playoff = playoff
        self.team1 = team1
        self.team2 = team2
        self.score1 = score1
        self.score2 = score2
        self.prob_win = None
        self.sim_result = None
        self.played = score1 is not None and score2 is not None

    def __repr__(self):
        return '''{date} {team1} {neutral} {team2}: {score}'''.format(
            date=self.date.date(),
            team1=self.team1.team_id,
            team2=self.team2.team_id,
            neutral='vs' if self.neutral else 'at',
            score='{}-{}'.format(self.score1, self.score2)
            if self.played else 'Unplayed',
        )

    def simulate(self):
        if self.played is False and self.prob_win:
            self.sim_result = numpy.random.binomial(
                1, self.prob_win, size=1)[0]
            return self.sim_result


class Season(object):
    def __init__(self, year=None, teamfile=None):
        self.year = year
        self.teams = self.__load_teams(teamfile)
        self.games = set()

    def simulate(self):
        for game in self.games:
            game.simulate()

    @staticmethod
    def __load_teams(fl):
        with open(fl) as f:
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
        return teams

    def load_gspread(self):
        scope = [
            'https://spreadsheets.google.com/feeds',
            'https://www.googleapis.com/auth/drive'
        ]

        credentials = ServiceAccountCredentials.from_json_keyfile_name(
            '/Users/dshurick/Downloads/dshurick-rdb-test-cb506c785735.json',
            scope)

        self.spreadsheet = gspread.authorize(credentials)

        self.worksheet = self.spreadsheet.open("NFL 2018 Expected Wins")
