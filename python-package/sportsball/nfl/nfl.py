import gspread
import numpy
import pandas
from oauth2client.service_account import ServiceAccountCredentials


class Team(object):
    def __init__(self, team_id, name, conference, division):
        self.team_id = team_id
        self.name = name
        self.conference = conference
        self.division = division
        self.ratings = {}

    def __repr__(self):
        return '''{}: {}'''.format(self.team_id, self.name)


class Game(object):
    def __init__(self, week, season, neutral, playoff, away_team, home_team,
                 away_score, home_score):
        self.week = week
        self.season = season
        self.neutral = neutral
        self.playoff = playoff
        self.away_team = away_team
        self.home_team = home_team
        self.away_score = away_score
        self.home_score = home_score
        self.prob_win = None
        self.sim_result = None
        self.played = away_score is not None and home_score is not None

    def __repr__(self):
        return '''Week {week} {team1} {neutral} {team2}: {score}'''.format(
            week=self.week,
            team1=self.away_team.team_id,
            team2=self.home_team.team_id,
            neutral='vs' if self.neutral else 'at',
            score='{}-{}'.format(self.away_score, self.home_score)
            if self.played else 'Unplayed',
        )

    def simulate(self):
        if self.played is False and self.prob_win:
            self.sim_result = numpy.random.binomial(
                1, self.prob_win, size=1)[0]
            return self.sim_result


class Season(object):
    def __init__(self, year=None):
        self.year = year
        self.teams = None
        self.games = None
        self.spreadsheet = None
        self.worksheet = None

    def simulate(self):
        for game in self.games:
            game.simulate()
        return self

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

        scheduled_games = pandas.DataFrame(
            self.worksheet.get_worksheet(0).get_all_records())[[
                'week',
                'away_team',
                'home_team',
                'home_adv',
                'score_away',
                'score_home',
                'vegas_spread',
            ]]

        teaminfo = pandas.DataFrame(
            self.worksheet.get_worksheet(6).get_all_records())

        self.teams = {
            x['team_name']: Team(
                team_id=x['team_id'],
                name=x['team_name'],
                conference=x['conference'],
                division=x['division'],
            )
            for x in teaminfo.to_dict('records')
        }

        self.games = [
            Game(
                week=x.get('week'),
                season=2018,
                neutral=x.get('home_adv') == 0,
                playoff=False,
                away_team=self.teams.get(x.get('away_team')),
                home_team=self.teams.get(x.get('home_team')),
                away_score=None if x['score_away'] == '' else x['score_away'],
                home_score=None if x['score_home'] == '' else x['score_home'],
            ) for x in scheduled_games.to_dict('records')
        ]
