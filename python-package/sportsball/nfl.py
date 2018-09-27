#!/usr/bin/env python
# -*- coding: utf-8 -*-

import functools

import numpy
from gspread_pandas import Spread
from scipy.special import expit


def infer_df_types(df):
    for col in df:
        try:
            df[col] = df[col].astype(numpy.int)
        except ValueError:
            try:
                df[col] = df[col].astype(numpy.float)
            except ValueError:
                pass
    return df


@functools.total_ordering
class Team(object):
    def __init__(self, team_id: str, name: str, conference: str,
                 division: str):
        """

        :param team_id: str
        :param name: str
        :param conference: str
        :param division: str
        """
        self.team_id = team_id
        self.name = name
        self.conference = conference
        self.division = division
        self.ratings = dict()
        self.wins = 0
        self.ties = 0
        self.losses = 0

    def __repr__(self):
        return '''{:10} | {:15} | {:2}-{:2}-{:2}'''.format(
            self.division,
            self.name,
            self.wins,
            self.ties,
            self.losses,
        )

    def __lt__(self, other):
        if self.division == other.division:
            if self.wins - self.losses == other.wins - other.losses:
                return self.name < other.name
            else:
                return self.wins - self.losses < other.wins - other.losses
        else:
            return self.division < other.division

    def __eq__(self, other):
        if self.name != other.name:
            return False
        if self.division != other.division:
            return False
        if self.wins - self.losses != other.wins - other.losses:
            return False
        return True

    def winpct(self):
        return float(self.wins / (self.wins + self.losses))

    def attach_rating(self, system: str, week: int, **kwargs) -> None:
        """

        :rtype: None
        :param system: str
        :param week: int
        """
        if system not in self.ratings:
            self.ratings[system] = dict()
        self.ratings[system].update({week: kwargs})

    def get_rating(self, system, week):
        syst = self.ratings.get(system)
        if syst:
            weekmax = max(list(syst.keys()))
            return syst.get(min(weekmax, week)).get('rating')

    def reset_record(self):
        self.wins = 0
        self.ties = 0
        self.losses = 0


class Game(object):
    def __init__(self, week: int, season: int, neutral: bool, playoff: bool,
                 away_team: Team, home_team: Team, away_score: int,
                 home_score: int):
        """

        :param week: int
        :param season: int
        :param neutral: bool
        :param playoff: bool
        :param away_team: Team
        :param home_team: Team
        :param away_score: int
        :param home_score: int
        """
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

    def predict_prob_vegas(self, coef, home_adv):
        away_rating = self.away_team.get_rating('vegas', self.week)
        home_rating = self.home_team.get_rating('vegas', self.week)
        linear_pred = home_rating - away_rating + home_adv if not self.neutral else 0
        return expit(coef * linear_pred)

    def simulate(self):
        if self.played is False:
            if self.prob_win:
                self.sim_result = numpy.random.binomial(
                    1, self.prob_win, size=1)[0]
        else:
            self.sim_result = 1 if self.home_score > self.away_score else 0
        self.away_team.wins += 1 - self.sim_result
        self.away_team.losses += self.sim_result
        self.home_team.wins += self.sim_result
        self.home_team.losses += 1 - self.sim_result


class Season(object):
    def __init__(self, year=None, sheettitle=None):
        self.year = year
        self.teams = None
        self.games = None
        self.spread = None
        self.sheettitle = sheettitle
        self.ratings = dict()

        if self.sheettitle:
            self.attach_worksheet()
            self.load_teams()
            self.load_games()

    def simulate(self):
        self.reset_records()
        for game in self.games:
            game.simulate()
        return self.get_team_records()

    def reset_records(self):
        for team in self.teams.values():
            team.reset_record()

    def get_team_records(self):
        return {
            team.name: {
                'wins': team.wins,
                'losses': team.losses
            }
            for team in self.teams.values()
        }

    def attach_worksheet(self):
        self.spread = Spread('dshurick', self.sheettitle)

    def load_teams(self):
        if not self.spread:
            self.attach_worksheet()

        teaminfo = self.spread.sheet_to_df(sheet='teaminfo', index=None)

        self.teams = {
            x['team_name']: Team(
                team_id=x['team_id'],
                name=x['team_name'],
                conference=x['conference'],
                division=x['division'],
            )
            for x in teaminfo.to_dict('records')
        }

    def load_games(self, season=2018):
        if self.teams is None:
            self.load_teams()

        scheduled_games = self.spread.sheet_to_df(
            sheet='Season2018', index=None).infer_objects()[[
                'week',
                'away_team',
                'home_team',
                'home_adv',
                'score_away',
                'score_home',
                'vegas_spread',
            ]]

        scheduled_games = infer_df_types(scheduled_games)

        self.games = [
            Game(
                week=x.get('week'),
                season=season,
                neutral=x.get('home_adv') == 0,
                playoff=False,
                away_team=self.teams.get(x.get('away_team')),
                home_team=self.teams.get(x.get('home_team')),
                away_score=None if x['score_away'] == '' else x['score_away'],
                home_score=None if x['score_home'] == '' else x['score_home'],
            ) for x in scheduled_games.to_dict('records')
        ]

    def attach_sagarin_ratings(self):
        self.__attach_rating_system('sagarin')

    def attach_massey_ratings(self):
        self.__attach_rating_system('massey')

    def attach_scorex_ratings(self):
        self.__attach_rating_system('scorex')

    def attach_538_ratings(self):
        self.__attach_rating_system('FiveThirtyEight')

    def attach_vegas_ratings(self):
        self.__attach_rating_system('vegas')

    def set_game_probs(self, system: str):
        if system == 'vegas':
            for game in self.games:
                home_adv = self.get_home_adv(system='vegas', week=game.week)
                game.prob_win = game.predict_prob_vegas(
                    0.1354771, home_adv=home_adv)

    def get_home_adv(self, system: str, week: int):
        syst = self.ratings.get(system)
        if syst:
            weekmax = max(list(syst.keys()))
            return syst.get(min(weekmax, week))

    def __attach_rating_system(self, system: str):
        if not self.teams:
            self.load_teams()

        df = self.spread.sheet_to_df(sheet=system, index=None)
        df = infer_df_types(df)

        for x in df.to_dict('records'):
            team_name = x.get('team')
            if team_name in self.teams:
                x['system'] = system
                self.teams[team_name].attach_rating(
                    **{k: v
                       for k, v in x.items() if k != 'team'})
            if team_name == 'HomeAdv':
                self.__attach_home_rating(
                    system=system,
                    week=x.get('week'),
                    rating=x.get('rating'),
                )

    def __attach_home_rating(self, system: str, week: int, rating: float):
        if system not in self.ratings:
            self.ratings[system] = dict()
        self.ratings[system].update({week: rating})
