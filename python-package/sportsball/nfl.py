#!/usr/bin/env python
# -*- coding: utf-8 -*-

import functools
import math
from collections import Counter
from enum import Enum, auto

import numpy
import pandas
from gspread_pandas import Spread
from scipy.special import expit


def infer_df_types(df):
    for col in df:
        try:
            df[col] = pandas.to_numeric(df[col])
        except ValueError:
            pass
    return df


class TeamSet(set):
    def popmax(self):
        x = sorted(self)[-1]
        self.remove(x)
        return x


class GameRound(Enum):
    REG = auto()
    WILDCARD = auto()
    DIVISIONAL = auto()
    CONFERENCE = auto()
    SUPERBOWL = auto()


VALUE_MAP = {
    GameRound.REG.value: {
        1: 1.4,
        0: 0.9,
        -1: 0.4,
    },
    GameRound.WILDCARD.value: {
        1: 0,
        -1: 2.5,
    },
    GameRound.DIVISIONAL.value: {
        1: 5.5,
        -1: 5.5,
    },
    GameRound.CONFERENCE.value: {
        1: 4.0,
        -1: 4.0,
    },
    GameRound.SUPERBOWL.value: {
        1: 13.0,
        -1: 5.0,
    }
}


class Season(object):
    def __init__(self, year=None, sheettitle=None):
        self.year = year
        self.teams = None
        self.games = None
        self.spread = None
        self.sheettitle = sheettitle
        self.ratings = dict()
        self.league = None
        self.playoff_games = None
        self.coefs = {'vegas': 0.1057763}

        if self.sheettitle:
            self.attach_worksheet()
            self.load_teams()
            self.load_games()

    def simulate(self):
        for game in self.games:
            game.simulate()
        for team in self.teams.values():
            team.setrecord()
        self.sim_playoffs()

    def team_values(self):
        team_values = {team: 0.0 for team in self.teams.values()}
        for ii in range(4000):
            self.simulate()
            for game in self.games:
                away_team = game.away_team
                home_team = game.home_team
                team_values[away_team] += VALUE_MAP[game.game_round.value][
                    away_team.mapgmresult(game)]
                team_values[home_team] += VALUE_MAP[game.game_round.value][
                    home_team.mapgmresult(game)]
            for game in self.playoff_games:
                away_team = game.away_team
                home_team = game.home_team
                team_values[away_team] += VALUE_MAP[game.game_round.value][
                    away_team.mapgmresult(game)]
                team_values[home_team] += VALUE_MAP[game.game_round.value][
                    home_team.mapgmresult(game)]
        return team_values

    def attach_worksheet(self):
        self.spread = Spread('dshurick', self.sheettitle)

    def load_teams(self):
        if not self.spread:
            self.attach_worksheet()

        teaminfo = self.spread.sheet_to_df(sheet='teaminfo', index=None)

        self.teams = {
            x['team_name']: Team(
                season=self,
                team_id=x['team_id'],
                name=x['team_name'],
                conference=x['conference'],
                division=x['division'],
            )
            for x in teaminfo.to_dict('records')
        }

        teammap = {}
        for team in self.teams.values():
            if team.conference not in teammap:
                teammap[team.conference] = dict()
            if team.division not in teammap[team.conference]:
                teammap[team.conference][team.division] = []
            teammap[team.conference][team.division].append(team)

        self.league = teammap

    def load_games(self):
        if self.teams is None:
            self.load_teams()

        scheduled_games = self.spread.sheet_to_df(
            sheet='Season2018', index=None)[[
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
                season=self,
                week=x.get('week'),
                neutral=x.get('home_adv') == 0,
                playoff=False,
                away_team=self.teams.get(x.get('away_team')),
                home_team=self.teams.get(x.get('home_team')),
                away_score=x['score_away'],
                home_score=x['score_home'],
            ) for x in scheduled_games.to_dict('records')
        ]

        for game in self.games:
            away_team = game.away_team
            home_team = game.home_team
            away_team.games.append(game)
            home_team.games.append(game)

        for team in self.teams.values():
            team.setrecord()

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

    def print_divisions(self):
        for cnfrnce_str, cnfrnce in self.league.items():
            print(cnfrnce_str)
            for dvsn_str, dvsn in cnfrnce.items():
                print('  {}'.format(dvsn_str))

                for team in sorted(dvsn, reverse=True):
                    print(
                        '    {:15}| {:2} - {:2} - {:2} | {:>4} | {:2} | {:2}'.
                        format(
                            team.name,
                            team.wins,
                            team.ties,
                            team.losses,
                            '{:.0%}'.format(team.winpct),
                            team.divisional_record(),
                            team.conference_record(),
                        ))

    def sim_playoffs(self):
        teammap = {}
        for team in self.teams.values():
            if team.conference not in teammap:
                teammap[team.conference] = dict()
            if team.division not in teammap[team.conference]:
                teammap[team.conference][team.division] = TeamSet()
            teammap[team.conference][team.division].add(team)

        playoff_seeds = {}
        for cnfrnce_str, cnfrnce in teammap.items():
            division_leaders = TeamSet()
            wildcard_race = TeamSet()
            for dvsn_str, dvsn in cnfrnce.items():
                division_leaders.add(dvsn.popmax())
                wildcard_race.update(dvsn)
            cnfrnce_seeds = {}
            cnfrnce_seeds[1] = division_leaders.popmax()
            cnfrnce_seeds[2] = division_leaders.popmax()
            cnfrnce_seeds[3] = division_leaders.popmax()
            cnfrnce_seeds[4] = division_leaders.popmax()
            cnfrnce_seeds[5] = wildcard_race.popmax()
            cnfrnce_seeds[6] = wildcard_race.popmax()
            playoff_seeds[cnfrnce_str] = cnfrnce_seeds

        afc_seeds, nfc_seeds = playoff_seeds.values()

        round1 = set()
        for cnfrnce_str in playoff_seeds.keys():
            for awayseed, homeseed in ((5, 4), (6, 3)):
                game = Game(
                    week=18,
                    season=self,
                    neutral=False,
                    playoff=True,
                    away_team=playoff_seeds[cnfrnce_str][awayseed],
                    home_team=playoff_seeds[cnfrnce_str][homeseed],
                    away_score=None,
                    home_score=None,
                    game_round=GameRound.WILDCARD)
                game.simulate()
                round1.add(game)

        round1winners = {game.winner() for game in round1}

        afc_hiseed, afc_lowseed = sorted(
            {k
             for k, v in afc_seeds.items() if v in round1winners})
        nfc_hiseed, nfc_lowseed = sorted(
            {k
             for k, v in nfc_seeds.items() if v in round1winners})

        game5 = Game(
            week=19,
            season=self,
            neutral=False,
            playoff=True,
            away_team=afc_seeds[afc_lowseed],
            home_team=afc_seeds[1],
            away_score=None,
            home_score=None,
            game_round=GameRound.DIVISIONAL)

        # 6 vs. 3 (home)
        game6 = Game(
            week=19,
            season=self,
            neutral=False,
            playoff=True,
            away_team=afc_seeds[afc_hiseed],
            home_team=afc_seeds[2],
            away_score=None,
            home_score=None,
            game_round=GameRound.DIVISIONAL)

        # nfc
        # 5 vs. 4 (home)
        game7 = Game(
            week=19,
            season=self,
            neutral=False,
            playoff=True,
            away_team=nfc_seeds[nfc_lowseed],
            home_team=nfc_seeds[1],
            away_score=None,
            home_score=None,
            game_round=GameRound.DIVISIONAL)

        # 6 vs. 3 (home)
        game8 = Game(
            week=19,
            season=self,
            neutral=False,
            playoff=True,
            away_team=nfc_seeds[nfc_hiseed],
            home_team=nfc_seeds[2],
            away_score=None,
            home_score=None,
            game_round=GameRound.DIVISIONAL)

        round2 = {
            game5,
            game6,
            game7,
            game8,
        }
        for game in round2:
            game.simulate()

        game9 = Game(
            week=20,
            season=self,
            neutral=True,
            playoff=True,
            away_team=game5.winner(),
            home_team=game6.winner(),
            away_score=None,
            home_score=None,
            game_round=GameRound.CONFERENCE)

        game10 = Game(
            week=20,
            season=self,
            neutral=True,
            playoff=True,
            away_team=game7.winner(),
            home_team=game8.winner(),
            away_score=None,
            home_score=None,
            game_round=GameRound.CONFERENCE)

        round3 = {
            game9,
            game10,
        }
        for game in round3:
            game.simulate()

        # Superbowl
        game11 = Game(
            week=22,
            season=self,
            neutral=True,
            playoff=True,
            away_team=game9.winner(),
            home_team=game10.winner(),
            away_score=None,
            home_score=None,
            game_round=GameRound.SUPERBOWL)

        game11.simulate()

        self.playoff_games = round1 | round2 | round3 | {game11}

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


@functools.total_ordering
class Team(object):
    def __init__(self, season: Season, team_id: str, name: str,
                 conference: str, division: str):
        """

        :param team_id: str
        :param name: str
        :param conference: str
        :param division: str
        """
        self.season = season
        self.team_id = team_id
        self.name = name
        self.conference = conference
        self.division = division
        self.ratings = dict()
        self.games = []
        self._record = None

    def __repr__(self):
        return '''{}: {}'''.format(self.team_id, self.name)

    def __str__(self):
        return '''{:4}| {:10}| {:15}| {:2} - {:2} - {:2} | {:>4}'''.format(
            self.conference,
            self.division,
            self.name,
            self.wins,
            self.ties,
            self.losses,
            '{:.0%}'.format(self.winpct),
        )

    def __lt__(self, other, switch=None):
        if not switch:
            if self == other:
                return self.__lt__(other, switch='head_to_head')
            return self.wins + 0.5 * self.ties < other.wins + 0.5 * other.ties
        elif switch == 'head_to_head':
            if self.__eq__(other, switch='head_to_head'):
                return self.__lt__(other, switch='divisional')
            return self._head_to_head_record(other) < 0.0
        elif switch == 'divisional':
            if self.__eq__(other, switch='divisional'):
                return self.__lt__(other, switch='conference')
            return self.divisional_record() < other.divisional_record()
        elif switch == 'conference':
            if self.__eq__(other, switch='conference'):
                return numpy.random.binomial(n=1, p=0.5) == 1
            return self.conference_record() < other.conference_record()
        else:
            raise ValueError("invalid value for 'switch' argument")

    def __eq__(self, other, switch=None):
        if not switch:
            return math.isclose(self.wins + 0.5 * self.ties,
                                other.wins + 0.5 * other.ties)
        elif switch == 'head_to_head':
            return math.isclose(self._head_to_head_record(other), 0.0)
        elif switch == 'divisional':
            # tied overall record
            return math.isclose(self.divisional_record(),
                                other.divisional_record())
        elif switch == 'conference':
            # tied overall record
            return math.isclose(self.conference_record(),
                                other.conference_record())
        else:
            raise ValueError("invalid value for 'switch' argument")

    def __hash__(self):
        return hash((self.name, self.team_id, self.conference, self.division))

    def _head_to_head_games(self, other):
        for game in self.games:
            if game.result is not None:
                if game.away_team.team_id in (self.team_id, other.team_id
                                              ) and game.home_team.team_id in (
                                                  self.team_id, other.team_id):
                    yield game

    def _head_to_head_record(self, other):
        return sum(
            map(lambda x: self.mapgmresult(x),
                self._head_to_head_games(other)))

    def _division_games(self):
        for game in self.games:
            if game.result is not None:
                if game.away_team is self and game.home_team.division == self.division:
                    yield game
                if game.home_team is self and game.away_team.division == self.division:
                    yield game

    def divisional_record(self):
        return sum(map(lambda x: self.mapgmresult(x), self._division_games()))

    def _conference_games(self):
        for game in self.games:
            if game.result is not None:
                if game.away_team is self and game.home_team.conference == self.conference:
                    yield game
                if game.home_team is self and game.away_team.conference == self.conference:
                    yield game

    def conference_record(self):
        return sum(
            map(lambda x: self.mapgmresult(x), self._conference_games()))

    def mapgmresult(self, game):
        if game.result is not None:
            if game.away_team is self:
                return -game.result
            else:
                return game.result

    def _calcrecord(self, games=None):
        if games:
            return Counter([self.mapgmresult(game) for game in games])
        return Counter([self.mapgmresult(game) for game in self.games])

    def setrecord(self):
        self._record = self._calcrecord()

    @property
    def wins(self):
        return self._record[1]

    @property
    def losses(self):
        return self._record[-1]

    @property
    def ties(self):
        return self._record[0]

    def print_record(self):
        print("{:02} - {:02} - {:02}".format(self.wins, self.ties,
                                             self.losses))

    @property
    def winpct(self):
        return float(self.wins + 0.5 * self.ties) / float(
            self.wins + self.ties + self.losses)

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
            if system == 'massey':
                return syst.get(min(weekmax, week))
            return syst.get(min(weekmax, week)).get('rating')


class League(object):
    def __init__(self, afc, nfc):
        self.nfc = nfc
        self.afc = afc

    @property
    def teams(self):
        return list(self.__teams())

    def __teams(self):
        for conference in (
                self.nfc,
                self.afc,
        ):
            for team in conference.teams:
                yield team

    def __hash__(self):
        return hash((self.afc, self.nfc))


class Conference(object):
    def __init__(self, name, *divisions):
        self.name = name
        self.divisions = set(divisions)
        for division in self.divisions:
            setattr(self, division.name, division)

    @property
    def teams(self):
        return list(self.__teams())

    def __teams(self):
        for division in self.divisions:
            for team in division.teams:
                yield team

    def add_team(self, team):
        if team.division not in {x.name for x in self.divisions}:
            self.divisions.add(Conference(team.division))
        for division in self.divisions:
            if division.name == team.division:
                division.add_team(team)

    def __hash__(self):
        return hash((self.name, (division for division in self.divisions)))


class Division(object):
    def __init__(self, name, *teams):
        self.name = name
        self.teams = set(teams)
        for team in self.teams:
            setattr(self, team.team_id, team)

    def add_team(self, team):
        self.teams.add(team)

    def __hash__(self):
        return hash((self.name, (team for team in self.teams)))


class Game(object):
    def __init__(self,
                 season: Season,
                 week: int,
                 neutral: bool,
                 playoff: bool,
                 away_team: Team,
                 home_team: Team,
                 away_score: int = None,
                 home_score: int = None,
                 game_round: GameRound = GameRound.REG):
        """

        :param week: int
        :param neutral: bool
        :param playoff: bool
        :param away_team: Team
        :param home_team: Team
        :param away_score: int
        :param home_score: int
        """
        self.season = season
        self.week = week
        self.neutral = neutral
        self.playoff = playoff
        self.away_team = away_team
        self.home_team = home_team
        self.prob_win = None
        try:
            self.away_score = int(away_score)
            self.home_score = int(home_score)
        except (ValueError, TypeError):
            self.away_score = None
            self.home_score = None
        self.game_round = game_round

        self.played = self.away_score is not None and self.home_score is not None
        self.result = None
        if self.played:
            self.result = 0 if math.isclose(
                self.home_score, self.away_score) else int(
                    math.copysign(1, self.home_score - self.away_score))

    def __repr__(self):
        score = '{:2} - {:2}'.format(self.away_score,
                                     self.home_score) if self.played else ''
        winner = self.winner()
        return '''{:7} | Week {week:2} | {team1:10} {neutral} {team2:10} | {winner}'''.format(
            score,
            week=self.week,
            team1=self.away_team.team_id,
            team2=self.home_team.team_id
            if self.neutral else self.home_team.team_id.upper(),
            neutral='vs' if self.neutral else 'at',
            winner='TIE' if not winner else winner.team_id,
        )

    def winner(self):
        if self.result == 1:
            return self.home_team
        elif self.result == -1:
            return self.away_team

    def predict_prob_vegas(self):
        home_adv = self.season.get_home_adv(system='vegas', week=self.week)
        coef = self.season.coefs['vegas']
        away_rating = self.away_team.get_rating('vegas', self.week)
        home_rating = self.home_team.get_rating('vegas', self.week)
        if away_rating is not None and home_rating is not None:
            linear_pred = home_rating - away_rating + home_adv if not self.neutral else 0
            return expit(coef * linear_pred)

    def predict_prob_custom(self):
        # home_adv = 0.130935044
        # coef = {'massey': 0.15912980, '538': -0.00479260}

        massey_away = self.away_team.get_rating('massey', self.week)
        massey_home = self.home_team.get_rating('massey', self.week)

        # FiveThirtyEight_away = self.away_team.get_rating(
        #     'FiveThirtyEight', self.week)
        # FiveThirtyEight_home = self.home_team.get_rating(
        #     'FiveThirtyEight', self.week)

        away_rating = 0.07103225 * massey_away['off'] + 0.04929093 * massey_home['def']
        home_rating = 0.07103225 * massey_home['off'] + 0.04929093 * massey_away['def']

        if not self.neutral:
            home_rating += 0.3552361  # massey_home['hfa']

        massey_linear = home_rating - away_rating
        # FiveThirtyEight_linear = FiveThirtyEight_home - FiveThirtyEight_away

        return expit(massey_linear)

    def simulate(self):
        if self.played is False:
            if self.prob_win:
                self.result = 2 * numpy.random.binomial(
                    1, self.prob_win, size=1)[0] - 1
            else:
                self.prob_win = self.predict_prob_custom()
                self.result = 2 * numpy.random.binomial(
                    1, self.prob_win, size=1)[0] - 1
