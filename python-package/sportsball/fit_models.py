import numpy
import pandas
import sklearn
import sklearn.linear_model
from hyperopt import fmin, tpe, hp, STATUS_OK, Trials
from sklearn.model_selection import LeavePGroupsOut
from sklearn.model_selection import cross_validate
from sklearn_pandas import DataFrameMapper

from sportsball.nfl.nfl import Season

season_2018 = Season(
    year=2018,
    sheettitle='NFL 2018 Expected Wins',
)

season_2018.attach_sagarin_ratings()
season_2018.attach_538_ratings()
season_2018.attach_massey_ratings()
season_2018.attach_scorex_ratings()
season_2018.attach_vegas_ratings()


def genratings(season, system):
    for teamname, team in season.teams.items():
        for week, rating in team.ratings[system].items():
            ret = {
                'team_id': team.team_id,
                'system_name': system,
                'week': week,
            }
            ret.update(rating)
            yield ret


ratings_gnrtr = genratings(season_2018, 'massey')
ratings_pdf = pandas.DataFrame(list(ratings_gnrtr))
del ratings_pdf['system_name']
dtf = pandas.DataFrame([{
    'away_team': game.away_team.team_id,
    'home_team': game.home_team.team_id,
    'away_score': game.away_score,
    'home_score': game.home_score,
    'home_adv': 0 if game.neutral else 1,
    'week': game.week,
    'game_id': ii,
} for ii, game in enumerate(season_2018.games) if game.played] +
                       [{
                           'away_team': game.home_team.team_id,
                           'home_team': game.away_team.team_id,
                           'away_score': game.home_score,
                           'home_score': game.away_score,
                           'home_adv': 0 if game.neutral else -1,
                           'week': game.week,
                           'game_id': ii,
                       } for ii, game in enumerate(season_2018.games)
                        if game.played])[[
                            'game_id',
                            'week',
                            'away_team',
                            'home_team',
                            'home_adv',
                            'away_score',
                            'home_score',
                        ]]
dtf = dtf.merge(
    ratings_pdf.rename(
        index=str,
        columns={
            'team_id': 'away_team',
            'def': 'def_away',
            'hfa': 'hfa_away',
            'off': 'off_away',
            'rating': 'rating_away',
        }),
    how='left',
    on=[
        'week',
        'away_team',
    ]).merge(
        ratings_pdf.rename(
            index=str,
            columns={
                'team_id': 'home_team',
                'def': 'def_home',
                'hfa': 'hfa_home',
                'off': 'off_home',
                'rating': 'rating_home',
            }),
        how='left',
        on=[
            'week',
            'home_team',
        ])
dtf['away_team'] = pandas.Categorical(dtf['away_team'])
dtf['home_team'] = pandas.Categorical(dtf['home_team'])
dtf = dtf.loc[dtf.home_score != dtf.away_score]
dtf.loc[dtf.home_adv == 1, 'hfa_away'] = 0
dtf.loc[dtf.home_adv == -1, 'hfa_home'] = 0

y = numpy.array((dtf.home_score > dtf.away_score) * 1)

featurizer = DataFrameMapper(
    [(
        [col],
        None,
    ) for col in (
        'def_away',
        'hfa_away',
        'off_away',
        'def_home',
        'hfa_home',
        'off_home',
    )],
    sparse=True,
)

X = featurizer.fit_transform(dtf)

name = 'hypers'

space = {
    'penalty': hp.choice(name + '_penalty', ['l2', 'l1', 'elasticnet']),
    'alpha': hp.loguniform(name + '_alpha', numpy.log(1e-6), numpy.log(1e-1)),
    'l1_ratio': hp.uniform(name + '_l1_ratio', 0, 1),
    'fit_intercept': hp.choice(name + '_fit_intercept', [False, True]),
    'shuffle': hp.choice(name + '_shuffle', [False, True]),
}

logo = list(LeavePGroupsOut(3).split(dtf, y, numpy.array(dtf.game_id)))


def objective(params):
    pipe = sklearn.pipeline.Pipeline([
        (
            'featurizer',
            featurizer,
        ),
        (
            'lm',
            sklearn.linear_model.SGDClassifier(
                loss='log', max_iter=1000, tol=0.001, **params),
        ),
    ])

    cvresults = cross_validate(
        estimator=pipe,
        X=dtf,
        y=y,
        cv=logo,
        verbose=0,
        scoring='neg_log_loss',
    )

    ret = {
        'loss': -cvresults['test_score'].mean(),
        'status': STATUS_OK,
    }

    return ret


trials = Trials()
best = fmin(
    objective,
    space=space,
    verbose=1,
    algo=tpe.suggest,
    max_evals=200,
    trials=trials,
)

pipe = sklearn.pipeline.Pipeline([
    (
        'featurizer',
        featurizer,
    ),
    (
        'lm',
        sklearn.linear_model.SGDClassifier(
            loss='log',
            alpha=0.09521991362517578,
            fit_intercept=True,
            l1_ratio=0.7125903712801741,
            max_iter=1000,
            shuffle=False,
            penalty='l2',
            tol=0.001,
        ),
    ),
])
