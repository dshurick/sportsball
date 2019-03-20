import pandas as pd
from patsy.contrasts import Sum
from scipy.sparse import csr_matrix, hstack
from scipy.sparse.linalg import lsqr


def load_compact_data():
    season_cresults = pd.read_csv(
        '../input/datafiles/RegularSeasonCompactResults.csv')
    tourney_cresults = pd.read_csv(
        '../input/datafiles/NCAATourneyCompactResults.csv')
    secondary_tourney_cresults = pd.read_csv(
        '../input/datafiles/SecondaryTourneyCompactResults.csv')
    season_cresults['CRType'] = 'Regular'
    tourney_cresults['CRType'] = 'NCAA'
    secondary_tourney_cresults['CRType'] = 'Secondary'

    compact1 = pd.concat(
        [
            season_cresults,
            tourney_cresults,
            secondary_tourney_cresults,
        ],
        ignore_index=True,
        join='inner').rename(columns=str.lower)

    compact1 = compact1.rename(
        columns={
            'wteamid': 'team1',
            'wscore': 'score1',
            'lteamid': 'team2',
            'lscore': 'score2',
            'wloc': 'team1loc'
        })

    compact2 = compact1.rename(
        columns={
            'team1': 'team2',
            'score1': 'score2',
            'team2': 'team1',
            'score2': 'score1',
        })

    compact2['team1loc'] = compact2.team1loc.map({
        'H': 'A',
        'A': 'H',
        'N': 'N',
    })

    compact = pd.concat(
        [
            compact1,
            compact2,
        ], ignore_index=True, join='inner')

    compact['team1loc_num'] = compact.team1loc.map({
        'H': 1,
        'A': -1,
        'N': 0,
    })

    return compact


def load_detailed_data():
    season_dresults = pd.read_csv(
        '../input/datafiles/RegularSeasonDetailedResults.csv')
    tourney_dresults = pd.read_csv(
        '../input/datafiles/NCAATourneyDetailedResults.csv')

    season_dresults['tourney'] = False
    tourney_dresults['tourney'] = True

    detailed1 = pd.concat(
        [
            season_dresults,
            tourney_dresults,
        ], ignore_index=True, join='inner')

    detailed1 = detailed1.rename(columns=str.lower)
    detailed2 = detailed1.copy()

    detailed1 = detailed1.rename(
        columns={
            'wteamid': 'team1',
            'wscore': 'score1',
            'lteamid': 'team2',
            'lscore': 'score2',
            'wloc': 'team1loc',
            'wfgm': 'fgm1',
            'wfga': 'fga1',
            'wfgm3': 'fgm31',
            'wfga3': 'fga31',
            'wftm': 'ftm1',
            'wfta': 'fta1',
            'wor': 'or1',
            'wdr': 'dr1',
            'wast': 'ast1',
            'wto': 'to1',
            'wstl': 'stl1',
            'wblk': 'blk1',
            'wpf': 'pf1',
            'lfgm': 'fgm2',
            'lfga': 'fga2',
            'lfgm3': 'fgm32',
            'lfga3': 'fga32',
            'lftm': 'ftm2',
            'lfta': 'fta2',
            'lor': 'or2',
            'ldr': 'dr2',
            'last': 'ast2',
            'lto': 'to2',
            'lstl': 'stl2',
            'lblk': 'blk2',
            'lpf': 'pf2',
        })

    detailed2 = detailed2.rename(
        columns={
            'wteamid': 'team2',
            'wscore': 'score2',
            'lteamid': 'team1',
            'lscore': 'score1',
            'wloc': 'team1loc',
            'wfgm': 'fgm2',
            'wfga': 'fga2',
            'wfgm3': 'fgm32',
            'wfga3': 'fga32',
            'wftm': 'ftm2',
            'wfta': 'fta2',
            'wor': 'or2',
            'wdr': 'dr2',
            'wast': 'ast2',
            'wto': 'to2',
            'wstl': 'stl2',
            'wblk': 'blk2',
            'wpf': 'pf2',
            'lfgm': 'fgm1',
            'lfga': 'fga1',
            'lfgm3': 'fgm31',
            'lfga3': 'fga31',
            'lftm': 'ftm1',
            'lfta': 'fta1',
            'lor': 'or1',
            'ldr': 'dr1',
            'last': 'ast1',
            'lto': 'to1',
            'lstl': 'stl1',
            'lblk': 'blk1',
            'lpf': 'pf1',
        })

    detailed2['team1loc'] = detailed2.team1loc.map({
        'H': 'A',
        'A': 'H',
        'N': 'N',
    })

    detailed = pd.concat(
        [
            detailed1,
            detailed2,
        ], ignore_index=True, join='inner')

    detailed['teamseason1'] = detailed.season.astype(str).str.cat(
        detailed.team1.astype(str), sep=':')
    detailed['teamseason2'] = detailed.season.astype(str).str.cat(
        detailed.team2.astype(str), sep=':')

    detailed['team1'] = pd.Categorical(detailed['team1'])
    detailed['team2'] = pd.Categorical(detailed['team2'])
    detailed['teamseason1'] = pd.Categorical(detailed['teamseason1'])
    detailed['teamseason2'] = pd.Categorical(detailed['teamseason2'])
    detailed['team1loc'] = pd.Categorical(detailed['team1loc'])
    detailed['season'] = pd.Categorical(detailed['season'])
    detailed['team1win'] = pd.Categorical(
        detailed['score1'] > detailed['score2'])

    detailed['poss'] = (
        (detailed.fga1 + 0.475 * detailed.fta1 - detailed.or1 + detailed.to1) /
        2 + (detailed.fga2 + 0.475 * detailed.fta2 - detailed.or2 +
             detailed.to2) / 2)

    detailed['oe1'] = 100 * detailed.score1 / detailed.poss
    detailed['oe2'] = 100 * detailed.score2 / detailed.poss
    detailed['de1'] = detailed['oe2']
    detailed['de2'] = detailed['oe1']

    # eFG%  = (.5*3FGM + FGM) / FGA
    detailed['eFG1'] = (.5 * detailed.fgm31 + detailed.fgm1) / detailed.fga1
    detailed['eFG2'] = (.5 * detailed.fgm32 + detailed.fgm2) / detailed.fga2

    # TO% = TO / Possessions
    detailed['topct1'] = detailed.to1 / detailed.poss
    detailed['topct2'] = detailed.to2 / detailed.poss

    # OR% = OR / (OR + DR)
    detailed['orpct1'] = detailed.or1 / (detailed.or1 + detailed.dr2)
    detailed['orpct2'] = detailed.or2 / (detailed.or2 + detailed.dr1)

    # FTRate = FTA / FGA
    detailed['ftrate1'] = detailed.fta1 / detailed.fga1
    detailed['ftrate2'] = detailed.fta2 / detailed.fga2

    # shotopp1 = (fga1 + 0.475 * fta1) / poss
    detailed['shotopp1'] = (
        (detailed.fga1 + 0.475 * detailed.fta1) / detailed.poss)
    detailed['shotopp2'] = (
        (detailed.fga2 + 0.475 * detailed.fta2) / detailed.poss)

    # tspct1 = score1 / (2 * (fga1 + 0.475 * fta1))
    detailed['tspct1'] = (
        detailed.score1 / (2 * (detailed.fga1 + 0.475 * detailed.fta1)))
    detailed['tspct2'] = (
        detailed.score2 / (2 * (detailed.fga2 + 0.475 * detailed.fta2)))

    detailed['tempo'] = 40 * detailed.poss / (40 + 5 * detailed.numot)

    detailed['team1loc_num'] = detailed.team1loc.map({
        'H': 1,
        'A': -1,
        'N': 0,
    })

    return detailed


def design_matrix(games):
    contrast = Sum().code_with_intercept(
        games.teamseason1.cat.categories.tolist())

    matrix_team1 = csr_matrix(contrast.matrix)
    matrix_team2 = csr_matrix(contrast.matrix)[:, 1:]

    contrast_matrix_team1 = hstack(
        (matrix_team1, csr_matrix(matrix_team2.shape)), format='csr')
    contrast_matrix_team2 = hstack(
        (csr_matrix(matrix_team1.shape), matrix_team2), format='csr')

    X1 = contrast_matrix_team1[games.team1.cat.codes.tolist()]
    X2 = contrast_matrix_team2[games.team2.cat.codes.tolist()]
    X = X1 + X2
    return X, contrast_matrix_team1


def calc_adj_metrics(games):
    X, contrast_matrix_team1 = design_matrix(games)
    X = hstack([X, games.team1loc_num.values.reshape(-1, 1)])
    contrast_matrix_team1 = hstack([
        contrast_matrix_team1,
        csr_matrix((contrast_matrix_team1.shape[0], 1))
    ])

    # First let's do Adjusted Offensive Efficiency
    solution = lsqr(X, detailed.oe1.values)
    AdjOE = pd.DataFrame({
        'teamseason':
        detailed.teamseason1.cat.categories.tolist(),
        'AdjOE':
        contrast_matrix_team1.dot(solution[0])
    })

    # Now let's do Adjusted Defensive Efficiency
    solution = lsqr(X, detailed.de1.values)
    AdjDE = pd.DataFrame({
        'teamseason':
        detailed.teamseason1.cat.categories.tolist(),
        'AdjDE':
        contrast_matrix_team1.dot(solution[0])
    })

    # Adjusted Tempo
    solution = lsqr(X, detailed.tempo.values)
    AdjTempo = pd.DataFrame({
        'teamseason':
        detailed.teamseason1.cat.categories.tolist(),
        'AdjTempo':
        contrast_matrix_team1.dot(solution[0])
    })

    return AdjOE.merge(AdjDE, on='teamseason').merge(AdjTempo, on='teamseason')


if __name__ == '__main__':
    SecondaryTourneyCompactResults = pd.read_csv(
        '../input/datafiles/SecondaryTourneyCompactResults.csv')
    ConferenceTourneyGames = pd.read_csv(
        '../input/datafiles/ConferenceTourneyGames.csv')
    GameCities = pd.read_csv('../input/datafiles/GameCities.csv')
    SecondaryTourneyTeams = pd.read_csv(
        '../input/datafiles/SecondaryTourneyTeams.csv')

    teams = pd.read_csv('../input/datafiles/Teams.csv')
    teams2 = pd.read_csv(
        '../input/datafiles/TeamSpellings.csv', encoding='latin-1')
    season_cresults = pd.read_csv(
        '../input/datafiles/RegularSeasonCompactResults.csv')
    season_dresults = pd.read_csv(
        '../input/datafiles/RegularSeasonDetailedResults.csv')
    tourney_cresults = pd.read_csv(
        '../input/datafiles/NCAATourneyCompactResults.csv')
    tourney_dresults = pd.read_csv(
        '../input/datafiles/NCAATourneyDetailedResults.csv')
    slots = pd.read_csv('../input/datafiles/NCAATourneySlots.csv')
    seeds = pd.read_csv('../input/datafiles/NCAATourneySeeds.csv')
    compact = load_compact_data()
    detailed = load_detailed_data()
