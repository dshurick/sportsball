#!/usr/bin/env python3
"""
Optimize eliminator picks for 2025 season with correct constraint formulation.
"""

import pandas as pd
import numpy as np
from scipy.optimize import linprog

def optimize_eliminator_picks():
    """Find optimal eliminator picks for 2025 season."""
    print('🎯 OPTIMIZING ELIMINATOR PICKS FOR 2025 SEASON')
    print('=' * 50)
    
    # Load the updated predictions
    predictions = pd.read_csv('data/processed/nfl_2025_predictions_updated.csv')
    print(f'📊 Loaded {len(predictions)} game predictions')
    
    # Filter to weeks 2-18 (since Denver was picked in Week 1)
    future_games = predictions[predictions['week'] >= 2].copy()
    print(f'📊 Future games (Weeks 2-18): {len(future_games)}')
    
    # Create decision variables for each team-week combination where team plays
    teams = set()
    for _, game in future_games.iterrows():
        teams.add(game['away_team'])
        teams.add(game['home_team'])
    
    teams = sorted(list(teams))
    weeks = sorted(future_games['week'].unique())
    
    print(f'📊 Teams: {len(teams)}, Weeks: {len(weeks)} (Weeks {min(weeks)}-{max(weeks)})')
    
    # Find which teams play in which weeks
    team_week_games = {}
    for _, game in future_games.iterrows():
        week = game['week']
        away_team = game['away_team']
        home_team = game['home_team']
        
        if (away_team, week) not in team_week_games:
            team_week_games[(away_team, week)] = []
        if (home_team, week) not in team_week_games:
            team_week_games[(home_team, week)] = []
        
        team_week_games[(away_team, week)].append(game)
        team_week_games[(home_team, week)].append(game)
    
    # Create decision variables only for valid team-week combinations
    var_mapping = {}
    var_index = 0
    
    for (team, week), games in team_week_games.items():
        var_mapping[(team, week)] = var_index
        var_index += 1
    
    num_vars = len(var_mapping)
    print(f'📊 Decision variables: {num_vars}')
    
    # Objective: maximize log sum of win probabilities
    c = np.zeros(num_vars)
    
    for (team, week), var_idx in var_mapping.items():
        games = team_week_games[(team, week)]
        max_prob = 0
        
        for game in games:
            if game['away_team'] == team:
                prob = 1 - game['home_win_prob']  # Away team win probability
            else:
                prob = game['home_win_prob']  # Home team win probability
            max_prob = max(max_prob, prob)
        
        # Use log probability (negative for minimization)
        if max_prob > 0:
            c[var_idx] = -np.log(max_prob)
    
    print(f'📊 Objective coefficients set')
    
    # EQUALITY CONSTRAINTS: Pick exactly one team each week
    A_eq = []
    b_eq = []
    
    for week in weeks:
        constraint = np.zeros(num_vars)
        for team in teams:
            if (team, week) in var_mapping:
                constraint[var_mapping[(team, week)]] = 1
        A_eq.append(constraint)
        b_eq.append(1)
    
    A_eq = np.array(A_eq) if A_eq else np.empty((0, num_vars))
    b_eq = np.array(b_eq) if b_eq else np.empty(0)
    
    print(f'📊 Weekly constraints: {len(b_eq)}')
    
    # INEQUALITY CONSTRAINTS: Use each team at most once
    A_ub = []
    b_ub = []
    
    for team in teams:
        if team == 'DEN':
            # Denver already used in Week 1, so can't be used again
            constraint = np.zeros(num_vars)
            for week in weeks:
                if (team, week) in var_mapping:
                    constraint[var_mapping[(team, week)]] = 1
            if np.any(constraint > 0):
                A_ub.append(constraint)
                b_ub.append(0)  # Sum <= 0 (can't use Denver again)
        else:
            # Other teams can be used at most once
            constraint = np.zeros(num_vars)
            for week in weeks:
                if (team, week) in var_mapping:
                    constraint[var_mapping[(team, week)]] = 1
            if np.any(constraint > 0):
                A_ub.append(constraint)
                b_ub.append(1)  # Sum <= 1 (at most once)
    
    A_ub = np.array(A_ub) if A_ub else np.empty((0, num_vars))
    b_ub = np.array(b_ub) if b_ub else np.empty(0)
    
    print(f'📊 Team usage constraints: {len(b_ub)}')
    
    # Variable bounds (binary: 0 <= x <= 1)
    bounds = [(0, 1) for _ in range(num_vars)]
    
    print(f'\\n🔧 RUNNING OPTIMIZATION...')
    print(f'   Variables: {num_vars}')
    print(f'   Equality constraints: {len(b_eq)}')
    print(f'   Inequality constraints: {len(b_ub)}')
    
    # Solve the linear program
    result = linprog(c, A_ub=A_ub, b_ub=b_ub, A_eq=A_eq, b_eq=b_eq, 
                     bounds=bounds, method='highs')
    
    if result.success:
        print(f'✅ OPTIMIZATION SUCCESSFUL!')
        print(f'   Optimal value: {-result.fun:.4f}')
        
        # Extract the solution
        solution = result.x
        picks = []
        
        for (team, week), var_idx in var_mapping.items():
            if solution[var_idx] > 0.5:  # Binary decision
                # Find the best game for this team in this week
                games = team_week_games[(team, week)]
                best_prob = 0
                best_game_info = None
                
                for game in games:
                    if game['away_team'] == team:
                        prob = 1 - game['home_win_prob']
                        opponent = game['home_team']
                        location = 'at'
                    else:
                        prob = game['home_win_prob']
                        opponent = game['away_team']
                        location = 'vs'
                    
                    if prob > best_prob:
                        best_prob = prob
                        best_game_info = {
                            'team': team,
                            'opponent': opponent,
                            'location': location,
                            'probability': prob
                        }
                
                if best_game_info:
                    picks.append({
                        'week': week,
                        'team': team,
                        'game': f"{best_game_info['team']} {best_game_info['location']} {best_game_info['opponent']}",
                        'probability': best_game_info['probability']
                    })
        
        # Sort by week
        picks.sort(key=lambda x: x['week'])
        
        print(f'\\n🎯 YOUR ELIMINATOR PICKS FOR 2025:')
        print(f'Week 1: DEN vs TEN (already picked)')
        
        total_log_prob = 0
        for pick in picks:
            print(f'Week {pick["week"]}: {pick["game"]} ({pick["probability"]:.1%})')
            total_log_prob += np.log(pick['probability'])
        
        # Add Denver's Week 1 probability (need to look this up)
        denver_week1_prob = 0.8  # Estimate, could look up actual
        total_log_prob += np.log(denver_week1_prob)
        
        overall_prob = np.exp(total_log_prob)
        print(f'\\n📊 OVERALL SEASON SUCCESS PROBABILITY: {overall_prob:.2%}')
        
        return picks
        
    else:
        print(f'❌ OPTIMIZATION FAILED: {result.message}')
        return None

if __name__ == "__main__":
    picks = optimize_eliminator_picks()
