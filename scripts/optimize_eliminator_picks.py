#!/usr/bin/env python3
"""
Optimize eliminator picks for any NFL season starting from any week.
Supports specifying already used teams from previous weeks.
"""

import pandas as pd
import numpy as np
import argparse
from pathlib import Path
from scipy.optimize import linprog

def parse_used_teams(used_teams_str: str) -> dict:
    """Parse used teams string into week->team mapping.
    
    Format: "1:DEN,2:KC,3:BUF" or "DEN,KC,BUF" (assumes sequential weeks)
    """
    if not used_teams_str:
        return {}
    
    used_teams = {}
    teams_list = used_teams_str.split(',')
    
    for i, team_entry in enumerate(teams_list):
        team_entry = team_entry.strip()
        if ':' in team_entry:
            # Format: "week:team"
            week_str, team = team_entry.split(':', 1)
            week = int(week_str)
            used_teams[week] = team.strip().upper()
        else:
            # Format: just team names, assume sequential weeks starting from 1
            week = i + 1
            used_teams[week] = team_entry.strip().upper()
    
    return used_teams

def optimize_eliminator_picks(predictions_file: str = None, season: int = None, 
                            start_week: int = None, used_teams: dict = None):
    """Find optimal eliminator picks starting from any week with used teams constraints."""
    
    # Set defaults
    if used_teams is None:
        used_teams = {}
    if season is None:
        season = 2025
    if start_week is None:
        start_week = max(used_teams.keys()) + 1 if used_teams else 1
    if predictions_file is None:
        predictions_file = f'data/processed/nfl_{season}_predictions_updated.csv'
    
    print(f'🎯 OPTIMIZING ELIMINATOR PICKS FOR {season} SEASON')
    print('=' * 60)
    print(f'📂 Predictions: {predictions_file}')
    print(f'📅 Season: {season}')
    print(f'🚀 Starting from Week: {start_week}')
    
    if used_teams:
        print(f'🏈 Already used teams:')
        for week in sorted(used_teams.keys()):
            print(f'   Week {week}: {used_teams[week]}')
    else:
        print(f'🏈 No teams used yet')
    print()
    
    # Load the predictions
    try:
        predictions = pd.read_csv(predictions_file)
    except FileNotFoundError:
        print(f'❌ Predictions file not found: {predictions_file}')
        print(f'   Make sure to run generate predictions for {season} season first.')
        return None
    
    # Filter to the target season and future weeks
    season_games = predictions[predictions['season'] == season].copy()
    if len(season_games) == 0:
        print(f'❌ No games found for {season} season in predictions file')
        return None
    
    future_games = season_games[season_games['week'] >= start_week].copy()
    print(f'📊 Loaded {len(predictions)} total game predictions')
    print(f'📊 {season} season games: {len(season_games)}')
    print(f'📊 Future games (Week {start_week}+): {len(future_games)}')
    
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
    
    # INEQUALITY CONSTRAINTS: Use each team at most once (considering already used teams)
    A_ub = []
    b_ub = []
    
    used_team_names = set(used_teams.values())
    
    for team in teams:
        constraint = np.zeros(num_vars)
        for week in weeks:
            if (team, week) in var_mapping:
                constraint[var_mapping[(team, week)]] = 1
        
        if np.any(constraint > 0):
            if team in used_team_names:
                # Team already used in previous weeks, can't be used again
                A_ub.append(constraint)
                b_ub.append(0)  # Sum <= 0 (can't use this team again)
            else:
                # Team not yet used, can be used at most once
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
        
        print(f'\\n🎯 YOUR OPTIMAL ELIMINATOR PICKS FOR {season}:')
        
        # Show already used picks
        if used_teams:
            for week in sorted(used_teams.keys()):
                print(f'Week {week}: {used_teams[week]} (already picked)')
        
        # Show optimized picks
        total_log_prob = 0
        for pick in picks:
            print(f'Week {pick["week"]}: {pick["game"]} ({pick["probability"]:.1%})')
            total_log_prob += np.log(pick['probability'])
        
        # Add probabilities for already used picks (estimate if not available)
        for week in sorted(used_teams.keys()):
            # Try to find actual probability for used picks
            used_team = used_teams[week]
            week_games = season_games[season_games['week'] == week]
            
            found_prob = None
            for _, game in week_games.iterrows():
                if game['away_team'] == used_team:
                    found_prob = 1 - game['home_win_prob']
                    break
                elif game['home_team'] == used_team:
                    found_prob = game['home_win_prob']
                    break
            
            if found_prob is not None:
                total_log_prob += np.log(found_prob)
            else:
                # Estimate if we can't find the actual game
                estimated_prob = 0.65  # Conservative estimate
                total_log_prob += np.log(estimated_prob)
        
        overall_prob = np.exp(total_log_prob)
        print(f'\\n📊 OVERALL SEASON SUCCESS PROBABILITY: {overall_prob:.2%}')
        
        return picks
        
    else:
        print(f'❌ OPTIMIZATION FAILED: {result.message}')
        return None

def main():
    """Main function with command line argument parsing."""
    parser = argparse.ArgumentParser(
        description="Optimize NFL eliminator picks for any season starting from any week",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # 2025 season starting from Week 1 (no previous picks)
  python scripts/optimize_eliminator_picks.py
  
  # 2025 season starting from Week 3, with Week 1 and 2 picks
  python scripts/optimize_eliminator_picks.py --used-teams "DEN,KC"
  
  # Specific week format
  python scripts/optimize_eliminator_picks.py --used-teams "1:DEN,2:KC,4:BUF"
  
  # 2026 season (when available)
  python scripts/optimize_eliminator_picks.py --season 2026 --predictions-file data/processed/nfl_2026_predictions_updated.csv
  
  # Mid-season optimization from Week 8
  python scripts/optimize_eliminator_picks.py --start-week 8 --used-teams "DEN,KC,BUF,MIA,BAL,SF,GB"
        """
    )
    
    parser.add_argument('--predictions-file', 
                       help='Path to predictions file (default: auto-generated based on season)')
    parser.add_argument('--season', type=int, default=2025,
                       help='NFL season year (default: 2025)')
    parser.add_argument('--start-week', type=int,
                       help='Week to start optimization from (default: auto-detect from used teams or 1)')
    parser.add_argument('--used-teams', 
                       help='Already used teams. Format: "DEN,KC,BUF" or "1:DEN,2:KC,3:BUF"')
    
    args = parser.parse_args()
    
    # Parse used teams
    used_teams_dict = parse_used_teams(args.used_teams or "")
    
    picks = optimize_eliminator_picks(
        predictions_file=args.predictions_file,
        season=args.season,
        start_week=args.start_week,
        used_teams=used_teams_dict
    )
    
    return picks

if __name__ == "__main__":
    main()
