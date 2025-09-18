#!/usr/bin/env python3
"""
Generate updated 2025 NFL season predictions using Week 1 results.
"""

import pandas as pd
import numpy as np
from sportsball.models.team_ratings import SpreadBasedTeamRatings

def generate_updated_predictions():
    """Generate updated 2025 season predictions with Week 1 results."""
    print('🔮 GENERATING UPDATED 2025 PREDICTIONS (WITH WEEK 1 RESULTS)')
    print('=' * 60)
    
    # Load the complete training data with Week 1 2025
    training_data = pd.read_csv('data/raw/nfl_merged_2023_2025_complete.csv')
    
    # Filter to complete training data
    training_data = training_data[
        training_data['spread'].notna() & 
        training_data['away_score'].notna() & 
        training_data['home_score'].notna()
    ].copy()
    
    # Initialize and train model
    model = SpreadBasedTeamRatings(
        lookback_weeks=8,
        home_field_advantage=2.0,
        rating_decay=0.1
    )
    model.fit(training_data)
    print(f'✅ Model trained on {len(training_data)} games including Week 1 2025')
    
    # Load 2025 schedule for predictions
    def normalize_team_name(team_name):
        team_mapping = {
            'Arizona Cardinals': 'ARI', 'Atlanta Falcons': 'ATL', 'Baltimore Ravens': 'BAL',
            'Buffalo Bills': 'BUF', 'Carolina Panthers': 'CAR', 'Chicago Bears': 'CHI',
            'Cincinnati Bengals': 'CIN', 'Cleveland Browns': 'CLE', 'Dallas Cowboys': 'DAL',
            'Denver Broncos': 'DEN', 'Detroit Lions': 'DET', 'Green Bay Packers': 'GB',
            'Houston Texans': 'HOU', 'Indianapolis Colts': 'IND', 'Jacksonville Jaguars': 'JAX',
            'Kansas City Chiefs': 'KC', 'Las Vegas Raiders': 'LV', 'Los Angeles Chargers': 'LAC',
            'Los Angeles Rams': 'LAR', 'Miami Dolphins': 'MIA', 'Minnesota Vikings': 'MIN',
            'New England Patriots': 'NE', 'New Orleans Saints': 'NO', 'New York Giants': 'NYG',
            'New York Jets': 'NYJ', 'Philadelphia Eagles': 'PHI', 'Pittsburgh Steelers': 'PIT',
            'San Francisco 49ers': 'SF', 'Seattle Seahawks': 'SEA', 'Tampa Bay Buccaneers': 'TB',
            'Tennessee Titans': 'TEN', 'Washington Commanders': 'WAS'
        }
        return team_mapping.get(team_name, team_name)
    
    schedule_2025 = pd.read_csv('data/raw/nfl_2025_schedule.csv')
    
    # Process schedule for predictions
    games_2025 = []
    for _, row in schedule_2025.iterrows():
        week = row['Week']
        winner = row['Winner/tie']
        loser = row['Loser/tie']
        at_symbol = row.get('Unnamed: 5', '')
        
        winner_abbr = normalize_team_name(winner)
        loser_abbr = normalize_team_name(loser)
        
        if not winner_abbr or not loser_abbr:
            continue
        
        # Determine home/away based on @ symbol
        if at_symbol == '@':
            home_team = loser_abbr
            away_team = winner_abbr
        else:
            home_team = winner_abbr
            away_team = loser_abbr
        
        games_2025.append({
            'season': 2025,
            'week': week,
            'away_team': away_team,
            'home_team': home_team
        })
    
    games_df = pd.DataFrame(games_2025)
    
    # Generate predictions for all 2025 games
    print(f'🔮 Generating predictions for {len(games_df)} games...')
    predictions = model.predict_games(games_df)
    
    # Add game identifiers
    predictions['game_id'] = (predictions['away_team'] + '_at_' + 
                             predictions['home_team'] + '_week_' + 
                             predictions['week'].astype(str))
    
    # Save updated predictions
    predictions_file = 'data/processed/nfl_2025_predictions_updated.csv'
    predictions.to_csv(predictions_file, index=False)
    
    print(f'\\n✅ UPDATED PREDICTIONS GENERATED')
    print(f'  File: {predictions_file}')
    print(f'  Total games: {len(predictions)}')
    
    # Show sample predictions for Week 2
    print(f'\\n📋 SAMPLE WEEK 2 PREDICTIONS (Updated with Week 1 results):')
    week2 = predictions[predictions['week'] == 2].sort_values('home_win_prob', ascending=False)
    for _, game in week2.head(8).iterrows():
        home_prob = game['home_win_prob']
        away_prob = 1 - home_prob
        print(f'  {game["away_team"]} @ {game["home_team"]}: {away_prob:.1%} vs {home_prob:.1%}')
    
    # Show highest confidence games
    print(f'\\n🎯 HIGHEST CONFIDENCE GAMES (Updated):')
    high_conf = predictions.copy()
    high_conf['max_prob'] = high_conf['home_win_prob'].apply(lambda x: max(x, 1-x))
    high_conf = high_conf.sort_values('max_prob', ascending=False)
    
    for _, game in high_conf.head(8).iterrows():
        home_prob = game['home_win_prob']
        away_prob = 1 - home_prob
        if home_prob > away_prob:
            fav_team = game['home_team']
            fav_prob = home_prob
        else:
            fav_team = game['away_team']
            fav_prob = away_prob
        print(f'  Week {game["week"]}: {game["away_team"]} @ {game["home_team"]} → {fav_team} ({fav_prob:.1%})')
    
    return predictions

if __name__ == "__main__":
    predictions = generate_updated_predictions()
