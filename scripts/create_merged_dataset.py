#!/usr/bin/env python3
"""
Create a properly merged dataset combining scraped odds with real CSV schedule data.
"""

import pandas as pd
import numpy as np
from sportsball.utils.nfl_teams import NFLTeams

def normalize_team_name(team_name):
    """Normalize team names to standard abbreviations."""
    if not team_name:
        return None
    
    # Simple team name mapping
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
    
    # Return abbreviation if found, otherwise return as-is
    return team_mapping.get(team_name, team_name)

def load_and_process_csv_data():
    """Load and process CSV schedule data to get the correct home/away assignments."""
    print("📋 PROCESSING CSV SCHEDULE DATA")
    print("=" * 40)
    
    # Load CSV files
    schedule_2023 = pd.read_csv('data/raw/nfl_2023_schedule.csv')
    schedule_2024 = pd.read_csv('data/raw/nfl_2024_schedule.csv')
    schedule_2025 = pd.read_csv('data/raw/nfl_2025_schedule.csv')
    
    def process_csv_schedule(df, year):
        """Process CSV schedule to extract games with correct home/away."""
        games = []
        
        for _, row in df.iterrows():
            week = row['Week']
            winner = row['Winner/tie']
            loser = row['Loser/tie'] 
            at_symbol = row.get('Unnamed: 5', '')  # The @ column (column 5)
            
            # Normalize team names
            winner = normalize_team_name(winner)
            loser = normalize_team_name(loser)
            
            if not winner or not loser:
                continue
            
            # @ symbol indicates the loser is home (appears before home team column)
            if at_symbol == '@':
                home_team = loser
                away_team = winner
            else:
                home_team = winner
                away_team = loser
            
            # Get scores
            pts_w = row.get('PtsW', np.nan)
            pts_l = row.get('PtsL', np.nan)
            
            # Assign scores correctly
            if at_symbol == '@':
                home_score = pts_l  # Loser is home
                away_score = pts_w  # Winner is away
            else:
                home_score = pts_w  # Winner is home
                away_score = pts_l  # Loser is away
            
            games.append({
                'season': year,
                'week': week,
                'away_team': away_team,
                'home_team': home_team,
                'away_score': away_score,
                'home_score': home_score,
                'game_key': f"{year}_{week}_{away_team}_{home_team}",
                'source': 'csv'
            })
        
        return pd.DataFrame(games)
    
    # Process all years
    csv_2023 = process_csv_schedule(schedule_2023, 2023)
    csv_2024 = process_csv_schedule(schedule_2024, 2024)
    csv_2025_w1 = process_csv_schedule(schedule_2025[schedule_2025['Week'] == 1], 2025)
    
    combined_csv = pd.concat([csv_2023, csv_2024, csv_2025_w1], ignore_index=True)
    
    print(f"Processed CSV data:")
    print(f"  2023: {len(csv_2023)} games")
    print(f"  2024: {len(csv_2024)} games") 
    print(f"  2025 Week 1: {len(csv_2025_w1)} games")
    print(f"  Total: {len(combined_csv)} games")
    
    return combined_csv

def load_and_process_odds_data():
    """Load scraped odds data."""
    print(f"\\n📊 PROCESSING ODDS DATA")
    print("=" * 40)
    
    # Load the scraped odds
    odds_df = pd.read_csv('data/raw/nfl_odds_2020_2024_real.csv')
    
    # Filter to 2023-2024 and normalize team names
    odds_filtered = odds_df[odds_df['season'].isin([2023, 2024])].copy()
    odds_filtered['away_team'] = odds_filtered['away_team'].apply(normalize_team_name)
    odds_filtered['home_team'] = odds_filtered['home_team'].apply(normalize_team_name)
    
    # Create game keys for matching (try both orientations)
    odds_filtered['game_key_1'] = (odds_filtered['season'].astype(str) + '_' + 
                                   odds_filtered['week'].astype(str) + '_' + 
                                   odds_filtered['away_team'] + '_' + 
                                   odds_filtered['home_team'])
    
    odds_filtered['game_key_2'] = (odds_filtered['season'].astype(str) + '_' + 
                                   odds_filtered['week'].astype(str) + '_' + 
                                   odds_filtered['home_team'] + '_' + 
                                   odds_filtered['away_team'])
    
    print(f"Loaded odds data: {len(odds_filtered)} games (2023-2024)")
    
    return odds_filtered

def merge_csv_and_odds_data():
    """Merge CSV and odds data, using CSV for correct home/away assignments."""
    print(f"\\n🔗 MERGING CSV AND ODDS DATA")
    print("=" * 40)
    
    csv_data = load_and_process_csv_data()
    odds_data = load_and_process_odds_data()
    
    merged_games = []
    matches = 0
    
    for _, csv_game in csv_data.iterrows():
        game_key = csv_game['game_key']
        
        # Try to find matching odds data (either orientation)
        odds_match = odds_data[
            (odds_data['game_key_1'] == game_key) | 
            (odds_data['game_key_2'] == game_key)
        ]
        
        if len(odds_match) > 0:
            odds_game = odds_match.iloc[0]
            matches += 1
            
            # Use CSV data for home/away (authoritative) and odds data for betting info
            merged_game = {
                'season': csv_game['season'],
                'week': csv_game['week'],
                'away_team': csv_game['away_team'],
                'home_team': csv_game['home_team'],
                'away_score': csv_game['away_score'],
                'home_score': csv_game['home_score'],
                'spread': odds_game.get('spread'),
                'spread_favorite': odds_game.get('spread_favorite'),
                'spread_line': odds_game.get('spread_line'),
                'total': odds_game.get('total'),
                'game_date': odds_game.get('game_date'),
                'winner': 'away' if float(csv_game['away_score']) > float(csv_game['home_score']) else 'home',
                'source': 'merged'
            }
            
            merged_games.append(merged_game)
        else:
            # No odds data found, use CSV only
            merged_game = {
                'season': csv_game['season'],
                'week': csv_game['week'],
                'away_team': csv_game['away_team'],
                'home_team': csv_game['home_team'],
                'away_score': csv_game['away_score'],
                'home_score': csv_game['home_score'],
                'spread': np.nan,
                'spread_favorite': np.nan,
                'spread_line': np.nan,
                'total': np.nan,
                'game_date': np.nan,
                'winner': 'away' if float(csv_game['away_score']) > float(csv_game['home_score']) else 'home',
                'source': 'csv_only'
            }
            merged_games.append(merged_game)
    
    merged_df = pd.DataFrame(merged_games)
    
    print(f"\\n📊 MERGE RESULTS:")
    print(f"  CSV games: {len(csv_data)}")
    print(f"  Odds matches found: {matches}")
    print(f"  Total merged games: {len(merged_df)}")
    print(f"  Games with odds: {len(merged_df[merged_df['source'] == 'merged'])}")
    print(f"  Games without odds: {len(merged_df[merged_df['source'] == 'csv_only'])}")
    
    # Show sample of merged data
    print(f"\\n📋 SAMPLE MERGED DATA (Week 9 2024):")
    week9_sample = merged_df[(merged_df['season'] == 2024) & (merged_df['week'] == 9)]
    for _, game in week9_sample.head().iterrows():
        spread_info = f"Spread: {game['spread']}" if pd.notna(game['spread']) else "No spread"
        print(f"  {game['away_team']} @ {game['home_team']} - Score: {game['away_score']}-{game['home_score']} - {spread_info}")
    
    return merged_df

def save_merged_dataset():
    """Create and save the final merged dataset."""
    print(f"\\n💾 CREATING FINAL DATASET")
    print("=" * 40)
    
    merged_df = merge_csv_and_odds_data()
    
    # Save the merged dataset
    output_file = 'data/raw/nfl_merged_2023_2024_real.csv'
    merged_df.to_csv(output_file, index=False)
    
    print(f"\\n✅ SAVED MERGED DATASET:")
    print(f"  File: {output_file}")
    print(f"  Total games: {len(merged_df)}")
    print(f"  Seasons: {sorted(merged_df['season'].unique())}")
    print(f"  Weeks per season: {merged_df.groupby('season')['week'].nunique().to_dict()}")
    
    # Show data quality
    with_odds = len(merged_df[merged_df['source'] == 'merged'])
    without_odds = len(merged_df[merged_df['source'] == 'csv_only'])
    
    print(f"\\n📈 DATA QUALITY:")
    print(f"  Games with odds data: {with_odds} ({with_odds/len(merged_df)*100:.1f}%)")
    print(f"  Games without odds: {without_odds} ({without_odds/len(merged_df)*100:.1f}%)")
    
    return merged_df

if __name__ == "__main__":
    save_merged_dataset()
