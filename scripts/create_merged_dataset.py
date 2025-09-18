#!/usr/bin/env python3
"""
Create a properly merged dataset combining scraped odds with real CSV schedule data.
"""

import pandas as pd
import numpy as np
import argparse
from pathlib import Path
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

def load_and_process_csv_data(csv_files):
    """Load and process CSV schedule data to get the correct home/away assignments."""
    print("📋 PROCESSING CSV SCHEDULE DATA")
    print("=" * 40)
    
    # Load CSV files
    schedule_dataframes = {}
    for year, filepath in csv_files.items():
        if Path(filepath).exists():
            schedule_dataframes[year] = pd.read_csv(filepath)
            print(f"  Loaded {filepath} for {year}")
        else:
            print(f"  ⚠️  Warning: {filepath} not found, skipping {year}")
    
    if not schedule_dataframes:
        raise FileNotFoundError("No valid CSV schedule files found")
    
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
    processed_dataframes = []
    game_counts = {}
    
    for year, df in schedule_dataframes.items():
        if year == 2025:
            # For 2025, only include completed games (games with scores)
            completed_games = df[df['PtsW'].notna() & df['PtsL'].notna()]
            if len(completed_games) > 0:
                processed_df = process_csv_schedule(completed_games, year)
                processed_dataframes.append(processed_df)
                game_counts[year] = len(processed_df)
                print(f"  2025: {len(processed_df)} completed games (from {len(df)} total games)")
            else:
                print(f"  2025: No completed games found")
        else:
            # For historical years, include all games
            processed_df = process_csv_schedule(df, year)
            processed_dataframes.append(processed_df)
            game_counts[year] = len(processed_df)
    
    if not processed_dataframes:
        raise ValueError("No games found in any CSV files")
    
    combined_csv = pd.concat(processed_dataframes, ignore_index=True)
    
    print(f"Processed CSV data:")
    for year, count in game_counts.items():
        print(f"  {year}: {count} games")
    print(f"  Total: {len(combined_csv)} games")
    
    return combined_csv

def load_and_process_odds_data(odds_files):
    """Load scraped odds data."""
    print(f"\\n📊 PROCESSING ODDS DATA")
    print("=" * 40)
    
    # Load all odds files
    odds_dataframes = []
    for filepath in odds_files:
        if Path(filepath).exists():
            odds_df = pd.read_csv(filepath)
            odds_dataframes.append(odds_df)
            print(f"  Loaded {filepath} ({len(odds_df)} games)")
        else:
            print(f"  ⚠️  Warning: {filepath} not found, skipping")
    
    if not odds_dataframes:
        print("  ⚠️  No odds files found, proceeding without odds data")
        return pd.DataFrame()
    
    # Combine all odds data
    odds_df = pd.concat(odds_dataframes, ignore_index=True)
    
    # Normalize team names
    odds_df['away_team'] = odds_df['away_team'].apply(normalize_team_name)
    odds_df['home_team'] = odds_df['home_team'].apply(normalize_team_name)
    
    # Create game keys for matching (try both orientations)
    odds_df['game_key_1'] = (odds_df['season'].astype(str) + '_' + 
                             odds_df['week'].astype(str) + '_' + 
                             odds_df['away_team'] + '_' + 
                             odds_df['home_team'])
    
    odds_df['game_key_2'] = (odds_df['season'].astype(str) + '_' + 
                             odds_df['week'].astype(str) + '_' + 
                             odds_df['home_team'] + '_' + 
                             odds_df['away_team'])
    
    print(f"Total odds data: {len(odds_df)} games")
    print(f"Seasons: {sorted(odds_df['season'].unique())}")
    
    return odds_df

def merge_csv_and_odds_data(csv_files, odds_files):
    """Merge CSV and odds data, using CSV for correct home/away assignments."""
    print(f"\\n🔗 MERGING CSV AND ODDS DATA")
    print("=" * 40)
    
    csv_data = load_and_process_csv_data(csv_files)
    odds_data = load_and_process_odds_data(odds_files)
    
    merged_games = []
    matches = 0
    
    # Handle case where no odds data is available
    if len(odds_data) == 0:
        print("  No odds data available, using CSV data only")
        for _, csv_game in csv_data.iterrows():
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
    else:
        # Merge with odds data
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

def save_merged_dataset(csv_files, odds_files, output_file):
    """Create and save the final merged dataset."""
    print(f"\\n💾 CREATING FINAL DATASET")
    print("=" * 40)
    
    merged_df = merge_csv_and_odds_data(csv_files, odds_files)
    
    # Save the merged dataset
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

def parse_arguments():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Create merged dataset combining scraped odds with CSV schedule data",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Default files (2023-2025)
  python scripts/create_merged_dataset.py
  
  # Custom files
  python scripts/create_merged_dataset.py \\
    --csv-2023 data/raw/nfl_2023_schedule.csv \\
    --csv-2024 data/raw/nfl_2024_schedule.csv \\
    --csv-2025 data/raw/nfl_2025_schedule.csv \\
    --odds data/raw/nfl_odds_2020_2024_real.csv data/raw/nfl_odds_2025_week1.csv \\
    --output data/raw/nfl_merged_complete.csv
        """
    )
    
    # CSV schedule files
    parser.add_argument('--csv-2023', default='data/raw/nfl_2023_schedule.csv',
                       help='Path to 2023 NFL schedule CSV file')
    parser.add_argument('--csv-2024', default='data/raw/nfl_2024_schedule.csv',
                       help='Path to 2024 NFL schedule CSV file')
    parser.add_argument('--csv-2025', default='data/raw/nfl_2025_schedule.csv',
                       help='Path to 2025 NFL schedule CSV file')
    
    # Odds files (can specify multiple)
    parser.add_argument('--odds', nargs='+', 
                       default=None,
                       help='Path(s) to scraped odds CSV files (default: auto-detect latest)')
    
    # Output file
    parser.add_argument('--output', default='data/raw/nfl_merged_2023_2025_complete.csv',
                       help='Output path for merged dataset')
    
    return parser.parse_args()

def find_latest_odds_file():
    """Find the most recent odds file in data/raw/."""
    import glob
    import os
    
    # Look for odds files in data/raw/
    odds_pattern = "data/raw/nfl_odds_*_complete_*.csv"
    odds_files = glob.glob(odds_pattern)
    
    if not odds_files:
        # Fallback to older pattern
        odds_pattern = "data/raw/nfl_odds_*_*.csv"
        odds_files = glob.glob(odds_pattern)
    
    if odds_files:
        # Sort by modification time, most recent first
        odds_files.sort(key=os.path.getmtime, reverse=True)
        return [odds_files[0]]
    
    # Final fallback to default files
    return ['data/raw/nfl_odds_2020_2024_real.csv', 'data/raw/nfl_odds_2025_week1.csv']

def main():
    """Main function."""
    args = parse_arguments()
    
    # Build CSV files dictionary
    csv_files = {
        2023: args.csv_2023,
        2024: args.csv_2024,
        2025: args.csv_2025
    }
    
    # Auto-detect odds files if not specified
    if args.odds is None:
        args.odds = find_latest_odds_file()
        print(f"🔍 Auto-detected odds file: {args.odds[0]}")
    
    print("🏈 NFL DATA MERGER")
    print("=" * 50)
    print(f"CSV Files:")
    for year, filepath in csv_files.items():
        print(f"  {year}: {filepath}")
    print(f"Odds Files:")
    for filepath in args.odds:
        print(f"  {filepath}")
    print(f"Output: {args.output}")
    print()
    
    # Create merged dataset
    save_merged_dataset(csv_files, args.odds, args.output)

if __name__ == "__main__":
    main()
