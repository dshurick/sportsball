#!/usr/bin/env python3
"""
Script to set up historical NFL data for model training.

This script helps convert your Google Sheets historical data
into the format needed for training the win probability model.
"""

import pandas as pd
from pathlib import Path
import argparse
from loguru import logger

from sportsball.utils.config import config
from sportsball.utils.nfl_teams import NFLTeams


def load_google_sheets_data(file_path: str) -> pd.DataFrame:
    """
    Load historical data from Google Sheets export.
    
    Args:
        file_path: Path to CSV file exported from Google Sheets
        
    Returns:
        Processed DataFrame
    """
    logger.info(f"Loading historical data from {file_path}")
    
    try:
        df = pd.read_csv(file_path)
        logger.info(f"Loaded {len(df)} rows from {file_path}")
        return df
    except Exception as e:
        logger.error(f"Failed to load data: {e}")
        raise


def standardize_historical_data(df: pd.DataFrame) -> pd.DataFrame:
    """
    Standardize historical data format.
    
    Args:
        df: Raw historical data
        
    Returns:
        Standardized DataFrame
    """
    logger.info("Standardizing historical data format")
    
    # Common column mappings from your Google Sheets
    column_mappings = {
        'away': 'away_team',
        'home': 'home_team', 
        'visitor': 'away_team',
        'host': 'home_team',
        'away_pts': 'away_score',
        'home_pts': 'home_score',
        'visitor_pts': 'away_score',
        'host_pts': 'home_score',
        'away_prob': 'away_win_prob',
        'home_prob': 'home_win_prob',
        'vegas_away_prob': 'away_win_prob',
        'vegas_home_prob': 'home_win_prob'
    }
    
    # Rename columns
    df = df.rename(columns=column_mappings)
    
    # Standardize team names
    nfl_teams = NFLTeams()
    for col in ['away_team', 'home_team']:
        if col in df.columns:
            df[col] = df[col].apply(lambda x: nfl_teams.get_abbreviation(str(x)) or str(x))
    
    # Ensure required columns exist
    required_columns = ['season', 'week', 'away_team', 'home_team']
    for col in required_columns:
        if col not in df.columns:
            if col == 'season':
                # Try to infer season from other columns or use default
                df[col] = 2023  # Default season
            elif col == 'week':
                df[col] = 1  # Default week
            else:
                logger.error(f"Missing required column: {col}")
                raise ValueError(f"Missing required column: {col}")
    
    # Add game outcome if scores are available
    if 'away_score' in df.columns and 'home_score' in df.columns:
        df['away_team_win'] = (df['away_score'] > df['home_score']).astype(int)
    
    # Convert probabilities from percentages if needed
    for col in ['away_win_prob', 'home_win_prob']:
        if col in df.columns:
            # If values are > 1, assume they're percentages
            if df[col].max() > 1:
                df[col] = df[col] / 100
    
    logger.info(f"Standardized data: {len(df)} games across {df['season'].nunique()} seasons")
    
    return df


def add_derived_features(df: pd.DataFrame) -> pd.DataFrame:
    """
    Add derived features for model training.
    
    Args:
        df: Standardized historical data
        
    Returns:
        DataFrame with additional features
    """
    logger.info("Adding derived features")
    
    nfl_teams = NFLTeams()
    
    # Home field advantage (always 1 for home team)
    df['home_field_advantage'] = 1
    
    # Division game indicator
    def is_division_game(row):
        away_team = nfl_teams.get_team(row['away_team'])
        home_team = nfl_teams.get_team(row['home_team'])
        if away_team and home_team:
            return int(away_team.division == home_team.division)
        return 0
    
    df['division_game'] = df.apply(is_division_game, axis=1)
    
    # Conference game indicator
    def is_conference_game(row):
        away_team = nfl_teams.get_team(row['away_team'])
        home_team = nfl_teams.get_team(row['home_team'])
        if away_team and home_team:
            return int(away_team.conference == home_team.conference)
        return 0
    
    df['conference_game'] = df.apply(is_conference_game, axis=1)
    
    # Week-based features
    df['early_season'] = (df['week'] <= 4).astype(int)
    df['late_season'] = (df['week'] >= 15).astype(int)
    df['playoffs'] = (df['week'] > 18).astype(int)
    
    logger.info("Added derived features: division_game, conference_game, early_season, late_season, playoffs")
    
    return df


def main():
    """Main function to process historical data."""
    parser = argparse.ArgumentParser(description="Process historical NFL data for model training")
    parser.add_argument("input_file", help="Path to CSV file with historical data")
    parser.add_argument("--output-file", help="Output file path (optional)")
    parser.add_argument("--seasons", nargs="+", type=int, help="Filter to specific seasons")
    
    args = parser.parse_args()
    
    # Load data
    df = load_google_sheets_data(args.input_file)
    
    # Filter seasons if specified
    if args.seasons:
        df = df[df['season'].isin(args.seasons)]
        logger.info(f"Filtered to seasons {args.seasons}: {len(df)} games")
    
    # Process data
    df = standardize_historical_data(df)
    df = add_derived_features(df)
    
    # Remove rows with missing critical data
    critical_columns = ['away_team', 'home_team', 'season', 'week']
    df = df.dropna(subset=critical_columns)
    
    # Save processed data
    if args.output_file:
        output_path = Path(args.output_file)
    else:
        output_path = config.processed_data_dir / "historical_games_processed.csv"
    
    df.to_csv(output_path, index=False)
    logger.info(f"Saved processed data to {output_path}")
    
    # Print summary
    print("\n📊 DATA SUMMARY")
    print("=" * 50)
    print(f"Total games: {len(df):,}")
    print(f"Seasons: {sorted(df['season'].unique())}")
    print(f"Weeks per season: {df.groupby('season')['week'].nunique().to_dict()}")
    print(f"Teams: {len(df['away_team'].unique())}")
    
    if 'away_team_win' in df.columns:
        win_rate = df['away_team_win'].mean()
        print(f"Away team win rate: {win_rate:.1%}")
    
    print(f"\n✅ Processed data saved to: {output_path}")
    print("\n💡 Next steps:")
    print("1. Train model: eliminator train-model historical_games_processed.csv")
    print("2. Optimize picks: eliminator optimize-picks")


if __name__ == "__main__":
    main()
