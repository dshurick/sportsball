#!/usr/bin/env python3
"""
Create sample NFL odds data for testing and demonstration.

Since the Sports Odds History website structure is complex, this script creates
realistic sample data that can be used to test and demonstrate the system.
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import random
from pathlib import Path

from sportsball.utils.nfl_teams import NFLTeams
from sportsball.utils.config import config
from sportsball.utils.logging import logger


def create_sample_odds_data(season: int = 2024, weeks: int = 18) -> pd.DataFrame:
    """
    Create realistic sample NFL odds data.
    
    Args:
        season: NFL season year
        weeks: Number of weeks to generate
        
    Returns:
        DataFrame with sample odds data
    """
    logger.info(f"Creating sample NFL odds data for {season} season, {weeks} weeks")
    
    teams = NFLTeams.get_all_teams()
    team_abbrevs = [team.abbreviation for team in teams]
    
    # Create a realistic schedule (16 games per week for most weeks)
    games_data = []
    
    random.seed(42)  # For reproducible results
    np.random.seed(42)
    
    for week in range(1, weeks + 1):
        # Determine number of games for this week
        if week <= 17:
            n_games = 16 if week not in [4, 5, 6, 9, 14] else 15  # Some weeks have byes
        else:
            n_games = 16  # Week 18
        
        # Create games for this week
        week_teams = team_abbrevs.copy()
        random.shuffle(week_teams)
        
        for game_num in range(n_games):
            if len(week_teams) < 2:
                break
                
            away_team = week_teams.pop()
            home_team = week_teams.pop()
            
            # Generate realistic odds
            # Home field advantage: typically 2.5-3 points
            home_advantage = np.random.normal(2.8, 0.8)
            
            # Team strength differential (simulate team ratings)
            team_diff = np.random.normal(0, 4)  # Most games are close
            
            # Calculate spread (positive means home team favored)
            raw_spread = home_advantage + team_diff
            spread = round(raw_spread * 2) / 2  # Round to nearest 0.5
            
            # Determine favorite
            if spread > 0:
                spread_favorite = 'home'
                spread_line = -abs(spread)  # Home team gets negative line
            elif spread < 0:
                spread_favorite = 'away'
                spread_line = abs(spread)   # Away team gets positive line
            else:
                spread_favorite = 'pick'
                spread_line = 0
            
            # Generate total (over/under)
            base_total = np.random.normal(44.5, 6)  # Average NFL total around 44-45
            total = round(base_total * 2) / 2  # Round to nearest 0.5
            total = max(35, min(65, total))  # Reasonable bounds
            
            # Generate game result
            # Convert spread to win probability
            home_win_prob = 1 / (1 + np.exp(0.25 * spread_line))
            home_wins = np.random.random() < home_win_prob
            
            if home_wins:
                winner = 'home'
                # Generate realistic scores
                home_score = np.random.randint(17, 35)
                away_score = home_score - np.random.randint(1, 14)
                away_score = max(0, away_score)
            else:
                winner = 'away'
                away_score = np.random.randint(17, 35)
                home_score = away_score - np.random.randint(1, 14)
                home_score = max(0, home_score)
            
            # Create game record
            game_data = {
                'season': season,
                'week': week,
                'away_team': away_team,
                'home_team': home_team,
                'spread': abs(spread) if spread != 0 else 0,
                'spread_favorite': spread_favorite,
                'spread_line': spread_line,
                'total': total,
                'away_score': away_score,
                'home_score': home_score,
                'winner': winner,
                'game_date': f"{season}-09-{week:02d}",  # Simplified date
                'source': 'sample_data'
            }
            
            games_data.append(game_data)
    
    df = pd.DataFrame(games_data)
    logger.info(f"Created {len(df)} sample games")
    
    return df


def create_historical_odds_data(start_year: int = 2020, end_year: int = 2023) -> pd.DataFrame:
    """
    Create sample historical odds data for multiple seasons.
    
    Args:
        start_year: Starting season
        end_year: Ending season (inclusive)
        
    Returns:
        Combined DataFrame with historical data
    """
    logger.info(f"Creating historical odds data for {start_year}-{end_year}")
    
    all_data = []
    
    for year in range(start_year, end_year + 1):
        season_data = create_sample_odds_data(year, weeks=17)  # Regular season only
        all_data.append(season_data)
    
    combined_df = pd.concat(all_data, ignore_index=True)
    logger.info(f"Created {len(combined_df)} historical games across {end_year - start_year + 1} seasons")
    
    return combined_df


def main():
    """Create and save sample odds data."""
    
    print("🏈 Creating Sample NFL Odds Data")
    print("=" * 50)
    
    # Create current season data
    current_season_df = create_sample_odds_data(2024, weeks=18)
    
    # Create historical data
    historical_df = create_historical_odds_data(2020, 2023)
    
    # Save data
    current_file = config.raw_data_dir / "nfl_odds_2024_sample.csv"
    historical_file = config.raw_data_dir / "nfl_odds_2020_2023_sample.csv"
    
    current_season_df.to_csv(current_file, index=False)
    historical_df.to_csv(historical_file, index=False)
    
    print(f"✅ Current season data saved: {current_file}")
    print(f"   - {len(current_season_df)} games for 2024 season")
    
    print(f"✅ Historical data saved: {historical_file}")
    print(f"   - {len(historical_df)} games from 2020-2023")
    
    # Show sample data
    print(f"\n📊 SAMPLE DATA PREVIEW")
    print("-" * 30)
    sample_cols = ['season', 'week', 'away_team', 'home_team', 'spread', 'total', 'away_score', 'home_score', 'winner']
    print(current_season_df[sample_cols].head(10).to_string(index=False))
    
    print(f"\n💡 NEXT STEPS")
    print("-" * 30)
    print("1. Process the sample data:")
    print(f"   python scripts/setup_historical_data.py {historical_file}")
    print("2. Train model with odds data:")
    print("   uv run eliminator train-model processed_data.csv")
    print("3. Test eliminator optimization:")
    print("   uv run eliminator optimize-picks --season 2024")
    
    print(f"\n📝 NOTE")
    print("-" * 30)
    print("This is sample data for demonstration. For real analysis:")
    print("- Use actual historical odds from your Google Sheets")
    print("- Implement proper scraping for current season data")
    print("- Consider integrating with sports data APIs")


if __name__ == "__main__":
    main()
