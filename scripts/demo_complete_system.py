#!/usr/bin/env python3

"""
Complete demonstration of the spread-based team ratings system.

This script shows the full workflow:
1. Train team ratings model on historical data
2. Use the model to predict game outcomes
3. Optimize eliminator picks
4. Show how ratings evolve through a season
"""

import pandas as pd
import numpy as np
from pathlib import Path

from sportsball.models.team_ratings import SpreadBasedTeamRatings
from sportsball.optimization.eliminator import EliminatorOptimizer
from sportsball.utils.logging import setup_logging, logger
from sportsball.utils.config import config

def create_2024_season_data() -> pd.DataFrame:
    """Create sample 2024 season data to extend the model."""
    
    teams = ['BUF', 'KC', 'SF', 'DAL', 'PHI', 'MIA', 'BAL', 'CIN', 
             'LAR', 'SEA', 'GB', 'MIN', 'DEN', 'LV', 'NYJ', 'NE',
             'PIT', 'CLE', 'HOU', 'IND', 'JAX', 'TEN', 'LAC', 'ARI',
             'ATL', 'CAR', 'NO', 'TB', 'CHI', 'DET', 'NYG', 'WAS']
    
    games = []
    game_id = 1
    
    # Create games for weeks 1-2 (already played)
    for week in [1, 2]:
        week_teams = teams.copy()
        np.random.shuffle(week_teams)
        
        # Create 16 games per week (32 teams / 2)
        for i in range(0, 32, 2):
            away_team = week_teams[i]
            home_team = week_teams[i + 1]
            
            # Generate realistic spread (home team favored by 0-7 points)
            spread = np.random.uniform(0.5, 7.0)
            
            # Generate scores based on spread
            home_score = np.random.randint(14, 35)
            # Away team covers spread about 45% of the time
            covers_spread = np.random.random() < 0.45
            if covers_spread:
                away_score = home_score + np.random.randint(1, 10)
            else:
                away_score = max(0, home_score - spread - np.random.randint(1, 7))
            
            winner = 'away' if away_score > home_score else 'home'
            
            games.append({
                'season': 2024,
                'week': week,
                'away_team': away_team,
                'home_team': home_team,
                'spread': spread,
                'away_score': int(away_score),
                'home_score': int(home_score),
                'winner': winner
            })
            game_id += 1
    
    return pd.DataFrame(games)

def create_upcoming_games() -> pd.DataFrame:
    """Create upcoming games for weeks 3-6."""
    
    teams = ['BUF', 'KC', 'SF', 'DAL', 'PHI', 'MIA', 'BAL', 'CIN', 
             'LAR', 'SEA', 'GB', 'MIN', 'DEN', 'LV', 'NYJ', 'NE',
             'PIT', 'CLE', 'HOU', 'IND', 'JAX', 'TEN', 'LAC', 'ARI',
             'ATL', 'CAR', 'NO', 'TB', 'CHI', 'DET', 'NYG', 'WAS']
    
    games = []
    game_id = 100
    
    for week in range(3, 7):  # Weeks 3-6
        week_teams = teams.copy()
        np.random.shuffle(week_teams)
        
        # Create 8 games per week for demonstration
        for i in range(0, 16, 2):
            away_team = week_teams[i]
            home_team = week_teams[i + 1]
            
            games.append({
                'game_id': f"2024_{week}_{game_id}",
                'season': 2024,
                'week': week,
                'away_team': away_team,
                'home_team': home_team
            })
            game_id += 1
    
    return pd.DataFrame(games)

def main():
    """Demonstrate the complete system."""
    
    setup_logging()
    
    print("🏈 COMPLETE SPREAD-BASED TEAM RATINGS DEMONSTRATION")
    print("=" * 65)
    
    # Step 1: Load historical data and add 2024 season data
    print("📊 Step 1: Preparing training data...")
    
    # Load existing historical data
    historical_file = "data/raw/nfl_odds_2020_2023_sample.csv"
    if not Path(historical_file).exists():
        print(f"❌ Historical data file not found: {historical_file}")
        print("Run: uv run python scripts/create_sample_odds_data.py first")
        return
    
    historical_df = pd.read_csv(historical_file)
    
    # Add 2024 season data (weeks 1-2)
    season_2024_df = create_2024_season_data()
    
    # Combine all training data
    all_training_data = pd.concat([historical_df, season_2024_df], ignore_index=True)
    
    print(f"✅ Training data: {len(all_training_data)} games from {all_training_data['season'].nunique()} seasons")
    print(f"   Including {len(season_2024_df)} games from 2024 season (weeks 1-2)")
    
    # Step 2: Train the model
    print(f"\n🏋️ Step 2: Training team ratings model...")
    
    model = SpreadBasedTeamRatings(
        lookback_weeks=8,
        home_field_advantage=3.0,
        rating_decay=0.1
    )
    
    # Train with all data including 2024
    model.fit(all_training_data, optimize_params=True)
    
    # Show 2024 team ratings
    print(f"\n📊 2024 Team Ratings (after 2 weeks):")
    ratings_2024 = model.get_team_ratings(2024)
    if ratings_2024:
        sorted_ratings = sorted(ratings_2024.items(), key=lambda x: x[1], reverse=True)
        
        print("Top 10 teams:")
        for i, (team, rating) in enumerate(sorted_ratings[:10]):
            print(f"  {i+1:2d}. {team}: {rating:+.2f}")
        
        print("\\nBottom 5 teams:")
        for i, (team, rating) in enumerate(sorted_ratings[-5:]):
            print(f"  {len(sorted_ratings)-4+i:2d}. {team}: {rating:+.2f}")
    
    # Step 3: Create upcoming games and predict
    print(f"\\n🎯 Step 3: Predicting upcoming games...")
    
    upcoming_games = create_upcoming_games()
    predictions_df = model.predict_game_probabilities(upcoming_games)
    
    print(f"Sample predictions for Week 3:")
    week3_games = predictions_df[predictions_df['week'] == 3].head(6)
    for _, game in week3_games.iterrows():
        away_prob = game['away_win_prob']
        home_prob = game['home_win_prob']
        print(f"  {game['away_team']} @ {game['home_team']}: {away_prob:.1%} / {home_prob:.1%}")
    
    # Step 4: Optimize eliminator picks
    print(f"\\n🎲 Step 4: Optimizing eliminator picks...")
    
    picked_teams = {1: "BUF", 2: "KC"}  # Already picked in weeks 1-2
    remaining_weeks = [3, 4, 5, 6]
    
    print(f"Already picked: {picked_teams}")
    print(f"Optimizing for weeks: {remaining_weeks}")
    
    optimizer = EliminatorOptimizer()
    
    try:
        result = optimizer.optimize_eliminator_picks(
            predictions_df,
            picked_teams,
            remaining_weeks,
            risk_adjustment=0.0
        )
        
        if result.success:
            print(f"\\n🎉 OPTIMIZATION SUCCESSFUL!")
            print("=" * 40)
            
            print("Optimal eliminator picks:")
            for week in sorted(result.optimal_picks.keys()):
                pick_info = result.optimal_picks[week]
                if isinstance(pick_info, tuple):
                    team, confidence = pick_info
                else:
                    team = pick_info
                    confidence = 0.5
                
                prob = result.weekly_probs[week]
                
                # Find the actual game for context
                week_games = predictions_df[predictions_df['week'] == week]
                game_context = ""
                for _, game in week_games.iterrows():
                    if game['away_team'] == team or game['home_team'] == team:
                        opponent = game['home_team'] if game['away_team'] == team else game['away_team']
                        location = "vs" if game['home_team'] == team else "@"
                        game_context = f" ({location} {opponent})"
                        break
                
                print(f"  Week {week}: {team}{game_context} - {prob:.1%} win probability")
            
            print(f"\\nExpected survival probability: {result.expected_survival_prob:.1%}")
            
            # Step 5: Show the power of dynamic ratings
            print(f"\\n📈 Step 5: Dynamic Rating System Benefits")
            print("=" * 45)
            print("✅ Ratings automatically update as games are played")
            print("✅ Early season blends previous year performance")
            print("✅ Model becomes more accurate as season progresses")
            print("✅ Accounts for betting market information (spreads)")
            print("✅ Optimized parameters for maximum predictive accuracy")
            
            # Show model performance
            metadata = model.model_metadata
            overall_acc = metadata.get('overall_accuracy', 0)
            print(f"\\n📊 Model Performance:")
            print(f"   Overall Accuracy: {overall_acc:.1%}")
            print(f"   Home Field Advantage: {model.home_field_advantage:.1f} points")
            print(f"   Rating Scale: {model.rating_scale:.2f}")
            
        else:
            print(f"\\n❌ Optimization failed: {result.message}")
            
    except Exception as e:
        print(f"\\n❌ Optimization error: {e}")
        logger.exception("Optimization failed")
    
    print(f"\\n✅ DEMONSTRATION COMPLETE!")
    print("\\n💡 Next Steps:")
    print("   1. Use real historical spread data for better accuracy")
    print("   2. Scrape current season spreads as games are played")
    print("   3. Run weekly to update picks as new information becomes available")
    print("   4. Adjust risk parameters based on your risk tolerance")

if __name__ == "__main__":
    main()
