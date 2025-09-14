#!/usr/bin/env python3

"""
Test the team ratings model with the eliminator optimizer.

This script creates mock upcoming games and tests the complete pipeline.
"""

import pandas as pd
import numpy as np
from sportsball.models.team_ratings import SpreadBasedTeamRatings
from sportsball.optimization.eliminator import EliminatorOptimizer
from sportsball.utils.logging import setup_logging, logger
from sportsball.utils.config import config

def create_mock_upcoming_games() -> pd.DataFrame:
    """Create mock upcoming games for testing."""
    
    # Mock games for weeks 3-6 of 2024 season
    games = []
    
    teams = ['BUF', 'KC', 'SF', 'DAL', 'PHI', 'MIA', 'BAL', 'CIN', 
             'LAR', 'SEA', 'GB', 'MIN', 'DEN', 'LV', 'NYJ', 'NE']
    
    game_id = 1
    for week in range(3, 7):  # Weeks 3-6
        # Create 4 games per week
        week_teams = teams.copy()
        np.random.shuffle(week_teams)
        
        for i in range(0, 8, 2):  # 4 games (8 teams)
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
    """Test team ratings with eliminator optimization."""
    
    setup_logging()
    
    print("🏈 TESTING TEAM RATINGS WITH ELIMINATOR OPTIMIZER")
    print("=" * 60)
    
    # Load trained team ratings model
    try:
        model = SpreadBasedTeamRatings()
        model.load_model(config.models_dir / "spread_ratings_model.joblib")
        print("✅ Loaded team ratings model")
    except FileNotFoundError:
        print("❌ Team ratings model not found. Train it first with:")
        print("   uv run eliminator train-team-ratings data/raw/nfl_odds_2020_2023_sample.csv")
        return
    
    # Create mock upcoming games
    upcoming_games = create_mock_upcoming_games()
    print(f"📅 Created {len(upcoming_games)} mock upcoming games")
    
    # Predict game probabilities
    print("\n🎯 Predicting game probabilities...")
    predictions_df = model.predict_game_probabilities(upcoming_games)
    
    print("Sample predictions:")
    for _, game in predictions_df.head(8).iterrows():
        away_prob = game['away_win_prob']
        home_prob = game['home_win_prob']
        print(f"  Week {game['week']}: {game['away_team']} @ {game['home_team']} "
              f"({away_prob:.1%} / {home_prob:.1%})")
    
    # Set up eliminator optimization
    print(f"\n🎲 Setting up eliminator optimization...")
    
    picked_teams = {1: "BUF", 2: "KC"}  # Already picked teams
    remaining_weeks = [3, 4, 5, 6]
    
    print(f"Already picked: {picked_teams}")
    print(f"Optimizing for weeks: {remaining_weeks}")
    
    # Run optimization
    optimizer = EliminatorOptimizer()
    
    try:
        result = optimizer.optimize_eliminator_picks(
            predictions_df,
            picked_teams,
            remaining_weeks,
            risk_adjustment=0.0
        )
        
        if result.success:
            print("\n🎉 OPTIMIZATION SUCCESSFUL!")
            print("=" * 40)
            
            print("Optimal picks:")
            for week, pick_info in result.optimal_picks.items():
                if isinstance(pick_info, tuple):
                    team, confidence = pick_info
                    # Handle case where confidence might be a string
                    if isinstance(confidence, str):
                        confidence = 0.5  # Default confidence
                    else:
                        confidence = float(confidence)
                else:
                    team = pick_info
                    confidence = 0.5  # Default confidence
                
                prob = result.weekly_probs[week]
                print(f"  Week {week}: {team} ({prob:.1%} win probability, {confidence:.1%} confidence)")
            
            print(f"\nExpected survival probability: {result.expected_survival_prob:.1%}")
            
            # Show team ratings used
            print(f"\n📊 Team Ratings (2024 season):")
            ratings_2024 = model.get_team_ratings(2024)
            if ratings_2024:
                sorted_ratings = sorted(ratings_2024.items(), key=lambda x: x[1], reverse=True)
                print("Top 8 teams:")
                for i, (team, rating) in enumerate(sorted_ratings[:8]):
                    print(f"  {i+1}. {team}: {rating:+.2f}")
            
        else:
            print(f"\n❌ Optimization failed: {result.message}")
            
    except Exception as e:
        print(f"\n❌ Optimization error: {e}")
        logger.exception("Optimization failed")
    
    print(f"\n✅ Test complete!")

if __name__ == "__main__":
    main()
