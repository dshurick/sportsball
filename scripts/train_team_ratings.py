#!/usr/bin/env python3

"""
Train and evaluate the spread-based team ratings model.

This script demonstrates the new dynamic team rating system that:
1. Uses betting spreads to calculate team ratings
2. Incorporates previous season ratings for early season predictions  
3. Optimizes parameters for maximum accuracy
4. Evaluates performance across different weeks of the season
"""

import pandas as pd
import numpy as np
from pathlib import Path

from sportsball.models.team_ratings import SpreadBasedTeamRatings
from sportsball.utils.logging import setup_logging, logger
from sportsball.utils.config import config

def load_historical_data() -> pd.DataFrame:
    """Load historical spread data."""
    
    # Try to load the sample data we created
    data_files = [
        "data/raw/nfl_odds_2020_2023_sample.csv",
        "historical_spreads_2020_2024.csv",
        "data/raw/nfl_odds_2024_sample.csv"
    ]
    
    dfs = []
    for file_path in data_files:
        if Path(file_path).exists():
            logger.info(f"Loading data from {file_path}")
            df = pd.read_csv(file_path)
            dfs.append(df)
    
    if not dfs:
        raise FileNotFoundError("No historical data files found. Run create_sample_odds_data.py first.")
    
    # Combine all data
    combined_df = pd.concat(dfs, ignore_index=True)
    
    # Ensure required columns exist
    required_cols = ['season', 'week', 'away_team', 'home_team', 'spread', 'away_score', 'home_score']
    missing_cols = [col for col in required_cols if col not in combined_df.columns]
    
    if missing_cols:
        raise ValueError(f"Missing required columns: {missing_cols}")
    
    # Add winner column if not present
    if 'winner' not in combined_df.columns:
        combined_df['winner'] = combined_df.apply(
            lambda row: 'away' if row['away_score'] > row['home_score'] else 'home', 
            axis=1
        )
    
    # Filter to regular season games only (weeks 1-17/18)
    combined_df = combined_df[combined_df['week'] <= 17]
    
    logger.info(f"Loaded {len(combined_df)} games from {combined_df['season'].nunique()} seasons")
    return combined_df

def analyze_model_performance(model: SpreadBasedTeamRatings) -> None:
    """Analyze and visualize model performance."""
    
    metadata = model.model_metadata
    
    print("\n🏆 MODEL PERFORMANCE ANALYSIS")
    print("=" * 50)
    
    # Overall performance
    overall_acc = metadata.get('overall_accuracy', 0)
    print(f"Overall Accuracy: {overall_acc:.1%}")
    
    # Performance by week
    acc_by_week = metadata.get('accuracy_by_week', {})
    logloss_by_week = metadata.get('log_loss_by_week', {})
    
    if acc_by_week:
        print(f"\n📊 Accuracy by Week:")
        for week in sorted(acc_by_week.keys()):
            acc = acc_by_week[week]
            loss = logloss_by_week.get(week, 0)
            print(f"  Week {week:2d}: {acc:.1%} accuracy, {loss:.3f} log loss")
    
    # Model parameters
    print(f"\n⚙️  Optimized Parameters:")
    print(f"  Home Field Advantage: {model.home_field_advantage:.2f} points")
    print(f"  Rating Scale Factor: {model.rating_scale:.2f}")
    print(f"  Previous Season Decay: {model.rating_decay:.1%}")
    
    # Team ratings for latest season
    if model.team_ratings:
        latest_season = max(model.team_ratings.keys())
        ratings = model.team_ratings[latest_season]
        
        print(f"\n🏈 Team Ratings for {latest_season} Season:")
        sorted_teams = sorted(ratings.items(), key=lambda x: x[1], reverse=True)
        
        print("  Top 5 Teams:")
        for i, (team, rating) in enumerate(sorted_teams[:5]):
            print(f"    {i+1}. {team}: {rating:+.2f}")
        
        print("  Bottom 5 Teams:")
        for i, (team, rating) in enumerate(sorted_teams[-5:]):
            print(f"    {len(sorted_teams)-4+i}. {team}: {rating:+.2f}")

def test_predictions(model: SpreadBasedTeamRatings, test_games: pd.DataFrame) -> None:
    """Test model predictions on sample games."""
    
    print("\n🎯 SAMPLE PREDICTIONS")
    print("=" * 50)
    
    # Get a few sample games for prediction
    sample_games = test_games.head(10)
    
    for _, game in sample_games.iterrows():
        prob_away = model.predict_game_probability(
            game['away_team'], game['home_team'], 
            game['season'], game['week']
        )
        
        actual_winner = game['winner']
        predicted_winner = 'away' if prob_away > 0.5 else 'home'
        correct = "✅" if predicted_winner == actual_winner else "❌"
        
        print(f"{correct} Week {game['week']}: {game['away_team']} @ {game['home_team']}")
        print(f"    Predicted: {game['away_team']} {prob_away:.1%} | {game['home_team']} {1-prob_away:.1%}")
        print(f"    Actual: {actual_winner} team won ({game['away_score']}-{game['home_score']})")
        print()

def main():
    """Main training and evaluation pipeline."""
    
    setup_logging()
    
    print("🏈 SPREAD-BASED TEAM RATINGS MODEL")
    print("=" * 50)
    
    # Load historical data
    logger.info("Loading historical spread data")
    games_df = load_historical_data()
    
    # Split into train/test
    # Use 2020-2022 for training, 2023+ for testing
    train_df = games_df[games_df['season'] <= 2022]
    test_df = games_df[games_df['season'] >= 2023]
    
    logger.info(f"Training on {len(train_df)} games from {train_df['season'].nunique()} seasons")
    logger.info(f"Testing on {len(test_df)} games from {test_df['season'].nunique()} seasons")
    
    # Initialize and train model
    logger.info("Initializing team ratings model")
    model = SpreadBasedTeamRatings(
        lookback_weeks=8,
        home_field_advantage=3.0,
        rating_decay=0.1
    )
    
    # Train the model
    logger.info("Training model (this may take a few minutes)")
    model.fit(train_df, optimize_params=True)
    
    # Save the trained model
    model.save_model()
    logger.info("Model saved successfully")
    
    # Analyze performance
    analyze_model_performance(model)
    
    # Test predictions
    if len(test_df) > 0:
        test_predictions(model, test_df)
    
    # Show how ratings evolve through a season
    print("\n📈 RATING EVOLUTION EXAMPLE")
    print("=" * 50)
    print("This model dynamically updates team ratings as more games are played.")
    print("Early season predictions blend previous year ratings with current performance.")
    print("As more data becomes available, ratings become more accurate.")
    
    print(f"\n✅ Training complete! Model ready for eliminator optimization.")
    print(f"Use: uv run eliminator optimize-picks --model-name team_ratings_model")

if __name__ == "__main__":
    main()
