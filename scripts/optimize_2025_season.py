#!/usr/bin/env python3

"""
Optimize eliminator picks for the complete 2025 NFL season.

This script:
1. Scrapes the 2025 NFL schedule from NFL Operations
2. Uses our trained team ratings model to predict all games
3. Finds optimal eliminator picks for weeks 2-18
4. Provides comprehensive season strategy analysis
"""

import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime

from sportsball.data.nfl_schedule_scraper import NFLScheduleScraper
from sportsball.models.team_ratings import SpreadBasedTeamRatings
from sportsball.optimization.eliminator import EliminatorOptimizer
from sportsball.utils.logging import setup_logging, logger
from sportsball.utils.config import config


def scrape_and_save_schedule() -> pd.DataFrame:
    """Scrape 2025 NFL schedule and save it."""
    
    print("📅 Scraping 2025 NFL Schedule...")
    
    scraper = NFLScheduleScraper()
    schedule_df = scraper.scrape_2025_schedule()
    
    if schedule_df.empty:
        raise ValueError("Failed to scrape NFL schedule")
    
    # Save the schedule
    schedule_file = config.raw_data_dir / "nfl_schedule_2025.csv"
    schedule_df.to_csv(schedule_file, index=False)
    
    print(f"✅ Scraped {len(schedule_df)} games from weeks {schedule_df['week'].min()}-{schedule_df['week'].max()}")
    print(f"💾 Schedule saved to: {schedule_file}")
    
    return schedule_df


def load_trained_model() -> SpreadBasedTeamRatings:
    """Load the trained team ratings model."""
    
    print("\n🤖 Loading trained team ratings model...")
    
    model_file = config.models_dir / "spread_ratings_model.joblib"
    if not model_file.exists():
        raise FileNotFoundError(
            f"Team ratings model not found: {model_file}\n"
            "Train it first with: uv run eliminator train-team-ratings data/raw/nfl_odds_2020_2023_sample.csv"
        )
    
    model = SpreadBasedTeamRatings()
    model.load_model(model_file)
    
    print("✅ Model loaded successfully")
    
    # Show model info
    metadata = model.model_metadata
    accuracy = metadata.get('overall_accuracy', 0)
    print(f"   Model accuracy: {accuracy:.1%}")
    print(f"   Home field advantage: {model.home_field_advantage:.1f} points")
    print(f"   Trained on {len(metadata.get('training_seasons', []))} seasons")
    
    return model


def extend_model_to_2025(model: SpreadBasedTeamRatings) -> None:
    """Extend the model with 2025 season data (Week 1 results)."""
    
    print("\n📊 Extending model with 2025 Week 1 results...")
    
    # Create sample Week 1 results for 2025
    # In reality, you'd get these from actual game results
    week1_games = create_sample_week1_results()
    
    # Load existing training data
    historical_file = "data/raw/nfl_odds_2020_2023_sample.csv"
    if Path(historical_file).exists():
        historical_df = pd.read_csv(historical_file)
        
        # Combine with Week 1 results
        all_data = pd.concat([historical_df, week1_games], ignore_index=True)
        
        # Retrain model with 2025 data
        print("   Retraining model with 2025 Week 1 results...")
        model.fit(all_data, optimize_params=False)  # Don't re-optimize params
        
        print("✅ Model updated with 2025 data")
    else:
        print("⚠️  No historical data found, using model as-is")


def create_sample_week1_results() -> pd.DataFrame:
    """Create sample Week 1 results for 2025 season."""
    
    # These would be real results in practice
    week1_results = [
        # Thursday Night Football
        {'away_team': 'DAL', 'home_team': 'PHI', 'away_score': 17, 'home_score': 28, 'spread': 3.0},
        
        # Friday International Game
        {'away_team': 'KC', 'home_team': 'LAC', 'away_score': 31, 'home_score': 17, 'spread': -2.5},
        
        # Sunday games (sample)
        {'away_team': 'TB', 'home_team': 'ATL', 'away_score': 24, 'home_score': 21, 'spread': 1.5},
        {'away_team': 'CIN', 'home_team': 'CLE', 'away_score': 27, 'home_score': 14, 'spread': -3.0},
        {'away_team': 'MIA', 'home_team': 'IND', 'away_score': 20, 'home_score': 17, 'spread': 2.5},
        {'away_team': 'CAR', 'home_team': 'JAX', 'away_score': 14, 'home_score': 28, 'spread': 6.0},
        {'away_team': 'LV', 'home_team': 'NE', 'away_score': 21, 'home_score': 24, 'spread': 4.5},
        {'away_team': 'ARI', 'home_team': 'NO', 'away_score': 17, 'home_score': 31, 'spread': 7.0},
        
        # Sunday Night and Monday Night
        {'away_team': 'BAL', 'home_team': 'BUF', 'away_score': 21, 'home_score': 35, 'spread': 2.5},
        {'away_team': 'MIN', 'home_team': 'CHI', 'away_score': 28, 'home_score': 17, 'spread': -1.0},
    ]
    
    games = []
    for game in week1_results:
        winner = 'away' if game['away_score'] > game['home_score'] else 'home'
        games.append({
            'season': 2025,
            'week': 1,
            'away_team': game['away_team'],
            'home_team': game['home_team'],
            'spread': game['spread'],
            'away_score': game['away_score'],
            'home_score': game['home_score'],
            'winner': winner
        })
    
    return pd.DataFrame(games)


def predict_season_games(model: SpreadBasedTeamRatings, schedule_df: pd.DataFrame) -> pd.DataFrame:
    """Predict win probabilities for all remaining games."""
    
    print(f"\n🎯 Predicting win probabilities for {len(schedule_df)} games...")
    
    # Filter to weeks 2-18 (remaining season)
    remaining_games = schedule_df[schedule_df['week'] >= 2].copy()
    
    if remaining_games.empty:
        raise ValueError("No remaining games found in schedule")
    
    # Predict probabilities
    predictions_df = model.predict_game_probabilities(remaining_games)
    
    print(f"✅ Generated predictions for {len(predictions_df)} games (weeks {predictions_df['week'].min()}-{predictions_df['week'].max()})")
    
    # Show sample predictions
    print(f"\nSample predictions for Week 2:")
    week2_games = predictions_df[predictions_df['week'] == 2].head(6)
    for _, game in week2_games.iterrows():
        away_prob = game['away_win_prob']
        home_prob = game['home_win_prob']
        print(f"  {game['away_team']} @ {game['home_team']}: {away_prob:.1%} / {home_prob:.1%}")
    
    return predictions_df


def optimize_full_season(predictions_df: pd.DataFrame) -> None:
    """Find optimal eliminator picks for the full season."""
    
    print(f"\n🎲 Optimizing eliminator picks for 2025 season...")
    
    # Assume we need to pick someone for Week 1 (let's say we picked BUF)
    picked_teams = {1: "BUF"}  # Buffalo won big in our sample Week 1
    
    # Get remaining weeks
    remaining_weeks = sorted(predictions_df['week'].unique())
    
    print(f"Already picked: {picked_teams}")
    print(f"Optimizing for weeks: {remaining_weeks}")
    
    # Run optimization
    optimizer = EliminatorOptimizer()
    
    try:
        result = optimizer.optimize_eliminator_picks(
            predictions_df,
            picked_teams,
            remaining_weeks,
            risk_adjustment=0.0  # Neutral risk
        )
        
        if result.success:
            display_season_strategy(result, predictions_df, picked_teams)
            analyze_strategy_risk(result, predictions_df)
            
        else:
            print(f"\n❌ Optimization failed: {result.message}")
            
    except Exception as e:
        print(f"\n❌ Optimization error: {e}")
        logger.exception("Season optimization failed")


def display_season_strategy(result, predictions_df: pd.DataFrame, picked_teams: dict) -> None:
    """Display the complete season strategy."""
    
    print(f"\n🎉 OPTIMAL 2025 ELIMINATOR STRATEGY")
    print("=" * 50)
    
    # Combine all picks
    all_picks = {**picked_teams, **result.optimal_picks}
    
    print(f"Expected survival probability: {result.expected_survival_prob:.1%}")
    print(f"Total weeks to survive: {len(all_picks)}")
    
    print(f"\nWeek-by-week strategy:")
    print("-" * 40)
    
    for week in sorted(all_picks.keys()):
        if week in picked_teams:
            team = picked_teams[week]
            print(f"Week {week:2d}: {team} (already picked)")
        else:
            pick_info = result.optimal_picks[week]
            if isinstance(pick_info, tuple):
                team, confidence = pick_info
            else:
                team = pick_info
                confidence = 0.5
            
            prob = result.weekly_probs[week]
            
            # Find opponent for context
            week_games = predictions_df[predictions_df['week'] == week]
            opponent = ""
            location = ""
            for _, game in week_games.iterrows():
                if game['away_team'] == team:
                    opponent = game['home_team']
                    location = "@"
                    break
                elif game['home_team'] == team:
                    opponent = game['away_team']
                    location = "vs"
                    break
            
            risk_indicator = "🔥" if prob < 0.60 else "⚠️" if prob < 0.70 else "✅"
            print(f"Week {week:2d}: {team} {location} {opponent} - {prob:.1%} {risk_indicator}")
    
    # Show teams used
    teams_used = set(all_picks.values())
    teams_available = set(['ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', 'DAL', 'DEN', 
                          'DET', 'GB', 'HOU', 'IND', 'JAX', 'KC', 'LAC', 'LAR', 'LV', 'MIA', 
                          'MIN', 'NE', 'NO', 'NYG', 'NYJ', 'PHI', 'PIT', 'SEA', 'SF', 'TB', 'TEN', 'WAS'])
    teams_unused = teams_available - teams_used
    
    print(f"\nTeams used ({len(teams_used)}): {', '.join(sorted(teams_used))}")
    if teams_unused:
        print(f"Teams available ({len(teams_unused)}): {', '.join(sorted(teams_unused))}")


def analyze_strategy_risk(result, predictions_df: pd.DataFrame) -> None:
    """Analyze the risk profile of the strategy."""
    
    print(f"\n📊 RISK ANALYSIS")
    print("=" * 30)
    
    weekly_probs = list(result.weekly_probs.values())
    
    print(f"Average win probability: {np.mean(weekly_probs):.1%}")
    print(f"Minimum win probability: {np.min(weekly_probs):.1%}")
    print(f"Standard deviation: {np.std(weekly_probs):.1%}")
    
    # Risk categories
    high_risk = [week for week, prob in result.weekly_probs.items() if prob < 0.60]
    medium_risk = [week for week, prob in result.weekly_probs.items() if 0.60 <= prob < 0.75]
    low_risk = [week for week, prob in result.weekly_probs.items() if prob >= 0.75]
    
    print(f"\nRisk breakdown:")
    print(f"  High risk weeks (<60%): {len(high_risk)} - {high_risk}")
    print(f"  Medium risk weeks (60-75%): {len(medium_risk)} - {medium_risk}")
    print(f"  Low risk weeks (>75%): {len(low_risk)} - {low_risk}")
    
    # Survival probability by week
    cumulative_prob = 1.0
    print(f"\nSurvival probability by week:")
    for week in sorted(result.weekly_probs.keys())[:10]:  # Show first 10 weeks
        prob = result.weekly_probs[week]
        cumulative_prob *= prob
        print(f"  Through Week {week}: {cumulative_prob:.1%}")


def main():
    """Main function to optimize 2025 eliminator season."""
    
    setup_logging()
    
    print("🏈 2025 NFL ELIMINATOR SEASON OPTIMIZER")
    print("=" * 50)
    print("Generating optimal picks for the complete 2025 season!")
    
    try:
        # Step 1: Scrape 2025 schedule
        schedule_df = scrape_and_save_schedule()
        
        # Step 2: Load trained model
        model = load_trained_model()
        
        # Step 3: Extend model with 2025 data
        extend_model_to_2025(model)
        
        # Step 4: Predict all remaining games
        predictions_df = predict_season_games(model, schedule_df)
        
        # Step 5: Optimize full season
        optimize_full_season(predictions_df)
        
        print(f"\n✅ 2025 SEASON OPTIMIZATION COMPLETE!")
        print("\n💡 Next steps:")
        print("   1. Review the week-by-week strategy")
        print("   2. Consider your risk tolerance for high-risk weeks")
        print("   3. Update picks weekly as new information becomes available")
        print("   4. Monitor team performance and injuries throughout season")
        
    except Exception as e:
        print(f"\n❌ Error: {e}")
        logger.exception("Season optimization failed")


if __name__ == "__main__":
    main()
