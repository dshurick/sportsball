#!/usr/bin/env python3

"""
Analyze the 2025 NFL schedule and generate strategic eliminator picks.

This script provides a more flexible approach that:
1. Analyzes the complete 2025 schedule
2. Shows week-by-week best options
3. Provides strategic recommendations
4. Handles infeasible optimization scenarios
"""

import pandas as pd
import numpy as np
from pathlib import Path
from collections import defaultdict

from sportsball.data.nfl_schedule_scraper import NFLScheduleScraper
from sportsball.models.team_ratings import SpreadBasedTeamRatings
from sportsball.optimization.eliminator import EliminatorOptimizer
from sportsball.utils.logging import setup_logging, logger
from sportsball.utils.config import config


def analyze_schedule_and_predict() -> pd.DataFrame:
    """Load schedule and generate predictions."""
    
    print("🏈 2025 NFL ELIMINATOR SCHEDULE ANALYSIS")
    print("=" * 50)
    
    # Load or scrape schedule
    schedule_file = config.raw_data_dir / "nfl_schedule_2025.csv"
    
    if schedule_file.exists():
        print("📅 Loading saved 2025 schedule...")
        schedule_df = pd.read_csv(schedule_file)
    else:
        print("📅 Scraping 2025 NFL schedule...")
        scraper = NFLScheduleScraper()
        schedule_df = scraper.scrape_2025_schedule()
        schedule_df.to_csv(schedule_file, index=False)
    
    print(f"✅ Schedule: {len(schedule_df)} games, weeks {schedule_df['week'].min()}-{schedule_df['week'].max()}")
    
    # Load model
    print("\n🤖 Loading team ratings model...")
    model_file = config.models_dir / "spread_ratings_model.joblib"
    
    if not model_file.exists():
        print("❌ Model not found. Train it first with:")
        print("   uv run eliminator train-team-ratings data/raw/nfl_odds_2020_2023_sample.csv")
        return pd.DataFrame()
    
    model = SpreadBasedTeamRatings()
    model.load_model(model_file)
    print("✅ Model loaded successfully")
    
    # Generate predictions for weeks 2+
    remaining_games = schedule_df[schedule_df['week'] >= 2].copy()
    predictions_df = model.predict_game_probabilities(remaining_games)
    
    print(f"✅ Generated predictions for {len(predictions_df)} games")
    
    return predictions_df


def analyze_weekly_options(predictions_df: pd.DataFrame) -> None:
    """Analyze best options for each week."""
    
    print(f"\n📊 WEEKLY ANALYSIS - BEST ELIMINATOR OPTIONS")
    print("=" * 55)
    
    # Assume Buffalo was picked in Week 1
    used_teams = {'BUF'}
    
    weekly_recommendations = {}
    
    for week in sorted(predictions_df['week'].unique()):
        week_games = predictions_df[predictions_df['week'] == week]
        
        print(f"\n🗓️  WEEK {week}")
        print("-" * 20)
        
        # Find best available options
        options = []
        
        for _, game in week_games.iterrows():
            # Away team option
            if game['away_team'] not in used_teams:
                options.append({
                    'team': game['away_team'],
                    'opponent': game['home_team'],
                    'location': '@',
                    'win_prob': game['away_win_prob'],
                    'game': f"{game['away_team']} @ {game['home_team']}"
                })
            
            # Home team option
            if game['home_team'] not in used_teams:
                options.append({
                    'team': game['home_team'],
                    'opponent': game['away_team'],
                    'location': 'vs',
                    'win_prob': game['home_win_prob'],
                    'game': f"{game['home_team']} vs {game['away_team']}"
                })
        
        # Sort by win probability
        options.sort(key=lambda x: x['win_prob'], reverse=True)
        
        # Show top 5 options
        print("Top options:")
        for i, option in enumerate(options[:5]):
            prob = option['win_prob']
            risk_indicator = "🔥" if prob < 0.60 else "⚠️" if prob < 0.70 else "✅"
            print(f"  {i+1}. {option['team']} {option['location']} {option['opponent']}: {prob:.1%} {risk_indicator}")
        
        # Store recommendation (best available option)
        if options:
            best_option = options[0]
            weekly_recommendations[week] = best_option
            print(f"\n💡 Recommendation: {best_option['team']} ({best_option['win_prob']:.1%})")
            
            # Add to used teams for next week analysis
            used_teams.add(best_option['team'])
        else:
            print("\n❌ No available teams this week!")
    
    return weekly_recommendations


def generate_season_strategy(predictions_df: pd.DataFrame) -> None:
    """Generate a complete season strategy."""
    
    print(f"\n🎯 COMPLETE SEASON STRATEGY")
    print("=" * 40)
    
    # Try different approaches for optimization
    approaches = [
        {"name": "Conservative (High Win %)", "risk_adjustment": 0.2},
        {"name": "Balanced", "risk_adjustment": 0.0},
        {"name": "Aggressive (Maximize Survival)", "risk_adjustment": -0.1}
    ]
    
    picked_teams = {1: "BUF"}  # Week 1 pick
    
    for approach in approaches:
        print(f"\n📋 {approach['name']} Strategy:")
        print("-" * 30)
        
        try:
            remaining_weeks = sorted(predictions_df['week'].unique())
            
            optimizer = EliminatorOptimizer()
            result = optimizer.optimize_eliminator_picks(
                predictions_df,
                picked_teams,
                remaining_weeks,
                risk_adjustment=approach['risk_adjustment']
            )
            
            if result.success:
                print(f"✅ Expected survival: {result.expected_survival_prob:.1%}")
                
                # Show first 8 weeks of strategy
                all_picks = {**picked_teams, **result.optimal_picks}
                for week in sorted(list(all_picks.keys())[:8]):
                    if week in picked_teams:
                        team = picked_teams[week]
                        print(f"  Week {week}: {team} (already picked)")
                    else:
                        pick_info = result.optimal_picks[week]
                        if isinstance(pick_info, tuple):
                            team = pick_info[0]
                        else:
                            team = pick_info
                        
                        prob = result.weekly_probs.get(week, 0.5)
                        risk = "🔥" if prob < 0.60 else "⚠️" if prob < 0.70 else "✅"
                        print(f"  Week {week}: {team} ({prob:.1%}) {risk}")
                
                if len(all_picks) > 8:
                    print(f"  ... and {len(all_picks) - 8} more weeks")
                    
            else:
                print(f"❌ Optimization failed - trying greedy approach")
                greedy_strategy = generate_greedy_strategy(predictions_df, picked_teams)
                display_greedy_strategy(greedy_strategy, picked_teams)
                
        except Exception as e:
            print(f"❌ Error with {approach['name']}: {e}")


def generate_greedy_strategy(predictions_df: pd.DataFrame, picked_teams: dict) -> dict:
    """Generate a greedy week-by-week strategy."""
    
    used_teams = set(picked_teams.values())
    strategy = {}
    
    for week in sorted(predictions_df['week'].unique()):
        week_games = predictions_df[predictions_df['week'] == week]
        
        best_option = None
        best_prob = 0
        
        for _, game in week_games.iterrows():
            # Check away team
            if game['away_team'] not in used_teams:
                if game['away_win_prob'] > best_prob:
                    best_prob = game['away_win_prob']
                    best_option = {
                        'team': game['away_team'],
                        'opponent': game['home_team'],
                        'prob': game['away_win_prob'],
                        'location': '@'
                    }
            
            # Check home team
            if game['home_team'] not in used_teams:
                if game['home_win_prob'] > best_prob:
                    best_prob = game['home_win_prob']
                    best_option = {
                        'team': game['home_team'],
                        'opponent': game['away_team'],
                        'prob': game['home_win_prob'],
                        'location': 'vs'
                    }
        
        if best_option:
            strategy[week] = best_option
            used_teams.add(best_option['team'])
    
    return strategy


def display_greedy_strategy(strategy: dict, picked_teams: dict) -> None:
    """Display the greedy strategy results."""
    
    all_picks = {**picked_teams}
    survival_prob = 1.0
    
    print("Greedy week-by-week picks:")
    
    for week in sorted(strategy.keys())[:10]:  # Show first 10 weeks
        option = strategy[week]
        all_picks[week] = option['team']
        survival_prob *= option['prob']
        
        risk = "🔥" if option['prob'] < 0.60 else "⚠️" if option['prob'] < 0.70 else "✅"
        print(f"  Week {week}: {option['team']} {option['location']} {option['opponent']} ({option['prob']:.1%}) {risk}")
    
    print(f"\nGreedy survival probability (10 weeks): {survival_prob:.1%}")
    print(f"Teams used: {', '.join(sorted(set(all_picks.values())))}")


def show_key_insights(predictions_df: pd.DataFrame) -> None:
    """Show key strategic insights."""
    
    print(f"\n💡 KEY STRATEGIC INSIGHTS")
    print("=" * 35)
    
    # Find weeks with limited good options
    weekly_good_options = {}
    
    for week in sorted(predictions_df['week'].unique()):
        week_games = predictions_df[predictions_df['week'] == week]
        
        good_options = 0
        for _, game in week_games.iterrows():
            if game['away_win_prob'] >= 0.70:
                good_options += 1
            if game['home_win_prob'] >= 0.70:
                good_options += 1
        
        weekly_good_options[week] = good_options
    
    # Find challenging weeks
    challenging_weeks = [week for week, count in weekly_good_options.items() if count <= 2]
    
    print(f"🔥 Challenging weeks (≤2 teams >70%): {challenging_weeks}")
    
    # Find best weeks to use premium teams
    premium_teams = ['KC', 'BUF', 'SF', 'DAL', 'PHI']  # Top teams
    
    print(f"\n🏆 Premium team opportunities:")
    for team in premium_teams:
        if team == 'BUF':
            continue  # Already used
            
        best_week = None
        best_prob = 0
        
        for week in sorted(predictions_df['week'].unique()):
            week_games = predictions_df[predictions_df['week'] == week]
            
            for _, game in week_games.iterrows():
                if game['away_team'] == team and game['away_win_prob'] > best_prob:
                    best_prob = game['away_win_prob']
                    best_week = week
                elif game['home_team'] == team and game['home_win_prob'] > best_prob:
                    best_prob = game['home_win_prob']
                    best_week = week
        
        if best_week:
            print(f"  {team}: Week {best_week} ({best_prob:.1%})")
    
    # Overall statistics
    all_probs = []
    for _, game in predictions_df.iterrows():
        all_probs.extend([game['away_win_prob'], game['home_win_prob']])
    
    print(f"\n📊 Overall Statistics:")
    print(f"  Average win probability: {np.mean(all_probs):.1%}")
    print(f"  Games >70% confidence: {sum(1 for p in all_probs if p > 0.70)}")
    print(f"  Games >80% confidence: {sum(1 for p in all_probs if p > 0.80)}")


def main():
    """Main analysis function."""
    
    setup_logging()
    
    try:
        # Analyze schedule and generate predictions
        predictions_df = analyze_schedule_and_predict()
        
        if predictions_df.empty:
            return
        
        # Weekly analysis
        weekly_recommendations = analyze_weekly_options(predictions_df)
        
        # Season strategy
        generate_season_strategy(predictions_df)
        
        # Key insights
        show_key_insights(predictions_df)
        
        print(f"\n✅ ANALYSIS COMPLETE!")
        print("\n📝 Summary:")
        print("   • 2025 schedule successfully analyzed")
        print("   • Win probabilities generated for all games")
        print("   • Multiple strategic approaches provided")
        print("   • Key insights identified for decision making")
        
    except Exception as e:
        print(f"❌ Analysis failed: {e}")
        logger.exception("Analysis error")


if __name__ == "__main__":
    main()
