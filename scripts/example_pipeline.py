#!/usr/bin/env python3
"""
Example pipeline for NFL Eliminator Challenge optimization.

This script demonstrates the complete workflow:
1. Scrape current NFL data
2. Load/train win probability model
3. Optimize eliminator picks
4. Analyze results
"""

import pandas as pd
from pathlib import Path
import json

from sportsball.data import NFLDataScraper, GameDataProcessor, TeamRatingsProcessor
from sportsball.models import WinProbabilityModel
from sportsball.optimization import EliminatorOptimizer
from sportsball.utils.config import config
from sportsball.utils.logging import logger


def main():
    """Run the complete eliminator challenge pipeline."""
    
    # Configuration
    SEASON = 2024
    CURRENT_WEEK = 3
    PICKED_TEAMS = {1: "BUF", 2: "KC"}  # Example: already picked Buffalo in week 1, KC in week 2
    
    logger.info("Starting NFL Eliminator Challenge pipeline")
    
    # Step 1: Scrape current data
    logger.info("Step 1: Scraping NFL data")
    scraper = NFLDataScraper()
    
    # Get team ratings
    ratings_df = scraper.scrape_team_ratings(SEASON, CURRENT_WEEK)
    if ratings_df.empty:
        logger.warning("No team ratings available, using dummy data")
        # Create dummy ratings for demonstration
        from sportsball.utils.nfl_teams import NFLTeams
        teams = NFLTeams.get_all_teams()
        ratings_df = pd.DataFrame([
            {'team': team.abbreviation, 'elo_rating': 1500, 'source': 'dummy'}
            for team in teams
        ])
    
    # Get game schedule
    schedule_df = scraper.scrape_game_schedule(SEASON)
    if schedule_df.empty:
        logger.warning("No schedule available, creating dummy schedule")
        # Create dummy schedule for demonstration
        schedule_df = pd.DataFrame([
            {'season': SEASON, 'week': 3, 'away_team': 'BUF', 'home_team': 'MIA'},
            {'season': SEASON, 'week': 3, 'away_team': 'KC', 'home_team': 'LAC'},
            {'season': SEASON, 'week': 4, 'away_team': 'SF', 'home_team': 'DAL'},
            {'season': SEASON, 'week': 4, 'away_team': 'GB', 'home_team': 'MIN'},
        ])
    
    logger.info(f"Scraped {len(ratings_df)} team ratings and {len(schedule_df)} games")
    
    # Step 2: Process data
    logger.info("Step 2: Processing game data")
    processor = GameDataProcessor()
    games_df = processor.process_upcoming_games(schedule_df, ratings_df)
    
    if games_df.empty:
        logger.error("No games available for processing")
        return
    
    logger.info(f"Processed {len(games_df)} games for prediction")
    
    # Step 3: Load or create win probability model
    logger.info("Step 3: Loading win probability model")
    model = WinProbabilityModel()
    
    try:
        model.load_model()
        logger.info("Loaded existing model")
    except FileNotFoundError:
        logger.info("No existing model found, creating new model with dummy training")
        
        # Create dummy historical data for training
        historical_data = []
        import random
        random.seed(42)
        
        for week in range(1, 18):
            for game_num in range(16):  # 16 games per week
                # Create varied outcomes for training
                away_score = random.randint(14, 35)
                home_score = random.randint(14, 35)
                
                historical_data.append({
                    'season': 2023,
                    'week': week,
                    'away_team': 'BUF',
                    'home_team': 'MIA',
                    'away_score': away_score,
                    'home_score': home_score,
                    'home_field_advantage': 1,
                    'division_game': random.randint(0, 1),
                    'conference_game': random.randint(0, 1),
                    'early_season': 1 if week <= 4 else 0,
                    'late_season': 1 if week >= 15 else 0
                })
        
        historical_df = pd.DataFrame(historical_data)
        processed_historical = processor.process_historical_games(historical_df)
        
        # Train model - use only features that will be available during prediction
        feature_cols = [col for col in processed_historical.columns 
                       if col not in ['away_team_win', 'season', 'week', 'away_team', 'home_team', 
                                     'away_score', 'home_score', 'game_date', 'game_id']]
        X = processed_historical[feature_cols]
        y = processed_historical['away_team_win']
        
        model.fit(X, y, hyperparameter_tuning=False)  # Skip tuning for demo
        model.save_model()
        logger.info("Trained and saved new model")
    
    # Step 4: Predict game probabilities
    logger.info("Step 4: Predicting game probabilities")
    predictions_df = model.predict_game_probabilities(games_df)
    
    logger.info(f"Generated predictions for {len(predictions_df)} games")
    
    # Step 5: Optimize eliminator picks
    logger.info("Step 5: Optimizing eliminator picks")
    optimizer = EliminatorOptimizer()
    
    # Get remaining weeks (exclude weeks where we already picked)
    remaining_weeks = [w for w in predictions_df['week'].unique() 
                      if w >= CURRENT_WEEK and w not in PICKED_TEAMS.keys()]
    
    result = optimizer.optimize_eliminator_picks(
        predictions_df,
        picked_teams=PICKED_TEAMS,
        remaining_weeks=remaining_weeks,
        risk_adjustment=0.1  # Slightly risk averse
    )
    
    if result.success:
        logger.info("✅ Optimization successful!")
        
        # Display results
        print("\n🏆 OPTIMAL ELIMINATOR PICKS")
        print("=" * 50)
        for week in sorted(result.optimal_picks.keys()):
            team, opponent = result.optimal_picks[week]
            prob = result.weekly_probs[week]
            print(f"Week {week}: Pick {team} vs {opponent} ({prob:.1%} win probability)")
        
        print(f"\n📊 Expected survival probability: {result.expected_survival_prob:.1%}")
        
        # Run simulations
        logger.info("Step 6: Running season simulations")
        sim_results = optimizer.simulate_season_outcomes(
            result.optimal_picks,
            result.weekly_probs,
            n_simulations=10000
        )
        
        print(f"\n🎲 SIMULATION RESULTS (10,000 simulations)")
        print("=" * 50)
        print(f"Survival rate: {sim_results['survival_rate']:.1%}")
        print(f"Average weeks survived: {sim_results['avg_weeks_survived']:.1f}")
        
        # Save results
        results_file = config.processed_data_dir / f"eliminator_picks_{SEASON}_week_{CURRENT_WEEK}.json"
        results_data = {
            'season': SEASON,
            'current_week': CURRENT_WEEK,
            'picked_teams': PICKED_TEAMS,
            'optimal_picks': {str(k): v for k, v in result.optimal_picks.items()},
            'weekly_probabilities': {str(k): v for k, v in result.weekly_probs.items()},
            'expected_survival_prob': result.expected_survival_prob,
            'simulation_results': sim_results
        }
        
        with open(results_file, 'w') as f:
            json.dump(results_data, f, indent=2)
        
        logger.info(f"Results saved to {results_file}")
        
    else:
        logger.error(f"❌ Optimization failed: {result.solver_status}")
    
    logger.info("Pipeline completed!")


if __name__ == "__main__":
    main()
