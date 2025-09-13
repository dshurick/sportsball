#!/usr/bin/env python3
"""
Simple demo of the NFL Eliminator Challenge system.

This demonstrates the core functionality without complex data scraping.
"""

import pandas as pd
import numpy as np
from pathlib import Path

from sportsball.optimization import EliminatorOptimizer
from sportsball.utils.logging import logger


def create_demo_data():
    """Create demo game data with win probabilities."""
    
    # Create sample games for weeks 3-6 of 2024 season
    games_data = [
        # Week 3
        {'season': 2024, 'week': 3, 'game_id': '2024_3_BUF_MIA', 'away_team': 'BUF', 'home_team': 'MIA', 'away_win_prob': 0.65, 'home_win_prob': 0.35},
        {'season': 2024, 'week': 3, 'game_id': '2024_3_KC_LAC', 'away_team': 'KC', 'home_team': 'LAC', 'away_win_prob': 0.72, 'home_win_prob': 0.28},
        {'season': 2024, 'week': 3, 'game_id': '2024_3_SF_DAL', 'away_team': 'SF', 'home_team': 'DAL', 'away_win_prob': 0.58, 'home_win_prob': 0.42},
        {'season': 2024, 'week': 3, 'game_id': '2024_3_GB_MIN', 'away_team': 'GB', 'home_team': 'MIN', 'away_win_prob': 0.51, 'home_win_prob': 0.49},
        
        # Week 4
        {'season': 2024, 'week': 4, 'game_id': '2024_4_BAL_CLE', 'away_team': 'BAL', 'home_team': 'CLE', 'away_win_prob': 0.68, 'home_win_prob': 0.32},
        {'season': 2024, 'week': 4, 'game_id': '2024_4_DET_SEA', 'away_team': 'DET', 'home_team': 'SEA', 'away_win_prob': 0.55, 'home_win_prob': 0.45},
        {'season': 2024, 'week': 4, 'game_id': '2024_4_PHI_TB', 'away_team': 'PHI', 'home_team': 'TB', 'away_win_prob': 0.62, 'home_win_prob': 0.38},
        {'season': 2024, 'week': 4, 'game_id': '2024_4_LAR_ARI', 'away_team': 'LAR', 'home_team': 'ARI', 'away_win_prob': 0.59, 'home_win_prob': 0.41},
        
        # Week 5
        {'season': 2024, 'week': 5, 'game_id': '2024_5_NYJ_DEN', 'away_team': 'NYJ', 'home_team': 'DEN', 'away_win_prob': 0.48, 'home_win_prob': 0.52},
        {'season': 2024, 'week': 5, 'game_id': '2024_5_ATL_CAR', 'away_team': 'ATL', 'home_team': 'CAR', 'away_win_prob': 0.63, 'home_win_prob': 0.37},
        {'season': 2024, 'week': 5, 'game_id': '2024_5_HOU_IND', 'away_team': 'HOU', 'home_team': 'IND', 'away_win_prob': 0.57, 'home_win_prob': 0.43},
        {'season': 2024, 'week': 5, 'game_id': '2024_5_NO_WAS', 'away_team': 'NO', 'home_team': 'WAS', 'away_win_prob': 0.54, 'home_win_prob': 0.46},
        
        # Week 6
        {'season': 2024, 'week': 6, 'game_id': '2024_6_CIN_PIT', 'away_team': 'CIN', 'home_team': 'PIT', 'away_win_prob': 0.52, 'home_win_prob': 0.48},
        {'season': 2024, 'week': 6, 'game_id': '2024_6_JAX_TEN', 'away_team': 'JAX', 'home_team': 'TEN', 'away_win_prob': 0.56, 'home_win_prob': 0.44},
        {'season': 2024, 'week': 6, 'game_id': '2024_6_NYG_LV', 'away_team': 'NYG', 'home_team': 'LV', 'away_win_prob': 0.45, 'home_win_prob': 0.55},
        {'season': 2024, 'week': 6, 'game_id': '2024_6_CHI_MIA', 'away_team': 'CHI', 'home_team': 'MIA', 'away_win_prob': 0.49, 'home_win_prob': 0.51},
    ]
    
    return pd.DataFrame(games_data)


def main():
    """Run the simple eliminator challenge demo."""
    
    logger.info("🏈 Starting NFL Eliminator Challenge Demo")
    
    # Configuration
    PICKED_TEAMS = {1: "BUF", 2: "KC"}  # Already picked Buffalo in week 1, KC in week 2
    CURRENT_WEEK = 3
    
    logger.info(f"Already picked teams: {PICKED_TEAMS}")
    
    # Create demo data
    logger.info("Creating demo game data with win probabilities")
    games_df = create_demo_data()
    
    logger.info(f"Created {len(games_df)} games across {games_df['week'].nunique()} weeks")
    
    # Show the games
    print("\n📅 UPCOMING GAMES")
    print("=" * 80)
    for week in sorted(games_df['week'].unique()):
        print(f"\nWeek {week}:")
        week_games = games_df[games_df['week'] == week]
        for _, game in week_games.iterrows():
            away_prob = game['away_win_prob']
            home_prob = game['home_win_prob']
            favorite = game['away_team'] if away_prob > home_prob else game['home_team']
            fav_prob = max(away_prob, home_prob)
            print(f"  {game['away_team']} @ {game['home_team']} (Favorite: {favorite} {fav_prob:.1%})")
    
    # Optimize eliminator picks
    logger.info("Optimizing eliminator challenge picks")
    optimizer = EliminatorOptimizer()
    
    # Get remaining weeks (exclude weeks where we already picked)
    remaining_weeks = [w for w in games_df['week'].unique() 
                      if w >= CURRENT_WEEK and w not in PICKED_TEAMS.keys()]
    
    logger.info(f"Optimizing for remaining weeks: {remaining_weeks}")
    
    result = optimizer.optimize_eliminator_picks(
        games_df,
        picked_teams=PICKED_TEAMS,
        remaining_weeks=remaining_weeks,
        risk_adjustment=0.1  # Slightly risk averse
    )
    
    if result.success:
        print("\n🏆 OPTIMAL ELIMINATOR PICKS")
        print("=" * 80)
        
        # Show already picked teams
        print("Already picked:")
        for week, team in PICKED_TEAMS.items():
            print(f"  Week {week}: {team}")
        
        print("\nOptimal picks for remaining weeks:")
        for week in sorted(result.optimal_picks.keys()):
            team, opponent = result.optimal_picks[week]
            prob = result.weekly_probs[week]
            confidence = abs(prob - 0.5) * 2
            print(f"  Week {week}: Pick {team} vs {opponent} ({prob:.1%} win prob, {confidence:.1%} confidence)")
        
        print(f"\n📊 Expected survival probability: {result.expected_survival_prob:.1%}")
        print(f"⚡ Solved in {result.solver_time:.3f} seconds using {result.metadata.get('solver', 'Unknown')} solver")
        
        # Run simulations
        logger.info("Running season outcome simulations")
        sim_results = optimizer.simulate_season_outcomes(
            result.optimal_picks,
            result.weekly_probs,
            n_simulations=10000
        )
        
        print(f"\n🎲 SIMULATION RESULTS (10,000 simulations)")
        print("=" * 80)
        print(f"Survival rate: {sim_results['survival_rate']:.1%}")
        print(f"Average weeks survived: {sim_results['avg_weeks_survived']:.1f} / {sim_results['total_weeks']}")
        
        print("\nWeekly survival rates:")
        for week in sorted(sim_results['week_survival_rates'].keys()):
            rate = sim_results['week_survival_rates'][week]
            print(f"  Survive to Week {week}: {rate:.1%}")
        
        print("\n✅ Demo completed successfully!")
        
        # Show strategy insights
        print(f"\n💡 STRATEGY INSIGHTS")
        print("=" * 80)
        
        # Combine all picks (already picked + optimal picks)
        all_team_picks = {**PICKED_TEAMS}
        for week, (team, _) in result.optimal_picks.items():
            all_team_picks[week] = team
            
        avg_prob = np.mean(list(result.weekly_probs.values()))
        
        print(f"Average win probability: {avg_prob:.1%}")
        print(f"Total weeks to survive: {len(all_team_picks)}")
        print(f"Teams used: {', '.join(sorted(set(all_team_picks.values())))}")
        
        # Risk analysis
        risky_picks = [(week, team, prob) for week, (team, _) in result.optimal_picks.items() 
                       if result.weekly_probs[week] < 0.60 
                       for prob in [result.weekly_probs[week]]]
        
        if risky_picks:
            print(f"\n⚠️  Risky picks (< 60% win probability):")
            for week, team, prob in risky_picks:
                print(f"  Week {week}: {team} ({prob:.1%})")
        else:
            print(f"\n✅ All picks have ≥ 60% win probability")
        
    else:
        print(f"\n❌ Optimization failed: {result.solver_status}")
        logger.error(f"Optimization failed: {result.solver_status}")
    
    logger.info("Demo completed!")


if __name__ == "__main__":
    main()
