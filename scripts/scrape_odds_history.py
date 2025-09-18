#!/usr/bin/env python3
"""
Script to scrape historical NFL odds from Sports Odds History.

This script demonstrates how to use the SportsOddsHistoryScraper to collect
historical betting data for model training and analysis.
"""

import argparse
from pathlib import Path
import pandas as pd

from sportsball.data.odds_scraper import SportsOddsHistoryScraper
from sportsball.utils.logging import logger


def main():
    """Main function to scrape NFL odds data."""
    
    parser = argparse.ArgumentParser(description="Scrape historical NFL odds data")
    parser.add_argument("--season", type=int, help="Single season to scrape (e.g., 2024)")
    parser.add_argument("--start-year", type=int, default=2020, help="Start year for multi-season scrape")
    parser.add_argument("--end-year", type=int, default=2024, help="End year for multi-season scrape")
    parser.add_argument("--output-file", help="Output CSV filename")
    parser.add_argument("--preview", action="store_true", help="Preview first few games without saving")
    
    args = parser.parse_args()
    
    logger.info("🏈 Starting NFL Odds History Scraper")
    
    # Initialize scraper
    scraper = SportsOddsHistoryScraper()
    
    # Scrape data
    if args.season:
        logger.info(f"Scraping single season: {args.season}")
        df = scraper.scrape_season_odds(args.season)
        default_filename = f"nfl_odds_{args.season}_complete"
    else:
        logger.info(f"Scraping multiple seasons: {args.start_year}-{args.end_year}")
        df = scraper.scrape_multiple_seasons(args.start_year, args.end_year)
        default_filename = f"nfl_odds_{args.start_year}_{args.end_year}_complete"
    
    if df.empty:
        logger.error("No data scraped!")
        return
    
    # Show summary
    summary = scraper.get_odds_summary(df)
    print("\n📊 SCRAPING SUMMARY")
    print("=" * 50)
    print(f"Total games: {summary['total_games']:,}")
    print(f"Seasons: {summary['seasons']}")
    print(f"Weeks: {len(summary['weeks'])} weeks")
    print(f"Teams: {len(summary['teams'])} teams")
    print(f"Games with spread data: {summary['games_with_spread']:,}")
    print(f"Games with total data: {summary['games_with_total']:,}")
    print(f"Games with results: {summary['games_with_results']:,}")
    
    # Show sample data
    print(f"\n📋 SAMPLE DATA (first 5 games)")
    print("=" * 50)
    sample_cols = ['season', 'week', 'away_team', 'home_team', 'spread', 'total', 'away_score', 'home_score']
    available_cols = [col for col in sample_cols if col in df.columns]
    print(df[available_cols].head().to_string(index=False))
    
    if args.preview:
        logger.info("Preview mode - not saving data")
        return
    
    # Save data
    output_filename = args.output_file or default_filename
    saved_path = scraper.save_odds_data(df, output_filename)
    
    if saved_path:
        print(f"\n✅ Data saved to: {saved_path}")
        
        # Show data quality metrics
        print(f"\n📈 DATA QUALITY METRICS")
        print("=" * 50)
        
        if 'spread' in df.columns:
            spread_coverage = (df['spread'].notna().sum() / len(df)) * 100
            print(f"Spread data coverage: {spread_coverage:.1f}%")
        
        if 'total' in df.columns:
            total_coverage = (df['total'].notna().sum() / len(df)) * 100
            print(f"Total data coverage: {total_coverage:.1f}%")
        
        if 'away_score' in df.columns and 'home_score' in df.columns:
            result_coverage = (df[['away_score', 'home_score']].notna().all(axis=1).sum() / len(df)) * 100
            print(f"Game result coverage: {result_coverage:.1f}%")
        
        # Show team coverage
        if 'away_team' in df.columns:
            all_teams = set(df['away_team'].unique()) | set(df['home_team'].unique())
            teams_found = len(all_teams)
            print(f"Teams found: {teams_found}/32 NFL teams")
        
        print(f"\n💡 NEXT STEPS")
        print("=" * 50)
        print("1. Review the scraped data for quality and completeness")
        print("2. Use this data to enhance your win probability model:")
        print(f"   python scripts/setup_historical_data.py {saved_path}")
        print("3. Train model with odds data:")
        print("   uv run eliminator train-model processed_data.csv")
    
    logger.info("Scraping completed!")


if __name__ == "__main__":
    main()
