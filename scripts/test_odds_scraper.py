 #!/usr/bin/env python3
"""
Test script to debug the odds scraper and understand the website structure.
"""

import requests
from bs4 import BeautifulSoup
import pandas as pd
from sportsball.data.odds_scraper import SportsOddsHistoryScraper

def analyze_website_structure():
    """Analyze the structure of the Sports Odds History website."""
    
    url = "https://www.sportsoddshistory.com/nfl-game-season/?y=2024"
    
    session = requests.Session()
    session.headers.update({
        'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36'
    })
    
    response = session.get(url)
    soup = BeautifulSoup(response.content, 'html.parser')
    
    print("🔍 ANALYZING WEBSITE STRUCTURE")
    print("=" * 50)
    
    # Look for the main results table
    tables = soup.find_all('table')
    print(f"Found {len(tables)} tables")
    
    # Examine the first few tables
    for i, table in enumerate(tables[:5]):
        print(f"\n📊 TABLE {i+1}")
        print("-" * 30)
        
        rows = table.find_all('tr')
        print(f"Rows: {len(rows)}")
        
        if rows:
            # Look at first few rows
            for j, row in enumerate(rows[:3]):
                cells = row.find_all(['td', 'th'])
                cell_texts = [cell.get_text(strip=True) for cell in cells]
                print(f"  Row {j+1} ({len(cells)} cells): {cell_texts[:8]}")
    
    # Look for the weekly results table specifically
    print(f"\n🎯 LOOKING FOR WEEKLY RESULTS")
    print("-" * 30)
    
    # The website shows a summary table with weeks - let's find that
    week_table = None
    for table in tables:
        rows = table.find_all('tr')
        if rows:
            first_row_text = ' '.join([cell.get_text(strip=True) for cell in rows[0].find_all(['td', 'th'])])
            if 'week' in first_row_text.lower() and 'favorites' in first_row_text.lower():
                week_table = table
                break
    
    if week_table:
        print("Found weekly results table!")
        rows = week_table.find_all('tr')
        
        for i, row in enumerate(rows[:10]):  # Show first 10 rows
            cells = row.find_all(['td', 'th'])
            cell_texts = [cell.get_text(strip=True) for cell in cells]
            print(f"  Week Row {i}: {cell_texts}")
    
    # Look for individual game data
    print(f"\n🏈 LOOKING FOR INDIVIDUAL GAMES")
    print("-" * 30)
    
    # Try to find links to individual weeks
    week_links = soup.find_all('a', href=lambda x: x and '#' in x and x.split('#')[-1].isdigit())
    print(f"Found {len(week_links)} week links")
    
    for link in week_links[:5]:
        href = link.get('href', '')
        text = link.get_text(strip=True)
        print(f"  Link: {text} -> {href}")


def test_scraper_on_sample():
    """Test the scraper on a small sample."""
    
    print("\n🧪 TESTING SCRAPER")
    print("=" * 50)
    
    scraper = SportsOddsHistoryScraper()
    
    # Try to scrape just a few weeks
    df = scraper.scrape_season_odds(2024)
    
    if not df.empty:
        print(f"Scraped {len(df)} total records")
        
        # Show unique teams
        if 'away_team' in df.columns:
            unique_teams = set(df['away_team'].unique()) | set(df['home_team'].unique())
            print(f"Unique teams found: {len(unique_teams)}")
            print(f"Teams: {sorted(unique_teams)}")
        
        # Show sample of clean data
        print("\nSample games:")
        sample_cols = ['season', 'week', 'away_team', 'home_team', 'spread', 'total']
        available_cols = [col for col in sample_cols if col in df.columns]
        
        clean_games = df[df['away_team'] != df['home_team']]  # Filter out parsing errors
        if not clean_games.empty:
            print(clean_games[available_cols].head(10).to_string(index=False))
        else:
            print("No clean games found - parsing issues detected")
    else:
        print("No data scraped")


if __name__ == "__main__":
    analyze_website_structure()
    test_scraper_on_sample()
