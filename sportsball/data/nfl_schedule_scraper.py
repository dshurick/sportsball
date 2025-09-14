"""NFL schedule scraper for official NFL Operations website."""

import pandas as pd
import requests
from bs4 import BeautifulSoup
import re
from typing import Dict, List, Optional, Tuple
from datetime import datetime, timedelta
from loguru import logger

from ..utils.config import config
from ..utils.nfl_teams import NFLTeams


class NFLScheduleScraper:
    """Scraper for NFL schedule from operations.nfl.com."""
    
    def __init__(self):
        """Initialize the schedule scraper."""
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36'
        })
        self.nfl_teams = NFLTeams()
    
    def scrape_2025_schedule(self) -> pd.DataFrame:
        """
        Scrape the complete 2025 NFL schedule.
        
        Returns:
            DataFrame with columns: [week, date, away_team, home_team, time, network]
        """
        logger.info("Scraping 2025 NFL schedule from NFL Operations")
        
        url = "https://operations.nfl.com/gameday/nfl-schedule/2025-nfl-schedule/"
        
        try:
            response = self.session.get(url, timeout=30)
            response.raise_for_status()
            
            soup = BeautifulSoup(response.content, 'html.parser')
            
            # Find all tables containing schedule data
            games = []
            current_week = None
            
            # Look for week headers and game tables
            for element in soup.find_all(['h3', 'h4', 'table', 'tr']):
                
                # Check for week headers
                if element.name in ['h3', 'h4']:
                    week_text = element.get_text(strip=True)
                    week_match = re.search(r'WEEK\s+(\d+)', week_text, re.IGNORECASE)
                    if week_match:
                        current_week = int(week_match.group(1))
                        logger.debug(f"Found week {current_week}")
                        continue
                
                # Process table rows
                if element.name == 'tr' and current_week is not None:
                    cells = element.find_all(['td', 'th'])
                    if len(cells) >= 3:  # Need at least matchup, time, network
                        game_data = self._parse_game_row(cells, current_week)
                        if game_data:
                            games.append(game_data)
            
            # If table parsing didn't work, try parsing the raw text content
            if not games:
                logger.info("Table parsing failed, trying text parsing")
                games = self._parse_schedule_text(soup)
            
            if games:
                df = pd.DataFrame(games)
                logger.info(f"Successfully scraped {len(df)} games from {df['week'].nunique()} weeks")
                return self._clean_schedule_data(df)
            else:
                logger.error("No games found in schedule")
                return pd.DataFrame()
                
        except Exception as e:
            logger.error(f"Failed to scrape NFL schedule: {e}")
            return pd.DataFrame()
    
    def _parse_game_row(self, cells: List, week: int) -> Optional[Dict]:
        """Parse a single game row from the table."""
        
        try:
            cell_texts = [cell.get_text(strip=True) for cell in cells]
            
            # Look for matchup pattern: "Team1 at Team2" or "Team1 vs Team2"
            matchup_text = None
            time_text = None
            network_text = None
            
            for i, text in enumerate(cell_texts):
                if ' at ' in text or ' vs ' in text:
                    matchup_text = text
                    # Time is usually in the next column
                    if i + 1 < len(cell_texts):
                        time_text = cell_texts[i + 1]
                    # Network might be in the column after that
                    if i + 2 < len(cell_texts):
                        network_text = cell_texts[i + 2]
                    break
            
            if not matchup_text:
                return None
            
            # Parse teams from matchup
            if ' at ' in matchup_text:
                away_team, home_team = matchup_text.split(' at ', 1)
            elif ' vs ' in matchup_text:
                # Handle neutral site games
                away_team, home_team = matchup_text.split(' vs ', 1)
            else:
                return None
            
            # Clean team names
            away_team = self._normalize_team_name(away_team.strip())
            home_team = self._normalize_team_name(home_team.strip())
            
            if not away_team or not home_team:
                return None
            
            return {
                'week': week,
                'away_team': away_team,
                'home_team': home_team,
                'time': time_text or 'TBD',
                'network': network_text or 'TBD'
            }
            
        except Exception as e:
            logger.debug(f"Error parsing game row: {e}")
            return None
    
    def _parse_schedule_text(self, soup: BeautifulSoup) -> List[Dict]:
        """Parse schedule from raw text content as fallback."""
        
        games = []
        text_content = soup.get_text()
        
        # Split by weeks
        week_sections = re.split(r'WEEK\s+(\d+)', text_content, flags=re.IGNORECASE)
        
        for i in range(1, len(week_sections), 2):  # Skip first empty section
            if i + 1 >= len(week_sections):
                break
                
            week_num = int(week_sections[i])
            week_content = week_sections[i + 1]
            
            # Find game matchups in this week's content
            matchup_patterns = [
                r'([A-Za-z\s]+?)\s+at\s+([A-Za-z\s]+?)(?:\s+[\d:]+[ap]|$)',
                r'([A-Za-z\s]+?)\s+vs\s+([A-Za-z\s]+?)(?:\s+[\d:]+[ap]|$)'
            ]
            
            for pattern in matchup_patterns:
                matches = re.findall(pattern, week_content)
                for away_raw, home_raw in matches:
                    away_team = self._normalize_team_name(away_raw.strip())
                    home_team = self._normalize_team_name(home_raw.strip())
                    
                    if away_team and home_team and away_team != home_team:
                        games.append({
                            'week': week_num,
                            'away_team': away_team,
                            'home_team': home_team,
                            'time': 'TBD',
                            'network': 'TBD'
                        })
        
        return games
    
    def _normalize_team_name(self, team_text: str) -> Optional[str]:
        """Normalize team name to standard abbreviation."""
        
        # Remove extra whitespace and common suffixes
        team_text = re.sub(r'\s+', ' ', team_text.strip())
        team_text = re.sub(r'\s*\([^)]*\)', '', team_text)  # Remove parentheses
        
        # Try direct lookup first
        abbr = self.nfl_teams.get_abbreviation(team_text)
        if abbr:
            return abbr
        
        # Try common variations and mappings
        team_mappings = {
            'Dallas Cowboys': 'DAL',
            'Philadelphia Eagles': 'PHI',
            'Kansas City Chiefs': 'KC',
            'Los Angeles Chargers': 'LAC',
            'Tampa Bay Buccaneers': 'TB',
            'Atlanta Falcons': 'ATL',
            'Cincinnati Bengals': 'CIN',
            'Cleveland Browns': 'CLE',
            'Miami Dolphins': 'MIA',
            'Indianapolis Colts': 'IND',
            'Carolina Panthers': 'CAR',
            'Jacksonville Jaguars': 'JAX',
            'Las Vegas Raiders': 'LV',
            'New England Patriots': 'NE',
            'Arizona Cardinals': 'ARI',
            'New Orleans Saints': 'NO',
            'Pittsburgh Steelers': 'PIT',
            'New York Jets': 'NYJ',
            'New York Giants': 'NYG',
            'Washington Commanders': 'WAS',
            'Tennessee Titans': 'TEN',
            'Denver Broncos': 'DEN',
            'San Francisco 49ers': 'SF',
            'Seattle Seahawks': 'SEA',
            'Detroit Lions': 'DET',
            'Green Bay Packers': 'GB',
            'Houston Texans': 'HOU',
            'Los Angeles Rams': 'LAR',
            'Baltimore Ravens': 'BAL',
            'Buffalo Bills': 'BUF',
            'Minnesota Vikings': 'MIN',
            'Chicago Bears': 'CHI'
        }
        
        # Try exact match
        if team_text in team_mappings:
            return team_mappings[team_text]
        
        # Try partial matches
        for full_name, abbr in team_mappings.items():
            if team_text.lower() in full_name.lower() or full_name.lower() in team_text.lower():
                return abbr
        
        logger.debug(f"Could not normalize team name: '{team_text}'")
        return None
    
    def _clean_schedule_data(self, df: pd.DataFrame) -> pd.DataFrame:
        """Clean and validate the schedule data."""
        
        # Remove duplicates
        df = df.drop_duplicates(subset=['week', 'away_team', 'home_team'])
        
        # Filter out invalid weeks (should be 1-18)
        df = df[df['week'].between(1, 18)]
        
        # Add game_id
        df['game_id'] = df.apply(
            lambda row: f"2025_W{row['week']:02d}_{row['away_team']}_at_{row['home_team']}", 
            axis=1
        )
        
        # Add season
        df['season'] = 2025
        
        # Sort by week and teams
        df = df.sort_values(['week', 'away_team', 'home_team']).reset_index(drop=True)
        
        logger.info(f"Cleaned schedule: {len(df)} games across weeks {df['week'].min()}-{df['week'].max()}")
        
        return df
