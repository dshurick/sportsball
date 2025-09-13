"""NFL odds scraper for Sports Odds History website."""

import pandas as pd
import requests
from bs4 import BeautifulSoup
import time
import re
from typing import Dict, List, Optional, Tuple
from datetime import datetime
from loguru import logger

from ..utils.config import config
from ..utils.nfl_teams import NFLTeams


class SportsOddsHistoryScraper:
    """Scraper for historical NFL odds from sportsoddshistory.com."""
    
    def __init__(self):
        """Initialize the odds scraper."""
        self.base_url = "https://www.sportsoddshistory.com"
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36'
        })
        self.nfl_teams = NFLTeams()
    
    def scrape_season_odds(self, season: int) -> pd.DataFrame:
        """
        Scrape all NFL odds for a given season.
        
        Args:
            season: NFL season year (e.g., 2024)
            
        Returns:
            DataFrame with game odds data
        """
        logger.info(f"Scraping NFL odds for {season} season from Sports Odds History")
        
        url = f"{self.base_url}/nfl-game-season/?y={season}"
        
        try:
            time.sleep(config.request_delay)
            response = self.session.get(url, timeout=config.timeout)
            response.raise_for_status()
            
            soup = BeautifulSoup(response.content, 'html.parser')
            
            # Find all game tables for each week
            games_data = []
            
            # Debug: Let's see what we can find
            logger.info("Analyzing page structure...")
            
            # Look for different possible week indicators
            week_links = soup.find_all('a', href=re.compile(r'#\d+'))
            week_anchors = soup.find_all('a', {'name': re.compile(r'^\d+$')})
            
            logger.info(f"Found {len(week_links)} week links and {len(week_anchors)} week anchors")
            
            # Try to find tables directly
            all_tables = soup.find_all('table')
            logger.info(f"Found {len(all_tables)} tables on page")
            
            # Look for week sections in different ways
            week_sections = week_anchors if week_anchors else []
            
            # If no anchors found, try to find week patterns in text
            if not week_sections:
                # Look for "Week X" patterns in the HTML
                week_headers = soup.find_all(text=re.compile(r'Week\s+\d+', re.IGNORECASE))
                logger.info(f"Found {len(week_headers)} week header texts")
                
                # Try to parse tables sequentially if we can't find specific week markers
                if all_tables:
                    logger.info("Attempting to parse tables sequentially")
                    for i, table in enumerate(all_tables[:18]):  # Assume first 18 tables are weeks 1-18
                        week_num = i + 1
                        logger.info(f"Parsing table {i+1} as week {week_num}")
                        week_games = self._parse_week_table(table, season, week_num)
                        if week_games:
                            games_data.extend(week_games)
            else:
                # Original logic with week anchors
                for week_anchor in week_sections:
                    try:
                        week_num = int(week_anchor.get('name'))
                        if week_num > 18:  # Skip playoff weeks for now
                            continue
                            
                        logger.info(f"Scraping week {week_num}")
                        
                        # Find the table following this week anchor
                        week_table = self._find_week_table(week_anchor)
                        if week_table:
                            week_games = self._parse_week_table(week_table, season, week_num)
                            games_data.extend(week_games)
                            
                    except (ValueError, AttributeError) as e:
                        logger.warning(f"Error parsing week section: {e}")
                        continue
            
            if games_data:
                df = pd.DataFrame(games_data)
                logger.info(f"Scraped {len(df)} games for {season} season")
                return df
            else:
                logger.warning(f"No games found for {season} season")
                return pd.DataFrame()
                
        except Exception as e:
            logger.error(f"Error scraping {season} season odds: {e}")
            return pd.DataFrame()
    
    def _find_week_table(self, week_anchor) -> Optional[BeautifulSoup]:
        """Find the game table for a given week anchor."""
        # Look for the next table element after the week anchor
        current = week_anchor
        while current:
            current = current.find_next_sibling()
            if current and current.name == 'table':
                return current
            # Also check for tables in following elements
            if current:
                table = current.find('table')
                if table:
                    return table
        return None
    
    def _parse_week_table(self, table: BeautifulSoup, season: int, week: int) -> List[Dict]:
        """Parse a week's game table into structured data."""
        games = []
        
        try:
            rows = table.find_all('tr')
            logger.debug(f"Found {len(rows)} rows in week {week} table")
            
            # Look at the table structure
            if rows:
                first_row = rows[0]
                cells = first_row.find_all(['td', 'th'])
                logger.debug(f"First row has {len(cells)} cells: {[cell.get_text(strip=True) for cell in cells[:5]]}")
            
            for i, row in enumerate(rows):
                cells = row.find_all(['td', 'th'])
                if len(cells) < 3:  # Need minimum columns for game data
                    continue
                
                # Skip header rows (usually contain column names)
                cell_texts = [cell.get_text(strip=True) for cell in cells]
                if any(header in ' '.join(cell_texts).lower() for header in ['date', 'time', 'team', 'spread', 'total']):
                    logger.debug(f"Skipping header row {i}: {cell_texts[:3]}")
                    continue
                
                try:
                    game_data = self._parse_game_row(cells, season, week)
                    if game_data:
                        games.append(game_data)
                        logger.debug(f"Parsed game: {game_data['away_team']} @ {game_data['home_team']}")
                except Exception as e:
                    logger.debug(f"Error parsing game row {i}: {e}")
                    continue
        
        except Exception as e:
            logger.warning(f"Error parsing week {week} table: {e}")
        
        logger.info(f"Parsed {len(games)} games for week {week}")
        return games
    
    def _parse_game_row(self, cells: List, season: int, week: int) -> Optional[Dict]:
        """Parse a single game row into structured data."""
        try:
            # Extract text from cells
            cell_texts = [cell.get_text(strip=True) for cell in cells]
            
            # Look for team names and odds patterns
            # Format is typically: "Team1 @ Team2" or "Team1 vs Team2"
            game_info = None
            
            # Look for team matchup patterns in different cells
            for i, text in enumerate(cell_texts):
                # Look for team matchup (contains @ or vs or common team abbreviations)
                if any(pattern in text for pattern in ['@', ' vs ', ' at ', 'vs.']):
                    game_info = text
                    break
                # Also look for cells that might contain team names
                elif len(text) >= 2 and text.isupper() and len(text) <= 10:
                    # Might be team abbreviations
                    potential_teams = text.split()
                    if len(potential_teams) == 2:
                        # Check if both look like team abbreviations
                        if all(len(team) <= 4 and team.isupper() for team in potential_teams):
                            game_info = f"{potential_teams[0]} @ {potential_teams[1]}"
                            break
            
            # If still no game info, try to construct from individual team cells
            if not game_info:
                # Look for individual team abbreviations in adjacent cells
                team_candidates = []
                for text in cell_texts:
                    if len(text) == 2 or len(text) == 3:
                        abbr = self._normalize_team_name(text)
                        if abbr:
                            team_candidates.append(abbr)
                
                if len(team_candidates) >= 2:
                    game_info = f"{team_candidates[0]} @ {team_candidates[1]}"
            
            if not game_info:
                return None
            
            # Parse team names
            teams = self._parse_team_matchup(game_info)
            if not teams:
                return None
            
            away_team, home_team = teams
            
            # Look for spread and total in nearby cells
            spread_data = self._extract_spread_from_cells(cell_texts)
            total_data = self._extract_total_from_cells(cell_texts)
            
            # Try to find the result/score
            result_data = self._extract_result_from_cells(cell_texts)
            
            game_data = {
                'season': season,
                'week': week,
                'away_team': away_team,
                'home_team': home_team,
                'game_info': game_info,
                **spread_data,
                **total_data,
                **result_data
            }
            
            return game_data
            
        except Exception as e:
            logger.debug(f"Error parsing game row: {e}")
            return None
    
    def _parse_team_matchup(self, game_text: str) -> Optional[Tuple[str, str]]:
        """Parse team matchup text to extract away and home teams."""
        try:
            # Handle different formats: "Team1 @ Team2", "Team1 at Team2", "Team1 vs Team2"
            if '@' in game_text:
                parts = game_text.split('@')
            elif ' at ' in game_text:
                parts = game_text.split(' at ')
            elif ' vs ' in game_text:
                parts = game_text.split(' vs ')
            else:
                return None
            
            if len(parts) != 2:
                return None
            
            away_team_text = parts[0].strip()
            home_team_text = parts[1].strip()
            
            # Convert to standard abbreviations
            away_team = self._normalize_team_name(away_team_text)
            home_team = self._normalize_team_name(home_team_text)
            
            if away_team and home_team:
                return away_team, home_team
            
            return None
            
        except Exception:
            return None
    
    def _normalize_team_name(self, team_text: str) -> Optional[str]:
        """Normalize team name to standard abbreviation."""
        # Remove common prefixes/suffixes
        team_text = re.sub(r'\s*\([^)]*\)', '', team_text)  # Remove parentheses
        team_text = team_text.strip()
        
        # Handle common abbreviations directly
        common_abbrevs = {
            'ARI': 'ARI', 'ATL': 'ATL', 'BAL': 'BAL', 'BUF': 'BUF', 'CAR': 'CAR',
            'CHI': 'CHI', 'CIN': 'CIN', 'CLE': 'CLE', 'DAL': 'DAL', 'DEN': 'DEN',
            'DET': 'DET', 'GB': 'GB', 'HOU': 'HOU', 'IND': 'IND', 'JAX': 'JAX',
            'KC': 'KC', 'LV': 'LV', 'LAC': 'LAC', 'LAR': 'LAR', 'MIA': 'MIA',
            'MIN': 'MIN', 'NE': 'NE', 'NO': 'NO', 'NYG': 'NYG', 'NYJ': 'NYJ',
            'PHI': 'PHI', 'PIT': 'PIT', 'SF': 'SF', 'SEA': 'SEA', 'TB': 'TB',
            'TEN': 'TEN', 'WAS': 'WAS',
            # Alternative abbreviations
            'NWE': 'NE', 'NOR': 'NO', 'GNB': 'GB', 'KAN': 'KC', 'TAM': 'TB',
            'LVR': 'LV', 'LAS': 'LV', 'SDG': 'LAC', 'STL': 'LAR', 'WFT': 'WAS'
        }
        
        if team_text.upper() in common_abbrevs:
            return common_abbrevs[team_text.upper()]
        
        # Try direct lookup
        abbr = self.nfl_teams.get_abbreviation(team_text)
        if abbr:
            return abbr
        
        # Try some common variations
        variations = [
            team_text.replace('New York', 'NY'),
            team_text.replace('Los Angeles', 'LA'),
            team_text.replace('Las Vegas', 'LV'),
            team_text.replace('Tampa Bay', 'TB'),
            team_text.replace('Green Bay', 'GB'),
            team_text.replace('San Francisco', 'SF'),
            team_text.replace('Kansas City', 'KC'),
            team_text.replace('New England', 'NE'),
        ]
        
        for variation in variations:
            abbr = self.nfl_teams.get_abbreviation(variation)
            if abbr:
                return abbr
        
        logger.debug(f"Could not normalize team name: {team_text}")
        return None
    
    def _extract_spread_from_cells(self, cell_texts: List[str]) -> Dict:
        """Extract spread information from table cells."""
        spread_data = {
            'spread': None,
            'spread_favorite': None,
            'spread_line': None
        }
        
        for text in cell_texts:
            # Look for spread patterns like "-3.5", "+7", "PK"
            spread_match = re.search(r'([+-]?\d+\.?\d*)', text)
            if spread_match and ('.' in spread_match.group(1) or abs(float(spread_match.group(1))) <= 21):
                spread_value = float(spread_match.group(1))
                spread_data['spread_line'] = spread_value
                
                # Determine favorite (negative spread means favorite)
                if spread_value < 0:
                    spread_data['spread'] = abs(spread_value)
                    spread_data['spread_favorite'] = 'home'  # Assuming home team listed second
                elif spread_value > 0:
                    spread_data['spread'] = abs(spread_value)
                    spread_data['spread_favorite'] = 'away'
                else:
                    spread_data['spread'] = 0
                    spread_data['spread_favorite'] = 'pick'
                break
        
        return spread_data
    
    def _extract_total_from_cells(self, cell_texts: List[str]) -> Dict:
        """Extract over/under total from table cells."""
        total_data = {
            'total': None,
            'over_under': None
        }
        
        for text in cell_texts:
            # Look for total patterns like "O 47.5", "U 42", "Over 45"
            total_match = re.search(r'(?:O|U|Over|Under)\s*(\d+\.?\d*)', text, re.IGNORECASE)
            if total_match:
                total_data['total'] = float(total_match.group(1))
                if text.upper().startswith(('O', 'OVER')):
                    total_data['over_under'] = 'over'
                else:
                    total_data['over_under'] = 'under'
                break
            
            # Also look for just numbers that might be totals (typically 35-65 range)
            number_match = re.search(r'\b(\d{2}\.?\d*)\b', text)
            if number_match:
                num = float(number_match.group(1))
                if 35 <= num <= 65:  # Reasonable NFL total range
                    total_data['total'] = num
                    break
        
        return total_data
    
    def _extract_result_from_cells(self, cell_texts: List[str]) -> Dict:
        """Extract game result/score from table cells."""
        result_data = {
            'away_score': None,
            'home_score': None,
            'winner': None
        }
        
        for text in cell_texts:
            # Look for score patterns like "24-21", "31-17"
            score_match = re.search(r'(\d+)-(\d+)', text)
            if score_match:
                score1 = int(score_match.group(1))
                score2 = int(score_match.group(2))
                
                # Assume first score is away team (this might need adjustment based on site format)
                result_data['away_score'] = score1
                result_data['home_score'] = score2
                
                if score1 > score2:
                    result_data['winner'] = 'away'
                elif score2 > score1:
                    result_data['winner'] = 'home'
                else:
                    result_data['winner'] = 'tie'
                break
        
        return result_data
    
    def scrape_multiple_seasons(self, start_year: int, end_year: int) -> pd.DataFrame:
        """
        Scrape odds for multiple seasons.
        
        Args:
            start_year: Starting season year
            end_year: Ending season year (inclusive)
            
        Returns:
            Combined DataFrame with all seasons
        """
        logger.info(f"Scraping NFL odds for seasons {start_year}-{end_year}")
        
        all_seasons_data = []
        
        for year in range(start_year, end_year + 1):
            logger.info(f"Scraping season {year}")
            season_data = self.scrape_season_odds(year)
            
            if not season_data.empty:
                all_seasons_data.append(season_data)
            
            # Be respectful with delays between seasons
            if year < end_year:
                time.sleep(config.request_delay * 2)
        
        if all_seasons_data:
            combined_df = pd.concat(all_seasons_data, ignore_index=True)
            logger.info(f"Combined data: {len(combined_df)} games across {len(all_seasons_data)} seasons")
            return combined_df
        else:
            logger.warning("No data scraped for any season")
            return pd.DataFrame()
    
    def save_odds_data(self, df: pd.DataFrame, filename: str) -> str:
        """
        Save odds data to file.
        
        Args:
            df: DataFrame with odds data
            filename: Name of file to save
            
        Returns:
            Path to saved file
        """
        if df.empty:
            logger.warning("No data to save")
            return ""
        
        # Add timestamp to filename
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        if not filename.endswith('.csv'):
            filename = f"{filename}_{timestamp}.csv"
        
        filepath = config.raw_data_dir / filename
        df.to_csv(filepath, index=False)
        
        logger.info(f"Saved {len(df)} games to {filepath}")
        return str(filepath)
    
    def get_odds_summary(self, df: pd.DataFrame) -> Dict:
        """
        Get summary statistics of scraped odds data.
        
        Args:
            df: DataFrame with odds data
            
        Returns:
            Summary statistics dictionary
        """
        if df.empty:
            return {}
        
        summary = {
            'total_games': len(df),
            'seasons': sorted(df['season'].unique()) if 'season' in df.columns else [],
            'weeks': sorted(df['week'].unique()) if 'week' in df.columns else [],
            'teams': sorted(df['away_team'].unique()) if 'away_team' in df.columns else [],
            'games_with_spread': df['spread'].notna().sum() if 'spread' in df.columns else 0,
            'games_with_total': df['total'].notna().sum() if 'total' in df.columns else 0,
            'games_with_results': df['winner'].notna().sum() if 'winner' in df.columns else 0,
        }
        
        return summary
