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
            
            # Look for anchor tags with id attributes (like <a id="9">)
            week_anchors_by_id = soup.find_all('a', id=re.compile(r'^\d+$'))
            logger.info(f"Found {len(week_anchors_by_id)} week anchors by ID")
            
            # Try to find tables directly
            soh_tables = soup.find_all('table', class_='soh1')
            logger.info(f"Found {len(soh_tables)} tables on page")
            
            # Use anchor-based parsing if we found week anchors
            if week_anchors_by_id:
                logger.info("Using anchor-based week identification")
                for anchor in week_anchors_by_id:
                    try:
                        week_num = int(anchor.get('id'))
                        if week_num > 18:  # Skip playoff weeks for now
                            continue
                            
                        logger.info(f"Looking for Week {week_num} table")
                        
                        # Find the next table after this anchor
                        current = anchor
                        table = None
                        # Look through next siblings to find the table
                        for sibling in anchor.find_next_siblings():
                            if sibling.name == 'table' and 'soh1' in sibling.get('class', []):
                                table = sibling
                                break
                            # Stop if we hit another week anchor
                            if sibling.name == 'a' and sibling.get('id', '').isdigit():
                                break
                        
                        if table:
                            logger.info(f"Found table for Week {week_num}")
                            week_games = self._parse_week_table(table, season, week_num)
                            if week_games:
                                games_data.extend(week_games)
                        else:
                            logger.warning(f"No table found for Week {week_num}")
                            
                    except (ValueError, AttributeError) as e:
                        logger.warning(f"Error parsing week anchor: {e}")
                        continue
            else:
                # Fallback to sequential parsing
                logger.info("Falling back to sequential table parsing")
                for i, table in enumerate(soh_tables[:18]):  # Limit to reasonable number
                    week_num = i + 1
                    logger.info(f"Parsing table {i+1} as week {week_num}")
                    week_games = self._parse_week_table(table, season, week_num)
                    if week_games:
                        games_data.extend(week_games)
            
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
        """Parse a single game row from sportsoddshistory.com format."""
        try:
            # Extract text from cells
            cell_texts = [cell.get_text(strip=True) for cell in cells]
            
            # Skip if not enough cells or if it's a header row
            if len(cell_texts) < 10:
                return None
                
            # Skip header rows
            if any(header in ' '.join(cell_texts).lower() for header in ['day', 'date', 'time', 'favorite', 'underdog']):
                return None
            
            # sportsoddshistory.com format:
            # 0: Day, 1: Date, 2: Time, 3: @, 4: Favorite, 5: Score, 6: Spread, 7: @, 8: Underdog, 9: Over/Under, 10: Notes
            
            at_symbol_col3 = cell_texts[3] if len(cell_texts) > 3 else ""
            favorite_text = cell_texts[4] if len(cell_texts) > 4 else ""
            score_text = cell_texts[5] if len(cell_texts) > 5 else ""
            spread_text = cell_texts[6] if len(cell_texts) > 6 else ""
            at_symbol_col7 = cell_texts[7] if len(cell_texts) > 7 else ""
            underdog_text = cell_texts[8] if len(cell_texts) > 8 else ""
            total_text = cell_texts[9] if len(cell_texts) > 9 else ""
            
            # Extract team names
            favorite_team = self._normalize_team_name(favorite_text)
            underdog_team = self._normalize_team_name(underdog_text)
            
            if not favorite_team or not underdog_team:
                return None
            
            # Determine home/away based on @ symbols in columns 3 and 7
            # Column 3 @ means favorite is home, Column 7 @ means underdog is home
            if at_symbol_col3 == "@":
                # Favorite is home, underdog is away
                home_team = favorite_team
                away_team = underdog_team
            elif at_symbol_col7 == "@":
                # Underdog is home, favorite is away
                home_team = underdog_team
                away_team = favorite_team
            else:
                # Neutral site game - default to favorite as home
                home_team = favorite_team
                away_team = underdog_team
            
            # Parse spread
            spread_value = None
            spread_favorite = None
            if spread_text:
                spread_match = re.search(r'([WL])\s*([+-]?\d+(?:\.\d+)?)', spread_text)
                if spread_match:
                    win_loss = spread_match.group(1)
                    spread_value = float(spread_match.group(2))
                    # Determine which team was favored based on home/away assignment
                    # We already determined home/away above, so use that logic
                    if at_symbol_col3 == "@":
                        # Favorite is home, so if spread_value < 0, home is favored
                        spread_favorite = "home" if spread_value < 0 else "away"
                    elif at_symbol_col7 == "@":
                        # Underdog is home, so if spread_value < 0, away is favored
                        spread_favorite = "away" if spread_value < 0 else "home"
                    else:
                        # Neutral site, default to favorite being favored
                        spread_favorite = "home" if spread_value < 0 else "away"
            
            # Parse total
            total_value = None
            if total_text:
                total_match = re.search(r'([OU])\s*(\d+(?:\.\d+)?)', total_text)
                if total_match:
                    total_value = float(total_match.group(2))
            
            # Parse score
            away_score = None
            home_score = None
            winner = None
            if score_text:
                # Look for score pattern like "W 27-21" or "L 21-27"
                score_match = re.search(r'([WL])\s*(\d+)-(\d+)', score_text)
                if score_match:
                    win_loss = score_match.group(1)
                    score1 = int(score_match.group(2))
                    score2 = int(score_match.group(3))
                    
                    # The score_text is always for the 'favorite' team column
                    if win_loss == "W":
                        if at_symbol_col3 == "@":
                            # Favorite is home and won
                            home_score = score1
                            away_score = score2
                            winner = "home"
                        elif at_symbol_col7 == "@":
                            # Underdog is home, favorite is away and won
                            away_score = score1
                            home_score = score2
                            winner = "away"
                        else:
                            # Neutral site, favorite won (default to home)
                            home_score = score1
                            away_score = score2
                            winner = "home"
                    else:  # win_loss == "L"
                        if at_symbol_col3 == "@":
                            # Favorite is home and lost
                            home_score = score2
                            away_score = score1
                            winner = "away"
                        elif at_symbol_col7 == "@":
                            # Underdog is home, favorite is away and lost
                            away_score = score2
                            home_score = score1
                            winner = "home"
                        else:
                            # Neutral site, favorite lost (default to home)
                            home_score = score2
                            away_score = score1
                            winner = "away"
            
            # Parse date for game_date
            date_text = cell_texts[1] if len(cell_texts) > 1 else ""
            game_date = None
            if date_text:
                try:
                    # Parse date like "Oct 31, 2024"
                    game_date = datetime.strptime(date_text, "%b %d, %Y").strftime("%Y-%m-%d")
                except:
                    game_date = f"{season}-01-01"  # fallback
            
            game_data = {
                'season': season,
                'week': week,
                'away_team': away_team,
                'home_team': home_team,
                'spread': abs(spread_value) if spread_value else None,
                'spread_favorite': spread_favorite,
                'spread_line': spread_value,
                'total': total_value,
                'away_score': away_score,
                'home_score': home_score,
                'winner': winner,
                'game_date': game_date
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
