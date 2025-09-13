"""NFL data scraping using ffanalytics R package and web scraping."""

import subprocess
import json
import pandas as pd
import requests
from pathlib import Path
from typing import Dict, List, Optional, Union, Any
from datetime import datetime, timedelta
import time
from loguru import logger

from ..utils.config import config
from ..utils.nfl_teams import NFLTeams


class NFLDataScraper:
    """Scraper for NFL data from multiple sources."""
    
    def __init__(self, use_r_integration: bool = True):
        """
        Initialize the scraper.
        
        Args:
            use_r_integration: Whether to use R/ffanalytics for data scraping
        """
        self.use_r_integration = use_r_integration
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36'
        })
        
    def scrape_team_ratings(self, season: int, week: int) -> pd.DataFrame:
        """
        Scrape team ratings from multiple sources.
        
        Args:
            season: NFL season year
            week: Week number (0 for season-long)
            
        Returns:
            DataFrame with team ratings from various sources
        """
        logger.info(f"Scraping team ratings for {season} season, week {week}")
        
        ratings_data = []
        
        # Try to use ffanalytics if R integration is enabled
        if self.use_r_integration:
            try:
                r_data = self._scrape_with_ffanalytics(season, week)
                if r_data is not None:
                    ratings_data.append(r_data)
            except Exception as e:
                logger.warning(f"R integration failed: {e}")
        
        # Scrape additional sources
        try:
            fivethirtyeight_data = self._scrape_fivethirtyeight(season)
            if fivethirtyeight_data is not None:
                ratings_data.append(fivethirtyeight_data)
        except Exception as e:
            logger.warning(f"FiveThirtyEight scraping failed: {e}")
        
        try:
            espn_data = self._scrape_espn_fpi(season)
            if espn_data is not None:
                ratings_data.append(espn_data)
        except Exception as e:
            logger.warning(f"ESPN FPI scraping failed: {e}")
        
        # Combine all data sources
        if ratings_data:
            combined_df = pd.concat(ratings_data, ignore_index=True)
            return self._standardize_team_names(combined_df)
        else:
            logger.error("No team ratings data could be scraped")
            return pd.DataFrame()
    
    def scrape_game_schedule(self, season: int, week: Optional[int] = None) -> pd.DataFrame:
        """
        Scrape NFL game schedule.
        
        Args:
            season: NFL season year
            week: Specific week (None for all remaining games)
            
        Returns:
            DataFrame with game schedule
        """
        logger.info(f"Scraping game schedule for {season} season")
        
        try:
            # Use ESPN API for schedule data
            schedule_data = self._scrape_espn_schedule(season, week)
            return self._standardize_team_names(schedule_data)
        except Exception as e:
            logger.error(f"Failed to scrape game schedule: {e}")
            return pd.DataFrame()
    
    def scrape_betting_odds(self, season: int, week: int) -> pd.DataFrame:
        """
        Scrape betting odds for games.
        
        Args:
            season: NFL season year
            week: Week number
            
        Returns:
            DataFrame with betting odds
        """
        logger.info(f"Scraping betting odds for {season} season, week {week}")
        
        # Note: This would need to be implemented with a specific odds provider
        # For now, return empty DataFrame
        logger.warning("Betting odds scraping not yet implemented")
        return pd.DataFrame()
    
    def _scrape_with_ffanalytics(self, season: int, week: int) -> Optional[pd.DataFrame]:
        """Use R ffanalytics package to scrape data."""
        logger.info("Using R ffanalytics package for data scraping")
        
        # Create R script for scraping
        r_script = f'''
library(ffanalytics)
library(jsonlite)

# Scrape data
my_scrape <- tryCatch({{
    scrape_data(
        src = c("CBS", "ESPN", "FantasyPros", "NumberFire", "NFL"),
        pos = c("QB", "RB", "WR", "TE", "DST"),
        season = {season},
        week = {week}
    )
}}, error = function(e) {{
    cat("Error in scraping:", e$message, "\\n")
    return(NULL)
}})

if (!is.null(my_scrape)) {{
    # Convert to JSON for Python
    json_output <- toJSON(my_scrape, auto_unbox = TRUE)
    cat(json_output)
}} else {{
    cat("null")
}}
'''
        
        try:
            # Write R script to temporary file
            script_path = config.interim_data_dir / "temp_scrape.R"
            with open(script_path, 'w') as f:
                f.write(r_script)
            
            # Execute R script
            result = subprocess.run(
                ["Rscript", str(script_path)],
                capture_output=True,
                text=True,
                timeout=300
            )
            
            if result.returncode == 0 and result.stdout.strip() != "null":
                # Parse JSON output
                data = json.loads(result.stdout)
                # Convert to pandas DataFrame
                # This would need more processing based on ffanalytics output structure
                return self._process_ffanalytics_data(data)
            else:
                logger.error(f"R script failed: {result.stderr}")
                return None
                
        except Exception as e:
            logger.error(f"Error running R script: {e}")
            return None
        finally:
            # Clean up temporary file
            if script_path.exists():
                script_path.unlink()
    
    def _scrape_fivethirtyeight(self, season: int) -> Optional[pd.DataFrame]:
        """Scrape FiveThirtyEight NFL predictions."""
        logger.info("Scraping FiveThirtyEight NFL predictions")
        
        url = f"https://projects.fivethirtyeight.com/{season}-nfl-predictions/"
        
        try:
            time.sleep(config.request_delay)
            response = self.session.get(url, timeout=config.timeout)
            response.raise_for_status()
            
            # Parse the HTML to extract team ratings
            from bs4 import BeautifulSoup
            soup = BeautifulSoup(response.content, 'html.parser')
            
            # This would need to be implemented based on current 538 structure
            # For now, return placeholder data
            teams = NFLTeams.get_all_teams()
            data = []
            for team in teams:
                data.append({
                    'team': team.abbreviation,
                    'source': 'FiveThirtyEight',
                    'elo_rating': 1500,  # Placeholder
                    'season': season
                })
            
            return pd.DataFrame(data)
            
        except Exception as e:
            logger.error(f"Error scraping FiveThirtyEight: {e}")
            return None
    
    def _scrape_espn_fpi(self, season: int) -> Optional[pd.DataFrame]:
        """Scrape ESPN Football Power Index."""
        logger.info("Scraping ESPN FPI")
        
        # ESPN FPI endpoint (this may need to be updated)
        url = "https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams"
        
        try:
            time.sleep(config.request_delay)
            response = self.session.get(url, timeout=config.timeout)
            response.raise_for_status()
            
            data = response.json()
            
            fpi_data = []
            for team_data in data.get('sports', [{}])[0].get('leagues', [{}])[0].get('teams', []):
                team = team_data.get('team', {})
                abbr = team.get('abbreviation')
                
                if abbr:
                    fpi_data.append({
                        'team': abbr,
                        'source': 'ESPN_FPI',
                        'fpi_rating': 0,  # Would extract actual FPI if available
                        'season': season
                    })
            
            return pd.DataFrame(fpi_data)
            
        except Exception as e:
            logger.error(f"Error scraping ESPN FPI: {e}")
            return None
    
    def _scrape_espn_schedule(self, season: int, week: Optional[int] = None) -> pd.DataFrame:
        """Scrape ESPN NFL schedule."""
        logger.info("Scraping ESPN NFL schedule")
        
        # ESPN NFL schedule API
        if week:
            url = f"https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard?dates={season}&week={week}"
        else:
            url = f"https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard?dates={season}"
        
        try:
            time.sleep(config.request_delay)
            response = self.session.get(url, timeout=config.timeout)
            response.raise_for_status()
            
            data = response.json()
            
            games = []
            for event in data.get('events', []):
                competition = event.get('competitions', [{}])[0]
                competitors = competition.get('competitors', [])
                
                if len(competitors) >= 2:
                    away_team = competitors[0]['team']['abbreviation']
                    home_team = competitors[1]['team']['abbreviation']
                    
                    games.append({
                        'season': season,
                        'week': event.get('week', {}).get('number', 1),
                        'away_team': away_team,
                        'home_team': home_team,
                        'game_date': event.get('date'),
                        'game_id': event.get('id')
                    })
            
            return pd.DataFrame(games)
            
        except Exception as e:
            logger.error(f"Error scraping ESPN schedule: {e}")
            return pd.DataFrame()
    
    def _process_ffanalytics_data(self, data: Dict[str, Any]) -> pd.DataFrame:
        """Process ffanalytics data into standardized format."""
        # This would process the complex ffanalytics data structure
        # For now, return empty DataFrame
        logger.info("Processing ffanalytics data")
        return pd.DataFrame()
    
    def _standardize_team_names(self, df: pd.DataFrame) -> pd.DataFrame:
        """Standardize team names to abbreviations."""
        if df.empty:
            return df
        
        # Find team columns and standardize them
        team_columns = [col for col in df.columns if 'team' in col.lower()]
        
        for col in team_columns:
            df[col] = df[col].apply(lambda x: NFLTeams.get_abbreviation(x) or x)
        
        return df
    
    def save_data(self, data: pd.DataFrame, filename: str, data_type: str = "interim") -> Path:
        """
        Save scraped data to file.
        
        Args:
            data: DataFrame to save
            filename: Name of the file
            data_type: Type of data (raw, interim, processed)
            
        Returns:
            Path to saved file
        """
        if data_type == "raw":
            save_dir = config.raw_data_dir
        elif data_type == "processed":
            save_dir = config.processed_data_dir
        else:
            save_dir = config.interim_data_dir
        
        filepath = save_dir / filename
        
        if filename.endswith('.csv'):
            data.to_csv(filepath, index=False)
        elif filename.endswith('.parquet'):
            data.to_parquet(filepath, index=False)
        else:
            data.to_pickle(filepath)
        
        logger.info(f"Saved data to {filepath}")
        return filepath
