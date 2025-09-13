"""Data processing modules for NFL data."""

import pandas as pd
import numpy as np
from typing import Dict, List, Optional, Tuple
from datetime import datetime, timedelta
from loguru import logger

from ..utils.config import config
from ..utils.nfl_teams import NFLTeams


class GameDataProcessor:
    """Process NFL game data for modeling."""
    
    def __init__(self):
        """Initialize the processor."""
        self.nfl_teams = NFLTeams()
    
    def process_historical_games(self, games_df: pd.DataFrame, include_odds: bool = True) -> pd.DataFrame:
        """
        Process historical game results for model training.
        
        Args:
            games_df: DataFrame with historical game results
            
        Returns:
            Processed DataFrame ready for modeling
        """
        logger.info("Processing historical game data")
        
        if games_df.empty:
            return pd.DataFrame()
        
        # Standardize column names
        games_df = self._standardize_columns(games_df)
        
        # Add derived features
        games_df = self._add_game_features(games_df)
        
        # Add odds-based features if available
        if include_odds:
            games_df = self._add_odds_features(games_df)
        
        # Create target variable (1 if away team wins, 0 if home team wins)
        games_df['away_team_win'] = (games_df['away_score'] > games_df['home_score']).astype(int)
        
        return games_df
    
    def process_upcoming_games(self, games_df: pd.DataFrame, ratings_df: pd.DataFrame) -> pd.DataFrame:
        """
        Process upcoming games with current team ratings.
        
        Args:
            games_df: DataFrame with upcoming games
            ratings_df: DataFrame with current team ratings
            
        Returns:
            Processed DataFrame ready for prediction
        """
        logger.info("Processing upcoming games with team ratings")
        
        if games_df.empty:
            return pd.DataFrame()
        
        # Standardize column names
        games_df = self._standardize_columns(games_df)
        
        # Merge with team ratings
        games_df = self._merge_team_ratings(games_df, ratings_df)
        
        # Add derived features
        games_df = self._add_game_features(games_df)
        
        return games_df
    
    def _standardize_columns(self, df: pd.DataFrame) -> pd.DataFrame:
        """Standardize column names."""
        column_mapping = {
            'away': 'away_team',
            'home': 'home_team',
            'visitor': 'away_team',
            'host': 'home_team',
            'away_pts': 'away_score',
            'home_pts': 'home_score',
            'visitor_pts': 'away_score',
            'host_pts': 'home_score',
        }
        
        df = df.rename(columns=column_mapping)
        
        # Ensure required columns exist
        required_cols = ['season', 'week', 'away_team', 'home_team']
        for col in required_cols:
            if col not in df.columns:
                if col == 'season':
                    df[col] = config.current_season
                elif col == 'week':
                    df[col] = 1
                else:
                    logger.warning(f"Missing required column: {col}")
        
        return df
    
    def _add_game_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add derived features for games."""
        # Add home field advantage indicator
        df['home_field_advantage'] = 1
        
        # Add division game indicator
        df['division_game'] = df.apply(self._is_division_game, axis=1)
        
        # Add conference game indicator
        df['conference_game'] = df.apply(self._is_conference_game, axis=1)
        
        # Add week-based features
        df['early_season'] = (df['week'] <= 4).astype(int)
        df['late_season'] = (df['week'] >= 15).astype(int)
        df['playoffs'] = (df['week'] > 18).astype(int)
        
        return df
    
    def _merge_team_ratings(self, games_df: pd.DataFrame, ratings_df: pd.DataFrame) -> pd.DataFrame:
        """Merge team ratings with game data."""
        if ratings_df.empty:
            logger.warning("No team ratings available for merging")
            return games_df
        
        # Pivot ratings data to have one row per team
        ratings_pivot = self._pivot_ratings(ratings_df)
        
        # Merge away team ratings
        games_df = games_df.merge(
            ratings_pivot.add_prefix('away_'),
            left_on='away_team',
            right_index=True,
            how='left'
        )
        
        # Merge home team ratings
        games_df = games_df.merge(
            ratings_pivot.add_prefix('home_'),
            left_on='home_team',
            right_index=True,
            how='left'
        )
        
        # Calculate rating differentials
        rating_cols = [col for col in ratings_pivot.columns if 'rating' in col.lower()]
        for col in rating_cols:
            away_col = f'away_{col}'
            home_col = f'home_{col}'
            if away_col in games_df.columns and home_col in games_df.columns:
                games_df[f'{col}_differential'] = games_df[away_col] - games_df[home_col]
        
        return games_df
    
    def _pivot_ratings(self, ratings_df: pd.DataFrame) -> pd.DataFrame:
        """Pivot ratings data to have one row per team."""
        if 'source' in ratings_df.columns:
            # If multiple sources, take the mean
            ratings_agg = ratings_df.groupby('team').agg({
                col: 'mean' for col in ratings_df.columns 
                if col not in ['team', 'source', 'season']
            }).reset_index()
        else:
            ratings_agg = ratings_df.copy()
        
        return ratings_agg.set_index('team')
    
    def _is_division_game(self, row) -> int:
        """Check if game is within the same division."""
        away_team = self.nfl_teams.get_team(row['away_team'])
        home_team = self.nfl_teams.get_team(row['home_team'])
        
        if away_team and home_team:
            return int(away_team.division == home_team.division)
        return 0
    
    def _is_conference_game(self, row) -> int:
        """Check if game is within the same conference."""
        away_team = self.nfl_teams.get_team(row['away_team'])
        home_team = self.nfl_teams.get_team(row['home_team'])
        
        if away_team and home_team:
            return int(away_team.conference == home_team.conference)
        return 0
    
    def _add_odds_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add betting odds-based features."""
        
        # Spread-based features
        if 'spread' in df.columns and 'spread_favorite' in df.columns:
            # Convert spread to home team perspective
            df['home_spread'] = df.apply(self._calculate_home_spread, axis=1)
            
            # Implied win probability from spread
            df['spread_implied_prob_home'] = df['home_spread'].apply(self._spread_to_probability)
            df['spread_implied_prob_away'] = 1 - df['spread_implied_prob_home']
        
        # Total-based features
        if 'total' in df.columns:
            df['game_total'] = df['total']
            # High/low total indicators
            df['high_total'] = (df['total'] > 47.5).astype(int) if df['total'].notna().any() else 0
            df['low_total'] = (df['total'] < 42.5).astype(int) if df['total'].notna().any() else 0
        
        return df
    
    def _calculate_home_spread(self, row) -> float:
        """Calculate spread from home team perspective."""
        if pd.isna(row.get('spread')) or pd.isna(row.get('spread_favorite')):
            return 0.0
        
        spread = row['spread']
        favorite = row['spread_favorite']
        
        if favorite == 'home':
            return -spread  # Home team favored by spread points
        elif favorite == 'away':
            return spread   # Home team underdog by spread points
        else:  # pick'em
            return 0.0
    
    def _spread_to_probability(self, spread: float) -> float:
        """Convert point spread to implied win probability using logistic function."""
        if pd.isna(spread):
            return 0.5
        
        # Empirical formula: P(home_win) = 1 / (1 + exp(0.25 * spread))
        # This approximates the relationship between spread and win probability
        import math
        try:
            prob = 1 / (1 + math.exp(0.25 * spread))
            return max(0.01, min(0.99, prob))  # Clamp between 1% and 99%
        except (OverflowError, ValueError):
            return 0.5


class TeamRatingsProcessor:
    """Process team ratings from multiple sources."""
    
    def __init__(self):
        """Initialize the processor."""
        pass
    
    def combine_ratings(self, ratings_list: List[pd.DataFrame]) -> pd.DataFrame:
        """
        Combine ratings from multiple sources.
        
        Args:
            ratings_list: List of DataFrames with team ratings
            
        Returns:
            Combined DataFrame with aggregated ratings
        """
        logger.info("Combining team ratings from multiple sources")
        
        if not ratings_list:
            return pd.DataFrame()
        
        # Concatenate all ratings
        combined_df = pd.concat(ratings_list, ignore_index=True)
        
        # Standardize team names
        combined_df['team'] = combined_df['team'].apply(
            lambda x: NFLTeams.get_abbreviation(x) or x
        )
        
        # Aggregate by team and source
        rating_columns = [col for col in combined_df.columns 
                         if 'rating' in col.lower() and col != 'team']
        
        if not rating_columns:
            logger.warning("No rating columns found")
            return combined_df
        
        # Calculate mean ratings across sources for each team
        agg_dict = {col: 'mean' for col in rating_columns}
        agg_dict['source'] = lambda x: ', '.join(x.unique())
        
        aggregated_df = combined_df.groupby('team').agg(agg_dict).reset_index()
        
        return aggregated_df
    
    def normalize_ratings(self, ratings_df: pd.DataFrame) -> pd.DataFrame:
        """
        Normalize ratings to a standard scale.
        
        Args:
            ratings_df: DataFrame with team ratings
            
        Returns:
            DataFrame with normalized ratings
        """
        logger.info("Normalizing team ratings")
        
        if ratings_df.empty:
            return ratings_df
        
        df = ratings_df.copy()
        
        # Find rating columns
        rating_columns = [col for col in df.columns 
                         if 'rating' in col.lower() and col != 'team']
        
        # Normalize each rating column to z-scores
        for col in rating_columns:
            if df[col].notna().sum() > 1:  # Need at least 2 non-null values
                df[f'{col}_normalized'] = (df[col] - df[col].mean()) / df[col].std()
        
        return df
    
    def add_historical_context(self, current_ratings: pd.DataFrame, 
                              historical_ratings: pd.DataFrame) -> pd.DataFrame:
        """
        Add historical context to current ratings.
        
        Args:
            current_ratings: Current season ratings
            historical_ratings: Historical ratings data
            
        Returns:
            DataFrame with historical context features
        """
        logger.info("Adding historical context to team ratings")
        
        if current_ratings.empty or historical_ratings.empty:
            return current_ratings
        
        # Calculate historical averages for each team
        historical_agg = historical_ratings.groupby('team').agg({
            col: 'mean' for col in historical_ratings.columns 
            if 'rating' in col.lower() and col != 'team'
        }).reset_index()
        
        # Add suffix to distinguish from current ratings
        historical_agg = historical_agg.add_suffix('_historical')
        historical_agg = historical_agg.rename(columns={'team_historical': 'team'})
        
        # Merge with current ratings
        enhanced_ratings = current_ratings.merge(
            historical_agg, on='team', how='left'
        )
        
        return enhanced_ratings
