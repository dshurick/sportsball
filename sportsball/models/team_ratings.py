"""Dynamic team rating model based on betting spreads."""

import numpy as np
import pandas as pd
from typing import Dict, List, Optional, Tuple
from scipy.optimize import minimize
from sklearn.metrics import log_loss, accuracy_score
from loguru import logger
import joblib
from pathlib import Path
from datetime import datetime, timedelta

from .base import BaseModel
from ..utils.config import config
from ..utils.nfl_teams import NFLTeams


class SpreadBasedTeamRatings(BaseModel):
    """
    Dynamic team rating model using betting spreads.
    
    This model:
    1. Uses historical game spreads with exponential time-based weighting
    2. Weights recent games more heavily (up to 52 weeks back)
    3. Forecasts game probabilities using logistic regression on rating differences
    4. Optimizes home field advantage and rating decay parameters
    """
    
    def __init__(self, 
                 home_field_advantage: float = 3.0,
                 rating_decay: float = 0.1,
                 max_weeks_back: int = 52):
        """
        Initialize the team rating model.
        
        Args:
            home_field_advantage: Points advantage for home team
            rating_decay: How much to decay previous season ratings
            max_weeks_back: Maximum weeks to look back for game weighting
        """
        super().__init__("spread_based_team_ratings")
        self.home_field_advantage = home_field_advantage
        self.rating_decay = rating_decay
        self.max_weeks_back = max_weeks_back
        self.nfl_teams = NFLTeams()
        
        # Model parameters (to be optimized)
        self.team_ratings = {}  # {season: {team: rating}}
        
        # Model metadata
        self.model_metadata = {
            'max_weeks_back': max_weeks_back,
            'home_field_advantage': home_field_advantage,
            'rating_decay': rating_decay,
            'training_seasons': [],
            'accuracy_by_week': {},
            'log_loss_by_week': {}
        }
    
    def fit(self, games_df: pd.DataFrame, optimize_params: bool = True) -> None:
        """
        Fit the team rating model to historical game data.
        
        Args:
            games_df: DataFrame with columns [season, week, away_team, home_team, 
                     spread, away_score, home_score, winner]
            optimize_params: Whether to optimize hyperparameters
        """
        logger.info("Fitting spread-based team rating model")
        
        # Prepare data
        games_df = games_df.copy()
        games_df['home_win'] = (games_df['winner'] == 'home').astype(int)
        
        # Get unique seasons for training
        seasons = sorted(games_df['season'].unique())
        self.model_metadata['training_seasons'] = seasons
        
        if optimize_params:
            logger.info("Optimizing model parameters")
            self._optimize_parameters(games_df)
        
        # Calculate team ratings for each season using all historical data
        for season in seasons:
            logger.info(f"Calculating team ratings for {season} season")
            
            # Get all games up to the end of this season for time-weighted calculation
            historical_games = games_df[games_df['season'] <= season]
            
            # Get current season teams
            season_games = games_df[games_df['season'] == season]
            
            self.team_ratings[season] = self._calculate_season_ratings(historical_games, season)
        
        # Evaluate model performance
        self._evaluate_model(games_df)
        
        logger.info(f"Model fitted on {len(games_df)} games across {len(seasons)} seasons")
        logger.info(f"Overall accuracy: {self.model_metadata.get('overall_accuracy', 0):.3f}")
    
    def predict_game_probability(self, 
                                away_team: str, 
                                home_team: str, 
                                season: int, 
                                week: int) -> float:
        """
        Predict the probability that the away team wins.
        
        Args:
            away_team: Away team abbreviation
            home_team: Home team abbreviation  
            season: Season year
            week: Week number
            
        Returns:
            Probability that away team wins (0-1)
        """
        # Get current team ratings
        away_rating = self._get_team_rating(away_team, season, week)
        home_rating = self._get_team_rating(home_team, season, week)
        
        # Calculate rating difference (away - home - home_field_advantage)
        rating_diff = away_rating - home_rating - self.home_field_advantage
        
        # Convert to probability via logistic function
        prob_away_wins = 1 / (1 + np.exp(-rating_diff))
        
        return prob_away_wins
    
    def predict(self, X: pd.DataFrame) -> np.ndarray:
        """
        Predict game outcomes (required by BaseModel).
        
        Args:
            X: DataFrame with columns [away_team, home_team, season, week]
            
        Returns:
            Array of predicted outcomes (0=home wins, 1=away wins)
        """
        probabilities = self.predict_proba(X)
        return (probabilities[:, 1] > 0.5).astype(int)
    
    def predict_proba(self, X: pd.DataFrame) -> np.ndarray:
        """
        Predict game win probabilities (required by BaseModel).
        
        Args:
            X: DataFrame with columns [away_team, home_team, season, week]
            
        Returns:
            Array of shape (n_games, 2) with [home_win_prob, away_win_prob]
        """
        away_probs = []
        
        for _, row in X.iterrows():
            prob_away = self.predict_game_probability(
                row['away_team'], row['home_team'], row['season'], row['week']
            )
            away_probs.append(prob_away)
        
        away_probs = np.array(away_probs)
        home_probs = 1 - away_probs
        
        return np.column_stack([home_probs, away_probs])
    
    def predict_games(self, games_df: pd.DataFrame) -> pd.DataFrame:
        """
        Predict probabilities for multiple games.
        
        Args:
            games_df: DataFrame with games to predict
            
        Returns:
            DataFrame with added probability columns
        """
        games_df = games_df.copy()
        
        games_df['away_win_prob'] = games_df.apply(
            lambda row: self.predict_game_probability(
                row['away_team'], row['home_team'], row['season'], row['week']
            ), axis=1
        )
        
        # Add home win probability for compatibility
        games_df['home_win_prob'] = 1 - games_df['away_win_prob']
        
        return games_df
    
    def predict_game_probabilities(self, games_df: pd.DataFrame) -> pd.DataFrame:
        """
        Predict game probabilities (compatible with CLI interface).
        
        Args:
            games_df: DataFrame with games to predict
            
        Returns:
            DataFrame with prediction columns
        """
        return self.predict_games(games_df)
    
    def get_team_ratings(self, season: int, week: Optional[int] = None) -> Dict[str, float]:
        """
        Get current team ratings for a season/week.
        
        Args:
            season: Season year
            week: Week number (None for end-of-season ratings)
            
        Returns:
            Dictionary of team ratings
        """
        if season not in self.team_ratings:
            logger.warning(f"No ratings available for {season} season")
            return {}
        
        if week is None:
            return self.team_ratings[season].copy()
        
        # For specific week, we'd need to recalculate with data up to that week
        # For now, return season ratings
        return self.team_ratings[season].copy()
    
    def _calculate_season_ratings(self, historical_games: pd.DataFrame, season: int) -> Dict[str, float]:
        """Calculate team ratings for a specific season using all available historical data."""
        
        # Get all teams from current season
        current_season_games = historical_games[historical_games['season'] == season]
        teams = list(set(current_season_games['away_team'].unique()) | set(current_season_games['home_team'].unique()))
        
        # Use all historical games for time-weighted rating calculation
        ratings = self._solve_team_ratings(historical_games, teams, {})
        
        return ratings
    
    def _solve_team_ratings(self, 
                           games_df: pd.DataFrame, 
                           teams: List[str], 
                           prev_ratings: Dict[str, float]) -> Dict[str, float]:
        """
        Solve for team ratings using spread data and constraints.
        
        The approach:
        1. For each game, spread ≈ home_rating - away_rating + home_field_advantage
        2. Solve system of equations with constraint that ratings sum to zero
        3. Weight recent games more heavily
        """
        
        if len(games_df) == 0:
            return {team: prev_ratings.get(team, 0.0) for team in teams}
        
        # Create team index mapping
        team_to_idx = {team: i for i, team in enumerate(teams)}
        n_teams = len(teams)
        
        # Build system of equations: A * ratings = b
        equations = []
        targets = []
        weights = []
        
        # Get the most recent game date to calculate relative weights
        games_df['game_date'] = pd.to_datetime(games_df['game_date'])
        most_recent_date = games_df['game_date'].max()
        
        for _, game in games_df.iterrows():
            away_idx = team_to_idx.get(game['away_team'])
            home_idx = team_to_idx.get(game['home_team'])
            
            # Skip games with teams not in current season
            if away_idx is None or home_idx is None:
                continue
                
            # Equation: home_rating - away_rating = spread - home_field_advantage
            equation = np.zeros(n_teams)
            equation[home_idx] = 1
            equation[away_idx] = -1
            
            target = game['spread'] - self.home_field_advantage
            
            # Calculate exponential time-based weight
            game_date = pd.to_datetime(game['game_date'])
            weeks_ago = (most_recent_date - game_date).days / 7.0
            
            # Skip games older than max_weeks_back
            if weeks_ago > self.max_weeks_back:
                continue
                
            # Exponential decay: weight = exp(-weeks_ago / decay_constant)
            # At 52 weeks, weight should be close to 0, so decay_constant ≈ 52/5 ≈ 10
            decay_constant = self.max_weeks_back / 5.0
            weight = np.exp(-weeks_ago / decay_constant)
            
            equations.append(equation)
            targets.append(target)
            weights.append(weight)
        
        # Convert to matrices
        A = np.array(equations)
        b = np.array(targets)
        W = np.diag(weights)
        
        # Add constraint that ratings sum to zero
        constraint_eq = np.ones((1, n_teams))
        constraint_target = np.array([0.0])
        
        # Combine equations and constraint
        A_constrained = np.vstack([A, constraint_eq])
        b_constrained = np.hstack([b, constraint_target])
        
        # Solve weighted least squares with constraint
        try:
            # Use normal equations: (A^T W A) x = A^T W b
            AtWA = A_constrained.T @ A_constrained
            AtWb = A_constrained.T @ b_constrained
            
            # Add regularization for stability
            regularization = 0.01 * np.eye(n_teams)
            AtWA += regularization
            
            ratings_array = np.linalg.solve(AtWA, AtWb)
            
        except np.linalg.LinAlgError:
            logger.warning("Could not solve rating system, using previous ratings")
            ratings_array = np.array([prev_ratings.get(team, 0.0) for team in teams])
        
        # Ensure ratings sum to zero
        ratings_array -= np.mean(ratings_array)
        
        # No blending - ratings are determined purely by time-weighted historical data
        
        return {team: ratings_array[i] for i, team in enumerate(teams)}
    
    def _get_team_rating(self, team: str, season: int, week: int) -> float:
        """Get team rating for specific season/week."""
        if season not in self.team_ratings:
            # Use previous season ratings with decay if available
            prev_season = season - 1
            if prev_season in self.team_ratings:
                prev_rating = self.team_ratings[prev_season].get(team, 0.0)
                # Apply decay for new season
                return prev_rating * (1 - self.rating_decay)
            return 0.0
        
        return self.team_ratings[season].get(team, 0.0)
    
    def _optimize_parameters(self, games_df: pd.DataFrame) -> None:
        """Optimize model hyperparameters."""
        
        def objective(params):
            """Objective function to minimize (negative log likelihood)."""
            self.home_field_advantage = params[0]
            self.rating_decay = params[1]
            
            # Calculate predictions and compute log loss
            total_loss = 0
            total_games = 0
            
            for season in sorted(games_df['season'].unique()):
                # Get all games up to the end of this season for time-weighted calculation
                historical_games = games_df[games_df['season'] <= season]
                
                # Calculate ratings for this season
                season_ratings = self._calculate_season_ratings(historical_games, season)
                self.team_ratings[season] = season_ratings
                
                # Get current season games for evaluation
                season_games = games_df[games_df['season'] == season]
                
                # Predict games from week 6 onward for evaluation
                eval_games = season_games[season_games['week'] >= 6]
                
                if len(eval_games) > 0:
                    predictions = []
                    actuals = []
                    
                    for _, game in eval_games.iterrows():
                        prob = self.predict_game_probability(
                            game['away_team'], game['home_team'], 
                            game['season'], game['week']
                        )
                        predictions.append(prob)
                        actuals.append(1 - game['home_win'])  # Convert to away_win
                    
                    if len(predictions) > 0:
                        loss = log_loss(actuals, predictions)
                        total_loss += loss * len(predictions)
                        total_games += len(predictions)
            
            return total_loss / max(total_games, 1)
        
        # Optimize parameters
        initial_params = [self.home_field_advantage, self.rating_decay]
        bounds = [(0, 6), (0, 0.5)]
        
        result = minimize(objective, initial_params, bounds=bounds, method='L-BFGS-B')
        
        if result.success:
            self.home_field_advantage = result.x[0]
            self.rating_decay = result.x[1]
            
            logger.info(f"Optimized parameters:")
            logger.info(f"  Home field advantage: {self.home_field_advantage:.2f}")
            logger.info(f"  Rating decay: {self.rating_decay:.3f}")
        else:
            logger.warning("Parameter optimization failed, using default values")
    
    def _evaluate_model(self, games_df: pd.DataFrame) -> None:
        """Evaluate model performance by week."""
        
        accuracy_by_week = {}
        logloss_by_week = {}
        
        for week in range(6, 18):  # Evaluate weeks 6-17
            week_games = games_df[games_df['week'] == week]
            
            if len(week_games) == 0:
                continue
            
            predictions = []
            actuals = []
            
            for _, game in week_games.iterrows():
                prob = self.predict_game_probability(
                    game['away_team'], game['home_team'],
                    game['season'], game['week']
                )
                predictions.append(prob)
                actuals.append(1 - game['home_win'])  # Convert to away_win
            
            if len(predictions) > 0:
                accuracy = accuracy_score(actuals, [p > 0.5 for p in predictions])
                logloss = log_loss(actuals, predictions)
                
                accuracy_by_week[week] = accuracy
                logloss_by_week[week] = logloss
        
        self.model_metadata['accuracy_by_week'] = accuracy_by_week
        self.model_metadata['log_loss_by_week'] = logloss_by_week
        
        if accuracy_by_week:
            overall_accuracy = np.mean(list(accuracy_by_week.values()))
            self.model_metadata['overall_accuracy'] = overall_accuracy
    
    def save_model(self, filepath: Optional[str] = None) -> None:
        """Save the trained model."""
        if filepath is None:
            filepath = config.models_dir / "team_ratings_model.joblib"
        
        model_data = {
            'team_ratings': self.team_ratings,
            'home_field_advantage': self.home_field_advantage,
            'rating_decay': self.rating_decay,
            'max_weeks_back': self.max_weeks_back,
            'model_metadata': self.model_metadata
        }
        
        joblib.dump(model_data, filepath)
        logger.info(f"Team ratings model saved to {filepath}")
    
    def load_model(self, filepath: Optional[str] = None) -> None:
        """Load a trained model."""
        if filepath is None:
            filepath = config.models_dir / "team_ratings_model.joblib"
        
        if not Path(filepath).exists():
            raise FileNotFoundError(f"Model file not found: {filepath}")
        
        model_data = joblib.load(filepath)
        
        self.team_ratings = model_data['team_ratings']
        self.home_field_advantage = model_data['home_field_advantage']
        self.rating_decay = model_data['rating_decay']
        self.max_weeks_back = model_data.get('max_weeks_back', 52)  # Backward compatibility
        self.model_metadata = model_data['model_metadata']
        
        logger.info(f"Team ratings model loaded from {filepath}")
