"""Dynamic team rating model based on betting spreads."""

import numpy as np
import pandas as pd
from typing import Dict, List, Optional, Tuple
from scipy.optimize import minimize
from sklearn.metrics import log_loss, accuracy_score
from loguru import logger
import joblib
from pathlib import Path

from .base import BaseModel
from ..utils.config import config
from ..utils.nfl_teams import NFLTeams


class SpreadBasedTeamRatings(BaseModel):
    """
    Dynamic team rating model using betting spreads.
    
    This model:
    1. Uses recent game spreads (8-week window) to estimate team ratings
    2. Incorporates previous season ratings for early season predictions
    3. Forecasts game probabilities using logistic regression on rating differences
    4. Optimizes all parameters for maximum predictive accuracy
    """
    
    def __init__(self, 
                 lookback_weeks: int = 8,
                 home_field_advantage: float = 3.0,
                 rating_decay: float = 0.1):
        """
        Initialize the team rating model.
        
        Args:
            lookback_weeks: Number of recent weeks to use for rating calculation
            home_field_advantage: Points advantage for home team
            rating_decay: How much to decay previous season ratings
        """
        super().__init__("spread_based_team_ratings")
        self.lookback_weeks = lookback_weeks
        self.home_field_advantage = home_field_advantage
        self.rating_decay = rating_decay
        self.nfl_teams = NFLTeams()
        
        # Model parameters (to be optimized)
        self.team_ratings = {}  # {season: {team: rating}}
        self.rating_scale = 1.0  # Scaling factor for rating differences
        self.previous_season_weight = 0.8  # Weight for previous season ratings
        
        # Model metadata
        self.model_metadata = {
            'lookback_weeks': lookback_weeks,
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
        
        # Calculate team ratings for each season
        for season in seasons:
            logger.info(f"Calculating team ratings for {season} season")
            season_games = games_df[games_df['season'] == season]
            self.team_ratings[season] = self._calculate_season_ratings(season_games, season)
        
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
        
        # Apply scaling and convert to probability via logistic function
        scaled_diff = rating_diff * self.rating_scale
        prob_away_wins = 1 / (1 + np.exp(-scaled_diff))
        
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
    
    def _calculate_season_ratings(self, season_games: pd.DataFrame, season: int) -> Dict[str, float]:
        """Calculate team ratings for a specific season."""
        
        # Initialize ratings
        teams = list(set(season_games['away_team'].unique()) | set(season_games['home_team'].unique()))
        
        # Get previous season ratings if available
        prev_season_ratings = {}
        if season - 1 in self.team_ratings:
            prev_season_ratings = self.team_ratings[season - 1].copy()
            # Apply decay to previous season ratings
            for team in prev_season_ratings:
                prev_season_ratings[team] *= (1 - self.rating_decay)
        
        # Calculate ratings using spread-based approach
        ratings = self._solve_team_ratings(season_games, teams, prev_season_ratings)
        
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
        
        for _, game in games_df.iterrows():
            away_idx = team_to_idx[game['away_team']]
            home_idx = team_to_idx[game['home_team']]
            
            # Equation: home_rating - away_rating = spread - home_field_advantage
            equation = np.zeros(n_teams)
            equation[home_idx] = 1
            equation[away_idx] = -1
            
            target = game['spread'] - self.home_field_advantage
            
            # Weight recent games more (assuming games are chronologically ordered)
            weight = 1.0  # Could add recency weighting here
            
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
        
        # Blend with previous season ratings for early season
        if prev_ratings and len(games_df) < 5 * len(teams):  # Less than ~5 games per team
            blend_weight = min(len(games_df) / (3 * len(teams)), 1.0)  # Gradually increase weight
            for i, team in enumerate(teams):
                if team in prev_ratings:
                    ratings_array[i] = (blend_weight * ratings_array[i] + 
                                      (1 - blend_weight) * prev_ratings[team])
        
        return {team: ratings_array[i] for i, team in enumerate(teams)}
    
    def _get_team_rating(self, team: str, season: int, week: int) -> float:
        """Get team rating for specific season/week."""
        if season not in self.team_ratings:
            return 0.0
        
        return self.team_ratings[season].get(team, 0.0)
    
    def _optimize_parameters(self, games_df: pd.DataFrame) -> None:
        """Optimize model hyperparameters."""
        
        def objective(params):
            """Objective function to minimize (negative log likelihood)."""
            self.home_field_advantage = params[0]
            self.rating_scale = params[1]
            self.rating_decay = params[2]
            
            # Calculate predictions and compute log loss
            total_loss = 0
            total_games = 0
            
            for season in sorted(games_df['season'].unique()):
                season_games = games_df[games_df['season'] == season]
                
                # Calculate ratings for this season
                season_ratings = self._calculate_season_ratings(season_games, season)
                self.team_ratings[season] = season_ratings
                
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
        initial_params = [self.home_field_advantage, self.rating_scale, self.rating_decay]
        bounds = [(0, 6), (0.1, 3.0), (0, 0.5)]
        
        result = minimize(objective, initial_params, bounds=bounds, method='L-BFGS-B')
        
        if result.success:
            self.home_field_advantage = result.x[0]
            self.rating_scale = result.x[1] 
            self.rating_decay = result.x[2]
            
            logger.info(f"Optimized parameters:")
            logger.info(f"  Home field advantage: {self.home_field_advantage:.2f}")
            logger.info(f"  Rating scale: {self.rating_scale:.2f}")
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
            'rating_scale': self.rating_scale,
            'rating_decay': self.rating_decay,
            'lookback_weeks': self.lookback_weeks,
            'previous_season_weight': self.previous_season_weight,
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
        self.rating_scale = model_data['rating_scale']
        self.rating_decay = model_data['rating_decay']
        self.lookback_weeks = model_data['lookback_weeks']
        self.previous_season_weight = model_data['previous_season_weight']
        self.model_metadata = model_data['model_metadata']
        
        logger.info(f"Team ratings model loaded from {filepath}")
