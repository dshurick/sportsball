"""Constraint building for eliminator challenge optimization."""

import numpy as np
import pandas as pd
from typing import Dict, List, Tuple, Optional, Set
from dataclasses import dataclass
from loguru import logger

from ..utils.nfl_teams import NFLTeams


@dataclass
class EliminatorConstraints:
    """Container for eliminator challenge constraints."""
    
    # Constraint matrices and vectors
    A_eq: Optional[np.ndarray] = None  # Equality constraint matrix
    b_eq: Optional[np.ndarray] = None  # Equality constraint vector
    A_ub: Optional[np.ndarray] = None  # Inequality constraint matrix  
    b_ub: Optional[np.ndarray] = None  # Inequality constraint vector
    
    # Variable bounds
    bounds: Optional[List[Tuple[float, float]]] = None
    
    # Metadata
    n_games: int = 0
    n_teams: int = 0
    n_weeks: int = 0
    game_indices: Dict[str, int] = None
    team_indices: Dict[str, int] = None
    week_indices: Dict[str, int] = None


class ConstraintBuilder:
    """Build optimization constraints for eliminator challenge."""
    
    def __init__(self):
        """Initialize the constraint builder."""
        self.nfl_teams = NFLTeams()
    
    def build_eliminator_constraints(
        self, 
        games_df: pd.DataFrame,
        picked_teams: Optional[Dict[int, str]] = None,
        remaining_weeks: Optional[List[int]] = None
    ) -> EliminatorConstraints:
        """
        Build constraints for eliminator challenge optimization.
        
        Args:
            games_df: DataFrame with games and win probabilities
            picked_teams: Dict of {week: team} for already picked teams
            remaining_weeks: List of weeks still to pick (if None, infer from data)
            
        Returns:
            EliminatorConstraints object
        """
        logger.info("Building eliminator challenge constraints")
        
        if picked_teams is None:
            picked_teams = {}
        
        # Prepare data
        games_df = self._prepare_games_data(games_df)
        
        # Create indices
        game_indices, team_indices, week_indices = self._create_indices(games_df)
        
        n_games = len(games_df)
        n_teams = len(team_indices)
        n_weeks = len(week_indices)
        
        if remaining_weeks is None:
            remaining_weeks = sorted(week_indices.keys())
        
        logger.info(f"Building constraints for {n_games} games, {n_teams} teams, {n_weeks} weeks")
        
        # Build constraint matrices
        constraints = self._build_constraint_matrices(
            games_df, game_indices, team_indices, week_indices,
            picked_teams, remaining_weeks
        )
        
        # Set variable bounds (binary variables)
        bounds = [(0, 1) for _ in range(n_games)]
        
        return EliminatorConstraints(
            A_eq=constraints['A_eq'],
            b_eq=constraints['b_eq'],
            A_ub=constraints['A_ub'],
            b_ub=constraints['b_ub'],
            bounds=bounds,
            n_games=n_games,
            n_teams=n_teams,
            n_weeks=n_weeks,
            game_indices=game_indices,
            team_indices=team_indices,
            week_indices=week_indices
        )
    
    def _prepare_games_data(self, games_df: pd.DataFrame) -> pd.DataFrame:
        """Prepare games data for optimization."""
        df = games_df.copy()
        
        # Ensure required columns exist
        required_cols = ['season', 'week', 'away_team', 'home_team']
        missing_cols = [col for col in required_cols if col not in df.columns]
        if missing_cols:
            raise ValueError(f"Missing required columns: {missing_cols}")
        
        # Create game identifiers
        df['game_id'] = df.apply(
            lambda row: f"{row['season']}_{row['week']}_{row['away_team']}_{row['home_team']}", 
            axis=1
        )
        
        # Ensure we have win probabilities
        if 'away_win_prob' not in df.columns:
            logger.warning("No away_win_prob column found, using 0.5 as default")
            df['away_win_prob'] = 0.5
        
        if 'home_win_prob' not in df.columns:
            logger.warning("No home_win_prob column found, using 0.5 as default")
            df['home_win_prob'] = 0.5
        
        return df
    
    def _create_indices(self, games_df: pd.DataFrame) -> Tuple[Dict, Dict, Dict]:
        """Create index mappings for games, teams, and weeks."""
        
        # Game indices
        game_indices = {
            game_id: idx for idx, game_id in enumerate(games_df['game_id'])
        }
        
        # Team indices
        all_teams = set(games_df['away_team'].tolist() + games_df['home_team'].tolist())
        team_indices = {team: idx for idx, team in enumerate(sorted(all_teams))}
        
        # Week indices  
        all_weeks = sorted(games_df['week'].unique())
        week_indices = {week: idx for idx, week in enumerate(all_weeks)}
        
        return game_indices, team_indices, week_indices
    
    def _build_constraint_matrices(
        self,
        games_df: pd.DataFrame,
        game_indices: Dict[str, int],
        team_indices: Dict[str, int], 
        week_indices: Dict[str, int],
        picked_teams: Dict[int, str],
        remaining_weeks: List[int]
    ) -> Dict[str, np.ndarray]:
        """Build the constraint matrices for the optimization problem."""
        
        n_games = len(game_indices)
        n_teams = len(team_indices)
        n_weeks = len(week_indices)
        
        constraints = []
        constraint_values = []
        
        # Constraint 1: Exactly one game picked per week
        logger.info("Building weekly selection constraints")
        for week in remaining_weeks:
            if week in picked_teams:
                continue  # Skip weeks where we already picked
                
            week_games = games_df[games_df['week'] == week]
            if len(week_games) == 0:
                continue
                
            constraint_row = np.zeros(n_games)
            for _, game in week_games.iterrows():
                game_idx = game_indices[game['game_id']]
                constraint_row[game_idx] = 1
            
            constraints.append(constraint_row)
            constraint_values.append(1)  # Exactly 1 game per week
        
        # Constraint 2: Each team used at most once
        logger.info("Building team usage constraints")
        for team in team_indices:
            constraint_row = np.zeros(n_games)
            
            # Find all games where this team plays
            team_games = games_df[
                (games_df['away_team'] == team) | (games_df['home_team'] == team)
            ]
            
            for _, game in team_games.iterrows():
                game_idx = game_indices[game['game_id']]
                constraint_row[game_idx] = 1
            
            constraints.append(constraint_row)
            
            # If team already picked, constraint is 0, otherwise <= 1
            if team in picked_teams.values():
                constraint_values.append(0)
            else:
                constraint_values.append(1)
        
        # Convert to matrices
        if constraints:
            A_ub = np.array(constraints)
            b_ub = np.array(constraint_values)
        else:
            A_ub = np.zeros((0, n_games))
            b_ub = np.zeros(0)
        
        # Equality constraints (weekly picks for remaining weeks)
        eq_constraints = []
        eq_values = []
        
        for week in remaining_weeks:
            if week in picked_teams:
                continue
                
            week_games = games_df[games_df['week'] == week]
            if len(week_games) == 0:
                continue
                
            constraint_row = np.zeros(n_games)
            for _, game in week_games.iterrows():
                game_idx = game_indices[game['game_id']]
                constraint_row[game_idx] = 1
            
            eq_constraints.append(constraint_row)
            eq_values.append(1)
        
        if eq_constraints:
            A_eq = np.array(eq_constraints)
            b_eq = np.array(eq_values)
        else:
            A_eq = np.zeros((0, n_games))
            b_eq = np.zeros(0)
        
        return {
            'A_eq': A_eq,
            'b_eq': b_eq, 
            'A_ub': A_ub,
            'b_ub': b_ub
        }
    
    def validate_constraints(self, constraints: EliminatorConstraints) -> bool:
        """
        Validate that constraints are properly formed.
        
        Args:
            constraints: EliminatorConstraints object
            
        Returns:
            True if constraints are valid
        """
        logger.info("Validating optimization constraints")
        
        try:
            # Check matrix dimensions
            if constraints.A_eq is not None:
                if constraints.A_eq.shape[1] != constraints.n_games:
                    logger.error("A_eq matrix has wrong number of columns")
                    return False
                if constraints.b_eq is None or len(constraints.b_eq) != constraints.A_eq.shape[0]:
                    logger.error("b_eq vector has wrong dimensions")
                    return False
            
            if constraints.A_ub is not None:
                if constraints.A_ub.shape[1] != constraints.n_games:
                    logger.error("A_ub matrix has wrong number of columns")
                    return False
                if constraints.b_ub is None or len(constraints.b_ub) != constraints.A_ub.shape[0]:
                    logger.error("b_ub vector has wrong dimensions")
                    return False
            
            # Check bounds
            if constraints.bounds is not None:
                if len(constraints.bounds) != constraints.n_games:
                    logger.error("Bounds have wrong dimensions")
                    return False
            
            logger.info("Constraints validation passed")
            return True
            
        except Exception as e:
            logger.error(f"Constraint validation failed: {e}")
            return False
    
    def get_constraint_summary(self, constraints: EliminatorConstraints) -> Dict:
        """
        Get summary of constraints.
        
        Args:
            constraints: EliminatorConstraints object
            
        Returns:
            Summary dictionary
        """
        summary = {
            'n_games': constraints.n_games,
            'n_teams': constraints.n_teams,
            'n_weeks': constraints.n_weeks,
            'n_equality_constraints': constraints.A_eq.shape[0] if constraints.A_eq is not None else 0,
            'n_inequality_constraints': constraints.A_ub.shape[0] if constraints.A_ub is not None else 0,
            'n_variables': constraints.n_games
        }
        
        return summary
