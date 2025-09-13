"""Eliminator challenge optimization using constrained optimization."""

import numpy as np
import pandas as pd
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass
from loguru import logger

try:
    import cvxpy as cp
    CVXPY_AVAILABLE = True
except ImportError:
    CVXPY_AVAILABLE = False
    logger.warning("CVXPY not available, falling back to scipy")

try:
    from scipy.optimize import linprog
    SCIPY_AVAILABLE = True
except ImportError:
    SCIPY_AVAILABLE = False
    logger.error("Neither CVXPY nor scipy available for optimization")

from .constraints import ConstraintBuilder, EliminatorConstraints
from ..utils.config import config


@dataclass
class OptimizationResult:
    """Result of eliminator optimization."""
    
    success: bool
    optimal_picks: Dict[int, Tuple[str, str]]  # {week: (team, opponent)}
    expected_survival_prob: float
    weekly_probs: Dict[int, float]  # {week: win_probability}
    solver_status: str
    solver_time: float
    metadata: Dict[str, Any]


class EliminatorOptimizer:
    """Optimizer for NFL eliminator challenge."""
    
    def __init__(self, solver: str = "auto"):
        """
        Initialize the optimizer.
        
        Args:
            solver: Optimization solver to use ("cvxpy", "scipy", "auto")
        """
        self.solver = solver
        self.constraint_builder = ConstraintBuilder()
        
        # Determine available solver
        if solver == "auto":
            if SCIPY_AVAILABLE:
                self.solver = "scipy"
            elif CVXPY_AVAILABLE:
                self.solver = "cvxpy"
            else:
                raise RuntimeError("No optimization solver available")
        
        logger.info(f"Using {self.solver} solver for optimization")
    
    def optimize_eliminator_picks(
        self,
        games_df: pd.DataFrame,
        picked_teams: Optional[Dict[int, str]] = None,
        remaining_weeks: Optional[List[int]] = None,
        risk_adjustment: float = 0.0
    ) -> OptimizationResult:
        """
        Optimize eliminator challenge picks to maximize survival probability.
        
        Args:
            games_df: DataFrame with games and win probabilities
            picked_teams: Dict of {week: team} for already picked teams
            remaining_weeks: List of weeks still to pick
            risk_adjustment: Risk adjustment factor (0 = risk neutral, >0 = risk averse)
            
        Returns:
            OptimizationResult with optimal picks
        """
        logger.info("Starting eliminator challenge optimization")
        
        if picked_teams is None:
            picked_teams = {}
        
        # Build constraints
        constraints = self.constraint_builder.build_eliminator_constraints(
            games_df, picked_teams, remaining_weeks
        )
        
        # Validate constraints
        if not self.constraint_builder.validate_constraints(constraints):
            return OptimizationResult(
                success=False,
                optimal_picks={},
                expected_survival_prob=0.0,
                weekly_probs={},
                solver_status="Invalid constraints",
                solver_time=0.0,
                metadata={}
            )
        
        # Build objective function
        objective_coeffs = self._build_objective(games_df, risk_adjustment)
        
        # Solve optimization problem
        if self.solver == "cvxpy":
            result = self._solve_with_cvxpy(constraints, objective_coeffs, games_df)
        else:
            result = self._solve_with_scipy(constraints, objective_coeffs, games_df)
        
        return result
    
    def _build_objective(self, games_df: pd.DataFrame, risk_adjustment: float = 0.0) -> np.ndarray:
        """
        Build objective function coefficients.
        
        Args:
            games_df: DataFrame with games and win probabilities
            risk_adjustment: Risk adjustment factor
            
        Returns:
            Objective coefficients array
        """
        logger.info("Building objective function")
        
        # Create objective coefficients based on win probabilities
        objective_coeffs = []
        
        for _, game in games_df.iterrows():
            # Determine which team we would pick and their win probability
            away_prob = game.get('away_win_prob', 0.5)
            home_prob = game.get('home_win_prob', 0.5)
            
            # Pick the team with higher win probability
            win_prob = max(away_prob, home_prob)
            
            # Apply risk adjustment (penalize lower probabilities more)
            if risk_adjustment > 0:
                # Use log probability to penalize risk
                if win_prob > 0:
                    adjusted_prob = np.log(win_prob) - risk_adjustment * (1 - win_prob)
                else:
                    adjusted_prob = -np.inf
            else:
                adjusted_prob = win_prob
            
            objective_coeffs.append(adjusted_prob)
        
        return np.array(objective_coeffs)
    
    def _solve_with_cvxpy(
        self, 
        constraints: EliminatorConstraints, 
        objective_coeffs: np.ndarray,
        games_df: pd.DataFrame
    ) -> OptimizationResult:
        """Solve using CVXPY."""
        import time
        
        logger.info("Solving with CVXPY")
        start_time = time.time()
        
        try:
            # Create decision variables (binary for each game)
            x = cp.Variable(constraints.n_games, boolean=True)
            
            # Create objective (maximize expected log survival probability)
            objective = cp.Maximize(objective_coeffs @ x)
            
            # Create constraints
            constraint_list = []
            
            # Equality constraints
            if constraints.A_eq is not None and constraints.A_eq.shape[0] > 0:
                constraint_list.append(constraints.A_eq @ x == constraints.b_eq)
            
            # Inequality constraints  
            if constraints.A_ub is not None and constraints.A_ub.shape[0] > 0:
                constraint_list.append(constraints.A_ub @ x <= constraints.b_ub)
            
            # Variable bounds
            constraint_list.append(x >= 0)
            constraint_list.append(x <= 1)
            
            # Create and solve problem
            problem = cp.Problem(objective, constraint_list)
            
            # Try different solvers in order of preference
            solvers_to_try = [cp.ECOS_BB, cp.SCIP, cp.CBC, cp.GLPK_MI]
            solved = False
            
            for solver in solvers_to_try:
                try:
                    problem.solve(solver=solver, verbose=False)
                    if problem.status == cp.OPTIMAL:
                        solved = True
                        break
                except Exception:
                    continue
            
            if not solved:
                # Try without specifying solver
                problem.solve(verbose=False)
            
            solve_time = time.time() - start_time
            
            if problem.status == cp.OPTIMAL:
                # Extract solution
                solution = x.value
                optimal_picks, weekly_probs, survival_prob = self._extract_solution(
                    solution, games_df, constraints
                )
                
                return OptimizationResult(
                    success=True,
                    optimal_picks=optimal_picks,
                    expected_survival_prob=survival_prob,
                    weekly_probs=weekly_probs,
                    solver_status=problem.status,
                    solver_time=solve_time,
                    metadata={
                        'objective_value': problem.value,
                        'solver': 'CVXPY',
                        'n_variables': constraints.n_games
                    }
                )
            else:
                logger.error(f"Optimization failed with status: {problem.status}")
                return OptimizationResult(
                    success=False,
                    optimal_picks={},
                    expected_survival_prob=0.0,
                    weekly_probs={},
                    solver_status=problem.status,
                    solver_time=solve_time,
                    metadata={}
                )
                
        except Exception as e:
            logger.error(f"CVXPY optimization failed: {e}")
            return OptimizationResult(
                success=False,
                optimal_picks={},
                expected_survival_prob=0.0,
                weekly_probs={},
                solver_status=f"Error: {e}",
                solver_time=time.time() - start_time,
                metadata={}
            )
    
    def _solve_with_scipy(
        self,
        constraints: EliminatorConstraints,
        objective_coeffs: np.ndarray,
        games_df: pd.DataFrame
    ) -> OptimizationResult:
        """Solve using scipy.optimize.linprog."""
        import time
        
        logger.info("Solving with scipy")
        start_time = time.time()
        
        try:
            # scipy minimizes, so negate coefficients
            c = -objective_coeffs
            
            # Prepare constraints for scipy
            A_ub = constraints.A_ub
            b_ub = constraints.b_ub
            A_eq = constraints.A_eq
            b_eq = constraints.b_eq
            bounds = constraints.bounds
            
            # Solve
            result = linprog(
                c=c,
                A_ub=A_ub,
                b_ub=b_ub,
                A_eq=A_eq,
                b_eq=b_eq,
                bounds=bounds,
                method='highs',
                options={'presolve': True}
            )
            
            solve_time = time.time() - start_time
            
            if result.success:
                # Extract solution
                solution = result.x
                optimal_picks, weekly_probs, survival_prob = self._extract_solution(
                    solution, games_df, constraints
                )
                
                return OptimizationResult(
                    success=True,
                    optimal_picks=optimal_picks,
                    expected_survival_prob=survival_prob,
                    weekly_probs=weekly_probs,
                    solver_status="Optimal",
                    solver_time=solve_time,
                    metadata={
                        'objective_value': -result.fun,
                        'solver': 'scipy',
                        'n_variables': constraints.n_games
                    }
                )
            else:
                logger.error(f"Scipy optimization failed: {result.message}")
                return OptimizationResult(
                    success=False,
                    optimal_picks={},
                    expected_survival_prob=0.0,
                    weekly_probs={},
                    solver_status=result.message,
                    solver_time=solve_time,
                    metadata={}
                )
                
        except Exception as e:
            logger.error(f"Scipy optimization failed: {e}")
            return OptimizationResult(
                success=False,
                optimal_picks={},
                expected_survival_prob=0.0,
                weekly_probs={},
                solver_status=f"Error: {e}",
                solver_time=time.time() - start_time,
                metadata={}
            )
    
    def _extract_solution(
        self,
        solution: np.ndarray,
        games_df: pd.DataFrame,
        constraints: EliminatorConstraints
    ) -> Tuple[Dict[int, Tuple[str, str]], Dict[int, float], float]:
        """
        Extract optimal picks from solution.
        
        Args:
            solution: Solution vector from optimization
            games_df: Games DataFrame
            constraints: Constraint object
            
        Returns:
            Tuple of (optimal_picks, weekly_probs, survival_prob)
        """
        logger.info("Extracting solution")
        
        # Find selected games (solution > 0.5 for binary variables)
        selected_indices = np.where(solution > 0.5)[0]
        
        optimal_picks = {}
        weekly_probs = {}
        
        for idx in selected_indices:
            game = games_df.iloc[idx]
            week = game['week']
            
            # Determine which team to pick (higher win probability)
            away_prob = game.get('away_win_prob', 0.5)
            home_prob = game.get('home_win_prob', 0.5)
            
            if away_prob > home_prob:
                picked_team = game['away_team']
                opponent = game['home_team']
                win_prob = away_prob
            else:
                picked_team = game['home_team']
                opponent = game['away_team']
                win_prob = home_prob
            
            optimal_picks[week] = (picked_team, opponent)
            weekly_probs[week] = win_prob
        
        # Calculate overall survival probability
        if weekly_probs:
            survival_prob = np.prod(list(weekly_probs.values()))
        else:
            survival_prob = 0.0
        
        logger.info(f"Extracted {len(optimal_picks)} picks with survival probability {survival_prob:.3f}")
        
        return optimal_picks, weekly_probs, survival_prob
    
    def simulate_season_outcomes(
        self,
        picks: Dict[int, Tuple[str, str]],
        weekly_probs: Dict[int, float],
        n_simulations: int = 10000
    ) -> Dict[str, Any]:
        """
        Simulate season outcomes based on picks.
        
        Args:
            picks: Optimal picks {week: (team, opponent)}
            weekly_probs: Win probabilities {week: probability}
            n_simulations: Number of simulations to run
            
        Returns:
            Simulation results
        """
        logger.info(f"Running {n_simulations} season simulations")
        
        weeks = sorted(picks.keys())
        probs = [weekly_probs[week] for week in weeks]
        
        # Run simulations
        outcomes = []
        survival_weeks = []
        
        for _ in range(n_simulations):
            # Simulate each week
            survived = True
            weeks_survived = 0
            
            for i, prob in enumerate(probs):
                if np.random.random() < prob:
                    weeks_survived += 1
                else:
                    survived = False
                    break
            
            outcomes.append(survived)
            survival_weeks.append(weeks_survived)
        
        # Calculate statistics
        survival_rate = np.mean(outcomes)
        avg_weeks_survived = np.mean(survival_weeks)
        
        # Survival by week
        week_survival = {}
        for week in weeks:
            week_idx = weeks.index(week)
            survived_to_week = np.sum(np.array(survival_weeks) > week_idx)
            week_survival[week] = survived_to_week / n_simulations
        
        results = {
            'survival_rate': survival_rate,
            'avg_weeks_survived': avg_weeks_survived,
            'week_survival_rates': week_survival,
            'n_simulations': n_simulations,
            'total_weeks': len(weeks)
        }
        
        logger.info(f"Simulation results: {survival_rate:.1%} survival rate, "
                   f"{avg_weeks_survived:.1f} average weeks survived")
        
        return results
