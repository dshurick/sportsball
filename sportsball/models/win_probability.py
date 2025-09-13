"""Win probability model for NFL games."""

import pandas as pd
import numpy as np
from typing import Dict, List, Optional, Tuple, Any
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split, cross_val_score, GridSearchCV
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.metrics import classification_report, confusion_matrix
from loguru import logger

from .base import BaseModel
from ..utils.config import config


class WinProbabilityModel(BaseModel):
    """Logistic regression model for predicting NFL game win probabilities."""
    
    def __init__(self, model_name: str = "win_probability_model"):
        """
        Initialize the win probability model.
        
        Args:
            model_name: Name of the model for saving/loading
        """
        super().__init__(model_name)
        self.scaler = StandardScaler()
        self.pipeline = None
        
        # Default feature columns for win probability
        self.default_features = [
            'home_field_advantage',
            'division_game',
            'conference_game',
            'early_season',
            'late_season'
        ]
    
    def fit(self, X: pd.DataFrame, y: pd.Series, 
            feature_selection: bool = True,
            hyperparameter_tuning: bool = True,
            **kwargs) -> 'WinProbabilityModel':
        """
        Fit the win probability model.
        
        Args:
            X: Feature matrix
            y: Target variable (1 for away team win, 0 for home team win)
            feature_selection: Whether to perform automatic feature selection
            hyperparameter_tuning: Whether to tune hyperparameters
            **kwargs: Additional arguments for LogisticRegression
            
        Returns:
            Fitted model instance
        """
        logger.info("Fitting win probability model")
        
        # Prepare features
        X_processed = self._prepare_features(X, fit_scaler=True)
        
        if feature_selection:
            X_processed = self._select_features(X_processed, y)
        
        # Set up the model pipeline
        if hyperparameter_tuning:
            model = self._tune_hyperparameters(X_processed, y)
        else:
            model = LogisticRegression(
                random_state=config.model_random_state,
                max_iter=1000,
                **kwargs
            )
        
        # Create pipeline with scaling
        self.pipeline = Pipeline([
            ('scaler', StandardScaler()),
            ('classifier', model)
        ])
        
        # Fit the pipeline
        self.pipeline.fit(X_processed, y)
        self.model = self.pipeline
        self.is_fitted = True
        
        # Store metadata
        self.model_metadata = {
            'n_samples': len(X_processed),
            'n_features': len(self.feature_columns),
            'feature_columns': self.feature_columns,
            'target_distribution': y.value_counts().to_dict()
        }
        
        # Evaluate on training data (skip for now to avoid feature mismatch)
        try:
            train_metrics = self.evaluate(X, y)
            self.model_metadata['train_metrics'] = train_metrics
        except Exception as e:
            logger.warning(f"Could not evaluate training metrics: {e}")
            self.model_metadata['train_metrics'] = {'accuracy': 0.0}
        
        logger.info(f"Model fitted with {len(self.feature_columns)} features")
        logger.info(f"Training accuracy: {self.model_metadata['train_metrics']['accuracy']:.3f}")
        
        return self
    
    def predict(self, X: pd.DataFrame) -> np.ndarray:
        """
        Predict game outcomes.
        
        Args:
            X: Feature matrix
            
        Returns:
            Binary predictions (1 for away team win, 0 for home team win)
        """
        X_processed = self._prepare_features(X, fit_scaler=False)
        X_validated = self._validate_features(X_processed)
        
        return self.pipeline.predict(X_validated)
    
    def predict_proba(self, X: pd.DataFrame) -> np.ndarray:
        """
        Predict win probabilities.
        
        Args:
            X: Feature matrix
            
        Returns:
            Probability predictions [P(home_win), P(away_win)]
        """
        X_processed = self._prepare_features(X, fit_scaler=False)
        X_validated = self._validate_features(X_processed)
        
        return self.pipeline.predict_proba(X_validated)
    
    def predict_game_probabilities(self, games_df: pd.DataFrame) -> pd.DataFrame:
        """
        Predict win probabilities for games with team context.
        
        Args:
            games_df: DataFrame with game information
            
        Returns:
            DataFrame with win probabilities for each team
        """
        logger.info(f"Predicting probabilities for {len(games_df)} games")
        
        # Make predictions
        probabilities = self.predict_proba(games_df)
        
        # Create results DataFrame
        results = games_df[['season', 'week', 'away_team', 'home_team']].copy()
        results['home_win_prob'] = probabilities[:, 0]
        results['away_win_prob'] = probabilities[:, 1]
        
        # Add confidence metrics
        results['prediction_confidence'] = np.abs(probabilities[:, 1] - 0.5) * 2
        results['predicted_winner'] = np.where(
            probabilities[:, 1] > 0.5, 
            results['away_team'], 
            results['home_team']
        )
        
        return results
    
    def _prepare_features(self, X: pd.DataFrame, fit_scaler: bool = False) -> pd.DataFrame:
        """
        Prepare features for modeling.
        
        Args:
            X: Input features
            fit_scaler: Whether to fit the scaler (True for training)
            
        Returns:
            Processed feature matrix
        """
        X_processed = X.copy()
        
        # Identify available features
        available_features = []
        
        # Add default features if they exist
        for feature in self.default_features:
            if feature in X_processed.columns:
                available_features.append(feature)
        
        # Add rating differential features
        rating_diff_cols = [col for col in X_processed.columns if 'differential' in col]
        available_features.extend(rating_diff_cols)
        
        # Add individual rating features
        rating_cols = [col for col in X_processed.columns 
                      if 'rating' in col.lower() and 'differential' not in col]
        available_features.extend(rating_cols)
        
        # Store feature columns
        if fit_scaler or not self.feature_columns:
            self.feature_columns = available_features
        
        # Select features
        if self.feature_columns:
            missing_features = set(self.feature_columns) - set(X_processed.columns)
            if missing_features:
                logger.warning(f"Missing features: {missing_features}")
                # Use only available features
                self.feature_columns = [f for f in self.feature_columns if f in X_processed.columns]
        
        return X_processed
    
    def _select_features(self, X: pd.DataFrame, y: pd.Series) -> pd.DataFrame:
        """
        Perform automatic feature selection.
        
        Args:
            X: Feature matrix
            y: Target variable
            
        Returns:
            Feature matrix with selected features
        """
        from sklearn.feature_selection import SelectKBest, f_classif
        
        logger.info("Performing feature selection")
        
        # Use available features
        X_features = X[self.feature_columns]
        
        # Remove features with no variance
        variance_mask = X_features.var() > 0
        selected_features = X_features.columns[variance_mask].tolist()
        
        if len(selected_features) > 10:  # Only do selection if we have many features
            # Use SelectKBest to choose top features
            k = min(10, len(selected_features))
            selector = SelectKBest(score_func=f_classif, k=k)
            selector.fit(X_features[selected_features], y)
            
            # Get selected feature names
            selected_mask = selector.get_support()
            selected_features = [f for i, f in enumerate(selected_features) if selected_mask[i]]
        
        self.feature_columns = selected_features
        logger.info(f"Selected {len(selected_features)} features: {selected_features}")
        
        return X
    
    def _tune_hyperparameters(self, X: pd.DataFrame, y: pd.Series) -> LogisticRegression:
        """
        Tune hyperparameters using grid search.
        
        Args:
            X: Feature matrix
            y: Target variable
            
        Returns:
            Best LogisticRegression model
        """
        logger.info("Tuning hyperparameters")
        
        # Define parameter grid
        param_grid = [
            {
                'C': [0.01, 0.1, 1.0, 10.0],
                'penalty': ['l1', 'l2'],
                'solver': ['liblinear']
            },
            {
                'C': [0.01, 0.1, 1.0, 10.0],
                'penalty': ['l2'],
                'solver': ['saga']
            }
        ]
        
        # Create base model
        base_model = LogisticRegression(
            random_state=config.model_random_state,
            max_iter=1000
        )
        
        # Perform grid search
        grid_search = GridSearchCV(
            base_model,
            param_grid,
            cv=min(5, len(y) // 10),  # Adjust CV folds based on data size
            scoring='roc_auc',
            n_jobs=-1
        )
        
        X_features = X[self.feature_columns]
        grid_search.fit(X_features, y)
        
        logger.info(f"Best parameters: {grid_search.best_params_}")
        logger.info(f"Best CV score: {grid_search.best_score_:.3f}")
        
        return grid_search.best_estimator_
    
    def cross_validate(self, X: pd.DataFrame, y: pd.Series, cv: int = 5) -> Dict[str, float]:
        """
        Perform cross-validation.
        
        Args:
            X: Feature matrix
            y: Target variable
            cv: Number of cross-validation folds
            
        Returns:
            Cross-validation scores
        """
        logger.info(f"Performing {cv}-fold cross-validation")
        
        if not self.is_fitted:
            raise ValueError("Model must be fitted before cross-validation")
        
        X_processed = self._prepare_features(X, fit_scaler=False)
        X_features = X_processed[self.feature_columns]
        
        # Perform cross-validation
        cv_scores = cross_val_score(
            self.pipeline, X_features, y, 
            cv=cv, scoring='roc_auc'
        )
        
        results = {
            'mean_cv_score': cv_scores.mean(),
            'std_cv_score': cv_scores.std(),
            'cv_scores': cv_scores.tolist()
        }
        
        logger.info(f"CV Score: {results['mean_cv_score']:.3f} (+/- {results['std_cv_score']*2:.3f})")
        
        return results
    
    def get_model_summary(self) -> Dict[str, Any]:
        """
        Get comprehensive model summary.
        
        Returns:
            Dictionary with model information
        """
        if not self.is_fitted:
            return {"error": "Model not fitted"}
        
        summary = {
            "model_name": self.model_name,
            "model_type": "Logistic Regression",
            "n_features": len(self.feature_columns),
            "features": self.feature_columns,
            "metadata": self.model_metadata
        }
        
        # Add feature importance
        feature_importance = self.get_feature_importance()
        if feature_importance is not None:
            summary["feature_importance"] = feature_importance.to_dict('records')
        
        return summary
