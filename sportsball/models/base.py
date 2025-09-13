"""Base model class for NFL analysis."""

from abc import ABC, abstractmethod
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union
import pandas as pd
import numpy as np
import joblib
from loguru import logger

from ..utils.config import config


class BaseModel(ABC):
    """Base class for NFL prediction models."""
    
    def __init__(self, model_name: str):
        """
        Initialize the base model.
        
        Args:
            model_name: Name of the model for saving/loading
        """
        self.model_name = model_name
        self.model = None
        self.feature_columns = []
        self.is_fitted = False
        self.model_metadata = {}
    
    @abstractmethod
    def fit(self, X: pd.DataFrame, y: pd.Series, **kwargs) -> 'BaseModel':
        """
        Fit the model to training data.
        
        Args:
            X: Feature matrix
            y: Target variable
            **kwargs: Additional arguments
            
        Returns:
            Fitted model instance
        """
        pass
    
    @abstractmethod
    def predict(self, X: pd.DataFrame) -> np.ndarray:
        """
        Make predictions on new data.
        
        Args:
            X: Feature matrix
            
        Returns:
            Predictions array
        """
        pass
    
    @abstractmethod
    def predict_proba(self, X: pd.DataFrame) -> np.ndarray:
        """
        Predict class probabilities.
        
        Args:
            X: Feature matrix
            
        Returns:
            Probability predictions array
        """
        pass
    
    def save_model(self, filepath: Optional[Path] = None) -> Path:
        """
        Save the fitted model to disk.
        
        Args:
            filepath: Optional custom filepath
            
        Returns:
            Path where model was saved
        """
        if not self.is_fitted:
            raise ValueError("Model must be fitted before saving")
        
        if filepath is None:
            filepath = config.models_dir / f"{self.model_name}.joblib"
        
        model_data = {
            'model': self.model,
            'feature_columns': self.feature_columns,
            'model_metadata': self.model_metadata,
            'model_name': self.model_name
        }
        
        joblib.dump(model_data, filepath)
        logger.info(f"Model saved to {filepath}")
        
        return filepath
    
    def load_model(self, filepath: Optional[Path] = None) -> 'BaseModel':
        """
        Load a fitted model from disk.
        
        Args:
            filepath: Optional custom filepath
            
        Returns:
            Loaded model instance
        """
        if filepath is None:
            filepath = config.models_dir / f"{self.model_name}.joblib"
        
        if not filepath.exists():
            raise FileNotFoundError(f"Model file not found: {filepath}")
        
        model_data = joblib.load(filepath)
        
        self.model = model_data['model']
        self.feature_columns = model_data['feature_columns']
        self.model_metadata = model_data['model_metadata']
        self.is_fitted = True
        
        logger.info(f"Model loaded from {filepath}")
        
        return self
    
    def get_feature_importance(self) -> Optional[pd.DataFrame]:
        """
        Get feature importance if available.
        
        Returns:
            DataFrame with feature importance or None
        """
        if not self.is_fitted:
            logger.warning("Model not fitted, cannot get feature importance")
            return None
        
        if hasattr(self.model, 'coef_'):
            # For linear models
            importance = np.abs(self.model.coef_).flatten()
            return pd.DataFrame({
                'feature': self.feature_columns,
                'importance': importance
            }).sort_values('importance', ascending=False)
        
        elif hasattr(self.model, 'feature_importances_'):
            # For tree-based models
            return pd.DataFrame({
                'feature': self.feature_columns,
                'importance': self.model.feature_importances_
            }).sort_values('importance', ascending=False)
        
        else:
            logger.warning("Model does not support feature importance")
            return None
    
    def _validate_features(self, X: pd.DataFrame) -> pd.DataFrame:
        """
        Validate and prepare features for prediction.
        
        Args:
            X: Input features
            
        Returns:
            Validated feature matrix
        """
        if not self.is_fitted:
            raise ValueError("Model must be fitted before making predictions")
        
        # Check for required features
        missing_features = set(self.feature_columns) - set(X.columns)
        if missing_features:
            raise ValueError(f"Missing required features: {missing_features}")
        
        # Select and order features correctly
        X_validated = X[self.feature_columns].copy()
        
        # Handle missing values
        X_validated = X_validated.fillna(0)
        
        return X_validated
    
    def evaluate(self, X: pd.DataFrame, y: pd.Series) -> Dict[str, float]:
        """
        Evaluate model performance.
        
        Args:
            X: Feature matrix
            y: True labels
            
        Returns:
            Dictionary of evaluation metrics
        """
        from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, roc_auc_score
        
        predictions = self.predict(X)
        probabilities = self.predict_proba(X)
        
        metrics = {
            'accuracy': accuracy_score(y, predictions),
            'precision': precision_score(y, predictions, average='weighted', zero_division=0),
            'recall': recall_score(y, predictions, average='weighted', zero_division=0),
            'f1': f1_score(y, predictions, average='weighted', zero_division=0)
        }
        
        # Add AUC if probabilities are available
        if probabilities.ndim > 1 and probabilities.shape[1] > 1:
            try:
                metrics['roc_auc'] = roc_auc_score(y, probabilities[:, 1])
            except ValueError:
                pass  # Skip AUC if not applicable
        
        return metrics
