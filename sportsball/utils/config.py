"""Configuration management for sportsball package."""

import os
from pathlib import Path
from typing import Dict, Any, Optional
from pydantic import Field
from pydantic_settings import BaseSettings


class Config(BaseSettings):
    """Application configuration."""
    
    # Paths
    project_root: Path = Field(default_factory=lambda: Path(__file__).parent.parent.parent)
    data_dir: Path = Field(default_factory=lambda: Path(__file__).parent.parent.parent / "data")
    raw_data_dir: Path = Field(default_factory=lambda: Path(__file__).parent.parent.parent / "data" / "raw")
    interim_data_dir: Path = Field(default_factory=lambda: Path(__file__).parent.parent.parent / "data" / "interim")
    processed_data_dir: Path = Field(default_factory=lambda: Path(__file__).parent.parent.parent / "data" / "processed")
    models_dir: Path = Field(default_factory=lambda: Path(__file__).parent.parent.parent / "models")
    
    # NFL Season Configuration
    current_season: int = Field(default=2024)
    current_week: int = Field(default=1)
    
    # Data Sources
    ffanalytics_sources: list[str] = Field(default=[
        "CBS", "ESPN", "FantasyPros", "FantasySharks", 
        "FFToday", "NumberFire", "NFL", "RTSports"
    ])
    
    # API Configuration
    request_delay: float = Field(default=2.0, description="Delay between requests in seconds")
    max_retries: int = Field(default=3)
    timeout: int = Field(default=30)
    
    # Model Configuration
    model_random_state: int = Field(default=42)
    test_size: float = Field(default=0.2)
    cv_folds: int = Field(default=5)
    
    # Optimization Configuration
    optimization_solver: str = Field(default="ECOS")
    max_optimization_time: int = Field(default=300)  # seconds
    
    # Logging
    log_level: str = Field(default="INFO")
    log_file: Optional[str] = Field(default=None)
    
    class Config:
        env_file = ".env"
        env_prefix = "SPORTSBALL_"
    
    def __post_init__(self):
        """Create directories if they don't exist."""
        for path_attr in ["data_dir", "raw_data_dir", "interim_data_dir", "processed_data_dir", "models_dir"]:
            path = getattr(self, path_attr)
            path.mkdir(parents=True, exist_ok=True)


# Global config instance
config = Config()
