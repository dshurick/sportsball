"""
Sportsball: NFL sports analysis and eliminator challenge optimization.

A comprehensive package for NFL data analysis, win probability modeling,
and eliminator challenge optimization.
"""

__version__ = "0.1.0"
__author__ = "Devon Shurick"

from .models import WinProbabilityModel
from .optimization import EliminatorOptimizer
from .data import NFLDataScraper

__all__ = ["WinProbabilityModel", "EliminatorOptimizer", "NFLDataScraper"]
