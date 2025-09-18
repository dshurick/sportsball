# Scripts Directory

This directory contains the essential scripts for the NFL Eliminator Challenge system.

## Core Scripts

### 1. `scrape_odds_history.py`
- **Purpose**: Scrapes historical NFL odds data from sportsoddshistory.com
- **Usage**: Run to collect betting spreads and totals for historical seasons
- **Output**: CSV files with odds data

### 2. `create_merged_dataset.py`
- **Purpose**: Merges scraped odds data with official NFL schedule/results CSV files
- **Usage**: Run after scraping odds to create complete training dataset
- **Output**: `data/raw/nfl_merged_2023_2025_complete.csv`

### 3. `train_team_ratings.py`
- **Purpose**: Trains the spread-based team ratings model using historical data
- **Usage**: Run after creating merged dataset to train the predictive model
- **Output**: Trained model saved to `data/models/`

### 4. `generate_updated_2025_predictions.py`
- **Purpose**: Generates win probability predictions for all 2025 NFL games
- **Usage**: Run after training model to get predictions including Week 1 results
- **Output**: `data/processed/nfl_2025_predictions_updated.csv`

### 5. `optimize_eliminator_2025_fixed.py`
- **Purpose**: Optimizes eliminator picks for Weeks 2-18 using linear programming
- **Usage**: Run after generating predictions to get optimal pick strategy
- **Output**: Console output with recommended picks and probabilities

## Typical Workflow

1. Run `scrape_odds_history.py` to collect historical odds data
2. Run `create_merged_dataset.py` to merge with schedule/results data
3. Run `train_team_ratings.py` to train the predictive model
4. Run `generate_updated_2025_predictions.py` to get 2025 predictions
5. Run `optimize_eliminator_2025_fixed.py` to get optimal eliminator picks

## Dependencies

All scripts require the `sportsball` package to be installed in development mode:
```bash
pip install -e .
```
