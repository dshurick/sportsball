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
- **Features**: 
  - Command-line arguments for custom file paths
  - Automatically includes all completed 2025 games (not just Week 1)
  - Handles missing odds data gracefully
- **Output**: `data/raw/nfl_merged_2023_2025_complete.csv` (default)
- **Examples**:
  ```bash
  # Default files
  uv run python scripts/create_merged_dataset.py
  
  # Custom files
  uv run python scripts/create_merged_dataset.py \
    --csv-2025 data/raw/updated_2025_schedule.csv \
    --odds data/raw/odds_file1.csv data/raw/odds_file2.csv \
    --output data/raw/custom_merged.csv
  ```

### 3. `train_team_ratings.py`
- **Purpose**: Trains the spread-based team ratings model using historical data
- **Usage**: Run after creating merged dataset to train the predictive model
- **Modes**:
  - **Evaluation mode** (default): Holds out last season for performance testing
  - **Production mode** (`--production`): Uses ALL data for best predictions
- **Output**: Trained model saved to `data/models/`
- **Examples**:
  ```bash
  # Evaluation mode (with test set)
  uv run python scripts/train_team_ratings.py
  
  # Production mode (all data, for eliminator predictions)
  uv run python scripts/train_team_ratings.py --production
  ```

### 4. `generate_updated_2025_predictions.py`
- **Purpose**: Generates win probability predictions for all 2025 NFL games
- **Usage**: Always trains fresh model with all available data for most up-to-date predictions
- **Features**: 
  - **Fresh training**: Retrains model with latest game results every time
  - **All data**: Uses complete dataset including latest completed games
  - **No model loading**: Ensures predictions reflect most recent results
- **Output**: `data/processed/nfl_2025_predictions_updated.csv`

### 5. `optimize_eliminator_picks.py` *(formerly optimize_eliminator_2025_fixed.py)*
- **Purpose**: Optimizes eliminator picks for any season starting from any week
- **Features**:
  - **Multi-season support**: Works for 2025, 2026, or any future season
  - **Mid-season optimization**: Start from any week with previous picks
  - **Flexible input**: Specify used teams via command line
  - **Auto-detection**: Automatically determines start week from used teams
- **Usage Examples**:
  ```bash
  # Season start (no previous picks)
  uv run python scripts/optimize_eliminator_picks.py
  
  # Mid-season with previous picks
  uv run python scripts/optimize_eliminator_picks.py --used-teams "DEN,KC,BUF"
  
  # Specific weeks format
  uv run python scripts/optimize_eliminator_picks.py --used-teams "1:DEN,3:KC,5:BUF"
  
  # Different season
  uv run python scripts/optimize_eliminator_picks.py --season 2026
  ```
- **Output**: Console output with recommended picks and probabilities

## Seamless Workflow

**🚀 Super Easy - Run the complete pipeline with one command:**

```bash
# Complete pipeline (after scraping odds data)
./scripts/run_full_pipeline.sh
```

**Or run scripts individually - no arguments needed!** Each script automatically uses the output from the previous one:

```bash
# 1. Scrape historical odds data (when needed)
uv run python scripts/scrape_odds_history.py --start-year 2023 --end-year 2025

# 2. Merge with schedule/results data (auto-detects completed 2025 games)
uv run python scripts/create_merged_dataset.py

# 3. Train the predictive model (uses merged dataset)
uv run python scripts/train_team_ratings.py

# 4. Generate 2025 predictions (uses trained model)
uv run python scripts/generate_updated_2025_predictions.py

# 5. Get optimal eliminator picks (uses predictions)
uv run python scripts/optimize_eliminator_picks.py
```

**✨ The scripts automatically chain together:**
- `create_merged_dataset.py` uses the latest scraped odds file
- `train_team_ratings.py` uses `data/raw/nfl_merged_2023_2025_complete.csv`
- `generate_updated_2025_predictions.py` trains fresh model with all data for latest predictions
- `optimize_eliminator_picks.py` uses `data/processed/nfl_2025_predictions_updated.csv`

## Weekly Updates

When new games complete, just run the pipeline again:
```bash
# Updates will automatically include new completed games
uv run python scripts/create_merged_dataset.py
uv run python scripts/train_team_ratings.py
uv run python scripts/generate_updated_2025_predictions.py
uv run python scripts/optimize_eliminator_2025_fixed.py
```

## Custom Options

All scripts still accept arguments for customization:
```bash
# Use custom files
uv run python scripts/create_merged_dataset.py --odds my_odds.csv --output my_merged.csv
uv run python scripts/train_team_ratings.py --data-file my_merged.csv
uv run python scripts/optimize_eliminator_2025_fixed.py --week1-pick KC
```

## Dependencies

All scripts require the `sportsball` package to be installed in development mode:
```bash
pip install -e .
```
