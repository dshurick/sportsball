# 🏈 Sportsball: NFL Eliminator Challenge Optimizer

A comprehensive Python package for NFL data analysis and eliminator challenge optimization. This modernized system helps you make optimal weekly team selections to maximize your survival probability in NFL eliminator pools.

## 🚀 Features

- **Data Scraping**: Automated collection of NFL team ratings and game data from multiple sources
- **Win Probability Modeling**: GLM logistic regression model for predicting game outcomes
- **Constrained Optimization**: Mathematical optimization to maximize eliminator survival probability
- **R Integration**: Optional integration with the `ffanalytics` R package for enhanced data collection
- **Modern Python**: Built with Python 3.12+, uv package management, and type hints
- **CLI Interface**: Easy-to-use command line tools for the complete workflow

## 📋 The Eliminator Challenge Problem

In an NFL eliminator challenge, you must:
1. Pick one team each week that you think will win their game
2. You can only use each team **once** throughout the season
3. If your picked team loses, you're eliminated
4. Goal: Survive as many weeks as possible (ideally the entire season)

This package solves this as a constrained optimization problem, maximizing your overall survival probability while respecting the "use each team at most once" constraint.

## 🛠 Installation

### Prerequisites

- Python 3.12+
- [uv](https://github.com/astral-sh/uv) for package management
- R (optional, for enhanced data scraping with ffanalytics)

### Install with uv

```bash
# Clone the repository
git clone <your-repo-url>
cd sportsball

# Install with uv (includes all dependencies)
uv sync

# Install with optimization solvers
uv sync --extra optimization

# Install with R integration
uv sync --extra r-integration

# Install development dependencies
uv sync --extra dev
```

### Install R Dependencies (Optional)

If you want to use the ffanalytics R package for enhanced data scraping:

```r
install.packages("ffanalytics")
install.packages(c("tidyverse", "rvest", "jsonlite"))
```

## 🎯 Quick Start

### 1. Set up historical data

If you have historical game data (like your Google Sheets), process it first:

```bash
# Process your historical data
python scripts/setup_historical_data.py your_historical_data.csv

# This creates a processed file ready for model training
```

### 2. Train the win probability model

```bash
# Train model on historical data
uv run eliminator train-model data/processed/historical_games_processed.csv
```

### 3. Optimize your eliminator picks

```bash
# Optimize picks for current season
uv run eliminator optimize-picks --season 2024 --current-week 3 --picked-teams '{"1": "BUF", "2": "KC"}'
```

### 4. Scrape fresh data (optional)

```bash
# Scrape current NFL data
uv run eliminator scrape-data --season 2024 --week 3
```

## 📖 Detailed Usage

### Command Line Interface

The package provides a comprehensive CLI through the `eliminator` command:

```bash
# See all available commands
uv run eliminator --help

# Scrape NFL data
uv run eliminator scrape-data --season 2024 --week 3

# Train win probability model
uv run eliminator train-model historical_data.csv --model-name my_model

# Optimize eliminator picks
uv run eliminator optimize-picks \
    --season 2024 \
    --current-week 3 \
    --picked-teams '{"1": "BUF", "2": "KC"}' \
    --risk-adjustment 0.1

# Analyze existing picks
uv run eliminator analyze-picks my_picks.csv
```

### Python API

You can also use the package programmatically:

```python
from sportsball import NFLDataScraper, WinProbabilityModel, EliminatorOptimizer

# Scrape data
scraper = NFLDataScraper()
ratings_df = scraper.scrape_team_ratings(2024, 3)
schedule_df = scraper.scrape_game_schedule(2024)

# Load trained model
model = WinProbabilityModel()
model.load_model()

# Predict game probabilities
predictions_df = model.predict_game_probabilities(games_df)

# Optimize picks
optimizer = EliminatorOptimizer()
result = optimizer.optimize_eliminator_picks(
    predictions_df,
    picked_teams={1: "BUF", 2: "KC"},
    risk_adjustment=0.1
)

print(f"Optimal picks: {result.optimal_picks}")
print(f"Survival probability: {result.expected_survival_prob:.1%}")
```

### Example Pipeline

See `scripts/example_pipeline.py` for a complete end-to-end example.

## 🏗 Architecture

### Core Components

1. **Data Layer** (`sportsball.data`)
   - `NFLDataScraper`: Collects data from multiple sources
   - `GameDataProcessor`: Processes game data for modeling
   - `TeamRatingsProcessor`: Handles team rating aggregation

2. **Models** (`sportsball.models`)
   - `WinProbabilityModel`: Logistic regression for game outcome prediction
   - `BaseModel`: Abstract base class for all models

3. **Optimization** (`sportsball.optimization`)
   - `EliminatorOptimizer`: Constrained optimization solver
   - `ConstraintBuilder`: Builds mathematical constraints

4. **Utilities** (`sportsball.utils`)
   - `Config`: Configuration management
   - `NFLTeams`: Team information and mappings
   - Logging and other utilities

### Data Sources

The system can collect data from:
- **ffanalytics R package**: CBS, ESPN, FantasyPros, FantasySharks, FFToday, NumberFire, NFL, RTSports
- **FiveThirtyEight**: ELO ratings and predictions
- **ESPN API**: Game schedules and team information
- **Your historical data**: Google Sheets or CSV files

### Optimization Approach

The eliminator challenge is formulated as a Mixed Integer Linear Program (MILP):

**Objective**: Maximize log(survival probability) = Σ log(P(win_i)) for selected games

**Constraints**:
- Exactly one game selected per week
- Each team used at most once across all weeks
- Binary decision variables (0 or 1 for each game)

**Solvers**: CVXPY (preferred) or scipy.optimize

## 📊 Model Features

The win probability model uses these features:

- **Team Ratings**: ELO, FPI, or other rating systems
- **Home Field Advantage**: Standard 3-point advantage
- **Divisional Games**: Teams know each other well
- **Conference Games**: Familiarity effects
- **Season Timing**: Early season (weeks 1-4) vs late season (weeks 15+)
- **Rating Differentials**: Difference between team ratings

## 🔧 Configuration

Create a `.env` file to customize settings:

```bash
# Data sources
SPORTSBALL_FFANALYTICS_SOURCES=["CBS", "ESPN", "FantasyPros", "NumberFire"]
SPORTSBALL_REQUEST_DELAY=2.0

# Model settings
SPORTSBALL_MODEL_RANDOM_STATE=42
SPORTSBALL_TEST_SIZE=0.2

# Optimization
SPORTSBALL_OPTIMIZATION_SOLVER=ECOS
SPORTSBALL_MAX_OPTIMIZATION_TIME=300

# Logging
SPORTSBALL_LOG_LEVEL=INFO
SPORTSBALL_LOG_FILE=logs/sportsball.log
```

## 📈 Example Output

```
🏆 OPTIMAL ELIMINATOR PICKS
═══════════════════════════════════════════════════════════
Week │ Pick │ vs  │ Win Prob │ Confidence
─────┼──────┼─────┼──────────┼───────────
  3  │ SF   │ DAL │   72.3%  │   44.6%
  4  │ BUF  │ MIA │   68.1%  │   36.2%
  5  │ KC   │ LV  │   75.4%  │   50.8%

📊 Summary
Expected Survival Probability: 38.7%
Solver: CVXPY
Solve Time: 0.15s

🎲 Simulation Results (10,000 simulations)
Survival Rate: 39.2%
Average Weeks Survived: 12.3
```

## 🧪 Development

### Setup Development Environment

```bash
# Install with development dependencies
uv sync --extra dev

# Install pre-commit hooks
pre-commit install

# Run tests
pytest

# Run linting
black sportsball/
isort sportsball/
flake8 sportsball/
mypy sportsball/
```

### Project Structure

```
sportsball/
├── sportsball/           # Main package
│   ├── data/            # Data collection and processing
│   ├── models/          # ML models
│   ├── optimization/    # Optimization algorithms
│   ├── utils/           # Utilities and configuration
│   └── cli/             # Command line interface
├── scripts/             # Example scripts
├── tests/               # Test suite
├── data/                # Data storage
│   ├── raw/            # Raw scraped data
│   ├── interim/        # Intermediate processed data
│   └── processed/      # Final processed data
└── models/              # Saved model files
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Add tests for new functionality
5. Run the test suite (`pytest`)
6. Commit your changes (`git commit -m 'Add amazing feature'`)
7. Push to the branch (`git push origin feature/amazing-feature`)
8. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- [ffanalytics R package](https://github.com/FantasyFootballAnalytics/ffanalytics) for NFL data scraping capabilities
- FiveThirtyEight for ELO ratings and predictions
- ESPN for game schedules and team information
- The fantasy football analytics community for inspiration and data sources

## 📞 Support

- 📧 Create an issue on GitHub for bug reports or feature requests
- 💬 Check existing issues for common problems and solutions
- 📖 Read the documentation for detailed usage examples

---

**Good luck with your eliminator challenge! 🏆**