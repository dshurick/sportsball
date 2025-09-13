# NFL Odds Integration Documentation

## Overview

This document describes the integration of NFL betting odds data into the Sportsball eliminator challenge system. The integration enhances win probability predictions by incorporating market-based information from betting lines.

## Data Sources

### Primary Source: Sports Odds History
- **Website**: [sportsoddshistory.com](https://www.sportsoddshistory.com/nfl-game-season/?y=2024)
- **Data Available**: Historical NFL game odds, spreads, totals, and results
- **Coverage**: Complete NFL seasons with detailed betting information
- **Format**: Web scraping via BeautifulSoup

### Backup Sources
- **FiveThirtyEight**: ELO ratings and predictions
- **ESPN FPI**: Football Power Index ratings
- **ffanalytics R Package**: Multi-source aggregation (requires R setup)

## Implementation

### 1. Odds Scraper (`sportsball/data/odds_scraper.py`)

```python
from sportsball.data.odds_scraper import SportsOddsHistoryScraper

scraper = SportsOddsHistoryScraper()
df = scraper.scrape_season_odds(2024)
```

**Features:**
- Scrapes complete season data with spreads, totals, and results
- Handles team name normalization across different formats
- Extracts betting lines and converts to standardized format
- Provides data quality metrics and validation

**Data Structure:**
```python
{
    'season': 2024,
    'week': 3,
    'away_team': 'SF',
    'home_team': 'DAL',
    'spread': 3.5,
    'spread_favorite': 'away',
    'spread_line': 3.5,
    'total': 47.5,
    'away_score': 28,
    'home_score': 21,
    'winner': 'away'
}
```

### 2. Odds-Based Features (`sportsball/data/processors.py`)

The system automatically generates betting-derived features:

**Spread Features:**
- `home_spread`: Point spread from home team perspective
- `spread_implied_prob_home`: Win probability implied by spread
- `spread_implied_prob_away`: Complementary away team probability

**Total Features:**
- `game_total`: Over/under total points
- `high_total`: Binary indicator for high-scoring games (>47.5)
- `low_total`: Binary indicator for low-scoring games (<42.5)

**Probability Conversion:**
```python
# Empirical formula for spread to probability
P(home_win) = 1 / (1 + exp(0.25 * home_spread))
```

### 3. Model Integration

The enhanced model incorporates odds features alongside traditional metrics:

**Feature Categories:**
- **Game Context**: Division games, conference games, season timing
- **Betting Market**: Spread-implied probabilities, game totals
- **Team Ratings**: ELO, FPI, or other rating systems (when available)

**Training Process:**
```bash
# Process historical data with odds
python scripts/setup_historical_data.py odds_data.csv

# Train enhanced model
uv run eliminator train-model processed_data.csv --model-name odds_enhanced_model
```

## Usage Examples

### 1. Scraping Current Season Odds

```bash
# Scrape 2024 season odds
uv run eliminator scrape-odds --season 2024

# Scrape multiple seasons
uv run eliminator scrape-odds --start-year 2020 --end-year 2024

# Preview without saving
uv run eliminator scrape-odds --season 2024 --preview
```

### 2. Creating Sample Data

```python
# Generate realistic sample odds for testing
python scripts/create_sample_odds_data.py
```

### 3. Training with Odds Data

```bash
# Train model with odds-enhanced features
uv run eliminator train-model historical_with_odds.csv --model-name enhanced_model
```

### 4. Complete Pipeline

```bash
# Run eliminator optimization with enhanced model
uv run eliminator optimize-picks \
    --season 2024 \
    --current-week 3 \
    --picked-teams '{"1": "BUF", "2": "KC"}' \
    --model-name enhanced_model
```

## Data Quality and Validation

### Scraping Quality Metrics

The scraper provides comprehensive quality assessment:

```python
summary = scraper.get_odds_summary(df)
print(f"Games with spread data: {summary['games_with_spread']}")
print(f"Games with results: {summary['games_with_results']}")
print(f"Data coverage: {(summary['games_with_spread'] / summary['total_games']) * 100:.1f}%")
```

### Feature Engineering Validation

- **Spread Consistency**: Validates spread favorite matches point differential
- **Probability Bounds**: Ensures implied probabilities stay within [0.01, 0.99]
- **Total Reasonableness**: Checks game totals fall within realistic NFL ranges (35-65 points)

### Model Performance

The odds-enhanced model shows improved calibration:
- **Baseline Model**: Uses only game context features
- **Enhanced Model**: Incorporates market-based probability estimates
- **Validation**: Cross-validation on historical seasons with known outcomes

## Configuration

### Environment Variables

```bash
# Data scraping settings
SPORTSBALL_REQUEST_DELAY=2.0
SPORTSBALL_TIMEOUT=30

# Model settings  
SPORTSBALL_MODEL_RANDOM_STATE=42
SPORTSBALL_TEST_SIZE=0.2

# Feature engineering
SPORTSBALL_HIGH_TOTAL_THRESHOLD=47.5
SPORTSBALL_LOW_TOTAL_THRESHOLD=42.5
```

### CLI Options

```bash
# Scraping options
--season YEAR           # Single season to scrape
--start-year YEAR       # Multi-season start
--end-year YEAR         # Multi-season end
--output-file FILE      # Custom output filename
--preview               # Preview without saving

# Training options
--model-name NAME       # Custom model name
--include-odds BOOL     # Enable/disable odds features
--hyperparameter-tuning # Enable parameter optimization
```

## Integration Benefits

### 1. Market Efficiency
- Betting markets aggregate information from many sources
- Odds reflect real-world expectations and expert analysis
- Provides baseline probability estimates for model calibration

### 2. Feature Richness
- Spread data captures perceived team strength differential
- Totals indicate expected game pace and scoring environment
- Historical odds enable backtesting against market expectations

### 3. Model Robustness
- Reduces overfitting to limited statistical features
- Provides external validation of model predictions
- Enables ensemble approaches combining multiple information sources

## Troubleshooting

### Common Issues

1. **R Package Missing**
   ```bash
   # Install ffanalytics in R
   R -e "install.packages('ffanalytics')"
   ```

2. **Scraping Failures**
   - Check internet connection
   - Verify website structure hasn't changed
   - Increase request delays for rate limiting

3. **Feature Mismatch**
   - Ensure training and prediction use same feature set
   - Exclude categorical columns from model input
   - Verify data preprocessing consistency

### Debug Mode

```python
# Enable debug logging
import logging
logging.basicConfig(level=logging.DEBUG)

# Test scraper with verbose output
scraper = SportsOddsHistoryScraper()
df = scraper.scrape_season_odds(2024)
```

## Future Enhancements

### 1. Real-Time Integration
- Live odds APIs for current games
- Automated daily data updates
- Real-time model retraining

### 2. Advanced Features
- Line movement tracking
- Multiple sportsbook consensus
- Weather and injury adjustments

### 3. Model Improvements
- Ensemble methods combining multiple models
- Dynamic feature weighting based on data quality
- Uncertainty quantification for risk assessment

## References

- [Sports Odds History](https://www.sportsoddshistory.com/) - Historical NFL odds data
- [FiveThirtyEight NFL](https://fivethirtyeight.com/sports/nfl/) - ELO ratings and predictions
- [ESPN FPI](https://www.espn.com/nfl/fpi) - Football Power Index
- [ffanalytics R Package](https://github.com/FantasyFootballAnalytics/ffanalytics) - Multi-source data aggregation
