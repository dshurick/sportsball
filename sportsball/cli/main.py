"""Main CLI interface for sportsball eliminator challenge."""

import typer
import pandas as pd
from pathlib import Path
from typing import Optional, Dict, List
from rich.console import Console
from rich.table import Table
from rich.panel import Panel
from rich import print as rprint
from loguru import logger

from ..data import NFLDataScraper, GameDataProcessor, TeamRatingsProcessor
from ..models import WinProbabilityModel
from ..optimization import EliminatorOptimizer
from ..utils.config import config

app = typer.Typer(help="NFL Eliminator Challenge Optimizer")
console = Console()


@app.command()
def scrape_data(
    season: int = typer.Option(2024, help="NFL season year"),
    week: int = typer.Option(1, help="Current week number"),
    output_dir: Optional[str] = typer.Option(None, help="Output directory for data")
):
    """Scrape NFL data from various sources."""
    console.print(f"[bold blue]Scraping NFL data for {season} season, week {week}[/bold blue]")
    
    scraper = NFLDataScraper()
    
    # Scrape team ratings
    ratings_df = scraper.scrape_team_ratings(season, week)
    if not ratings_df.empty:
        ratings_file = scraper.save_data(ratings_df, f"team_ratings_{season}_week_{week}.csv")
        console.print(f"✅ Team ratings saved to {ratings_file}")
    else:
        console.print("❌ Failed to scrape team ratings")
    
    # Scrape game schedule
    schedule_df = scraper.scrape_game_schedule(season)
    if not schedule_df.empty:
        schedule_file = scraper.save_data(schedule_df, f"game_schedule_{season}.csv")
        console.print(f"✅ Game schedule saved to {schedule_file}")
    else:
        console.print("❌ Failed to scrape game schedule")


@app.command()
def train_model(
    historical_data: str = typer.Argument(..., help="Path to historical games CSV"),
    model_name: str = typer.Option("win_probability_model", help="Name for the model"),
    test_size: float = typer.Option(0.2, help="Test set size"),
):
    """Train win probability model on historical data."""
    console.print(f"[bold blue]Training win probability model[/bold blue]")
    
    # Load historical data
    try:
        historical_df = pd.read_csv(historical_data)
        console.print(f"✅ Loaded {len(historical_df)} historical games")
    except Exception as e:
        console.print(f"❌ Failed to load historical data: {e}")
        raise typer.Exit(1)
    
    # Process data
    processor = GameDataProcessor()
    processed_df = processor.process_historical_games(historical_df)
    
    if processed_df.empty:
        console.print("❌ No data available after processing")
        raise typer.Exit(1)
    
    # Prepare features and target
    feature_cols = [col for col in processed_df.columns 
                   if col not in ['away_team_win', 'season', 'week', 'away_team', 'home_team', 'game_date']]
    
    X = processed_df[feature_cols]
    y = processed_df['away_team_win']
    
    console.print(f"📊 Training with {len(feature_cols)} features on {len(X)} games")
    
    # Train model
    model = WinProbabilityModel(model_name)
    model.fit(X, y)
    
    # Save model
    model_path = model.save_model()
    console.print(f"✅ Model saved to {model_path}")
    
    # Show model summary
    summary = model.get_model_summary()
    console.print(f"📈 Training accuracy: {summary['metadata']['train_metrics']['accuracy']:.3f}")


@app.command()
def optimize_picks(
    season: int = typer.Option(2024, help="NFL season year"),
    current_week: int = typer.Option(1, help="Current week number"),
    picked_teams: Optional[str] = typer.Option(None, help="JSON string of already picked teams {week: team}"),
    model_name: str = typer.Option("win_probability_model", help="Name of trained model"),
    risk_adjustment: float = typer.Option(0.0, help="Risk adjustment factor (0=neutral, >0=risk averse)"),
):
    """Optimize eliminator challenge picks for remaining weeks."""
    console.print(f"[bold blue]Optimizing eliminator picks for {season} season[/bold blue]")
    
    # Parse picked teams
    picked_teams_dict = {}
    if picked_teams:
        import json
        try:
            picked_teams_dict = json.loads(picked_teams)
            console.print(f"📋 Already picked: {picked_teams_dict}")
        except json.JSONDecodeError:
            console.print("❌ Invalid picked_teams JSON format")
            raise typer.Exit(1)
    
    # Load model
    try:
        model = WinProbabilityModel(model_name)
        model.load_model()
        console.print(f"✅ Loaded model: {model_name}")
    except FileNotFoundError:
        console.print(f"❌ Model not found: {model_name}")
        console.print("💡 Train a model first using: eliminator train-model")
        raise typer.Exit(1)
    
    # Load or scrape current data
    try:
        # Try to load existing data
        ratings_file = config.interim_data_dir / f"team_ratings_{season}_week_{current_week}.csv"
        schedule_file = config.interim_data_dir / f"game_schedule_{season}.csv"
        
        if ratings_file.exists() and schedule_file.exists():
            ratings_df = pd.read_csv(ratings_file)
            schedule_df = pd.read_csv(schedule_file)
            console.print("✅ Loaded existing data files")
        else:
            console.print("📡 Scraping fresh data...")
            scraper = NFLDataScraper()
            ratings_df = scraper.scrape_team_ratings(season, current_week)
            schedule_df = scraper.scrape_game_schedule(season)
    
    except Exception as e:
        console.print(f"❌ Failed to load/scrape data: {e}")
        raise typer.Exit(1)
    
    # Process upcoming games
    processor = GameDataProcessor()
    games_df = processor.process_upcoming_games(schedule_df, ratings_df)
    
    # Filter to remaining weeks
    remaining_weeks = [w for w in games_df['week'].unique() 
                      if w >= current_week and w not in picked_teams_dict.keys()]
    games_df = games_df[games_df['week'].isin(remaining_weeks)]
    
    if games_df.empty:
        console.print("❌ No games available for optimization")
        raise typer.Exit(1)
    
    # Predict win probabilities
    predictions_df = model.predict_game_probabilities(games_df)
    console.print(f"🎯 Generated predictions for {len(predictions_df)} games")
    
    # Optimize picks
    optimizer = EliminatorOptimizer()
    result = optimizer.optimize_eliminator_picks(
        predictions_df, 
        picked_teams_dict, 
        remaining_weeks,
        risk_adjustment
    )
    
    if result.success:
        console.print("🎉 [bold green]Optimization successful![/bold green]")
        
        # Display results
        _display_optimization_results(result, predictions_df)
        
        # Run simulations
        sim_results = optimizer.simulate_season_outcomes(
            result.optimal_picks, 
            result.weekly_probs
        )
        _display_simulation_results(sim_results)
        
    else:
        console.print(f"❌ Optimization failed: {result.solver_status}")
        raise typer.Exit(1)


@app.command()
def analyze_picks(
    picks_file: str = typer.Argument(..., help="Path to CSV file with picks"),
    season: int = typer.Option(2024, help="NFL season year"),
):
    """Analyze existing eliminator picks."""
    console.print(f"[bold blue]Analyzing eliminator picks[/bold blue]")
    
    try:
        picks_df = pd.read_csv(picks_file)
        console.print(f"✅ Loaded {len(picks_df)} picks")
        
        # Display picks analysis
        _display_picks_analysis(picks_df)
        
    except Exception as e:
        console.print(f"❌ Failed to analyze picks: {e}")
        raise typer.Exit(1)


def _display_optimization_results(result, games_df):
    """Display optimization results in a nice table."""
    
    # Create picks table
    table = Table(title="🏆 Optimal Eliminator Picks")
    table.add_column("Week", style="cyan", no_wrap=True)
    table.add_column("Pick", style="bold green")
    table.add_column("vs", style="dim")
    table.add_column("Win Prob", style="magenta")
    table.add_column("Confidence", style="yellow")
    
    for week in sorted(result.optimal_picks.keys()):
        team, opponent = result.optimal_picks[week]
        prob = result.weekly_probs[week]
        confidence = abs(prob - 0.5) * 2
        
        table.add_row(
            str(week),
            team,
            opponent,
            f"{prob:.1%}",
            f"{confidence:.1%}"
        )
    
    console.print(table)
    
    # Summary panel
    summary_text = f"""
    Expected Survival Probability: {result.expected_survival_prob:.1%}
    Solver: {result.metadata.get('solver', 'Unknown')}
    Solve Time: {result.solver_time:.2f}s
    """
    
    console.print(Panel(summary_text, title="📊 Summary", border_style="blue"))


def _display_simulation_results(sim_results):
    """Display simulation results."""
    
    summary_text = f"""
    Survival Rate: {sim_results['survival_rate']:.1%}
    Average Weeks Survived: {sim_results['avg_weeks_survived']:.1f}
    Total Simulations: {sim_results['n_simulations']:,}
    """
    
    console.print(Panel(summary_text, title="🎲 Simulation Results", border_style="green"))


def _display_picks_analysis(picks_df):
    """Display analysis of existing picks."""
    
    table = Table(title="📈 Picks Analysis")
    table.add_column("Week", style="cyan")
    table.add_column("Team", style="bold")
    table.add_column("Result", style="green")
    table.add_column("Notes", style="dim")
    
    for _, row in picks_df.iterrows():
        table.add_row(
            str(row.get('week', '')),
            str(row.get('team', '')),
            str(row.get('result', 'TBD')),
            str(row.get('notes', ''))
        )
    
    console.print(table)


def main():
    """Main entry point for CLI."""
    app()


if __name__ == "__main__":
    main()
