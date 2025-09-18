#!/bin/bash

# NFL Eliminator Challenge - Complete Pipeline
# Run this script to execute the full workflow seamlessly

echo "🏈 NFL ELIMINATOR CHALLENGE - COMPLETE PIPELINE"
echo "=" * 50

echo ""
echo "📊 Step 1: Merging scraped odds with schedule data..."
uv run python scripts/create_merged_dataset.py

echo ""
echo "🧠 Step 2: Training team ratings model (production mode - all data)..."
uv run python scripts/train_team_ratings.py --production

echo ""
echo "🔮 Step 3: Generating 2025 predictions..."
uv run python scripts/generate_updated_2025_predictions.py

echo ""
echo "🎯 Step 4: Optimizing eliminator picks..."
uv run python scripts/optimize_eliminator_2025_fixed.py

echo ""
echo "✅ PIPELINE COMPLETE!"
echo "Your optimal eliminator picks are ready above."
