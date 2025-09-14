#!/usr/bin/env Rscript

# Test script to understand ffanalytics data structure
# This will help us understand what team-level data we can extract

# Install ffanalytics if not already installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cran.rstudio.com/")
}

if (!requireNamespace("ffanalytics", quietly = TRUE)) {
  cat("Installing ffanalytics...\n")
  remotes::install_github("FantasyFootballAnalytics/ffanalytics")
}

# Load required libraries
library(ffanalytics)
library(jsonlite)

cat("=== FFANALYTICS TEST SCRIPT ===\n")
cat("Testing ffanalytics data structure for team ratings extraction\n\n")

# Test 1: Check available sources
cat("1. Available sources (skipping internal check):\n")
cat("Known sources: CBS, ESPN, FantasyPros, FantasySharks, FFToday, NumberFire, NFL\n")

# Test 2: Try to scrape DST data (team defense)
cat("\n2. Attempting to scrape DST data from CBS and ESPN...\n")

tryCatch({
  # Scrape DST data for current week
  dst_data <- scrape_data(
    src = c("CBS", "ESPN"), 
    pos = "DST",
    season = NULL,  # Current season
    week = NULL     # Current week
  )
  
  cat("DST scraping successful!\n")
  cat("Data structure:\n")
  str(dst_data, max.level = 2)
  
  if (length(dst_data) > 0 && "DST" %in% names(dst_data)) {
    cat("\nDST data sample:\n")
    print(head(dst_data$DST, 10))
    
    cat("\nColumn names in DST data:\n")
    print(colnames(dst_data$DST))
    
    # Check unique data sources
    if ("data_src" %in% colnames(dst_data$DST)) {
      cat("\nUnique data sources:\n")
      print(unique(dst_data$DST$data_src))
    }
    
    # Check unique teams
    if ("team" %in% colnames(dst_data$DST)) {
      cat("\nUnique teams:\n")
      print(unique(dst_data$DST$team))
    }
  }
  
}, error = function(e) {
  cat("Error scraping DST data:", e$message, "\n")
})

# Test 3: Try to get projections table
cat("\n3. Attempting to create projections table...\n")

tryCatch({
  if (exists("dst_data") && length(dst_data) > 0) {
    projections <- projections_table(dst_data)
    cat("Projections table created successfully!\n")
    cat("Projections structure:\n")
    str(projections, max.level = 1)
    
    if (nrow(projections) > 0) {
      cat("\nProjections sample:\n")
      print(head(projections, 5))
      
      cat("\nProjections column names:\n")
      print(colnames(projections))
    }
  }
}, error = function(e) {
  cat("Error creating projections table:", e$message, "\n")
})

# Test 4: Check if we can get team-level aggregated data
cat("\n4. Summary of findings:\n")
cat("- This will help determine if ffanalytics can provide team ratings\n")
cat("- DST data represents team defense, which could be used as team strength indicator\n")
cat("- We may need to aggregate player data by team to get team ratings\n")

cat("\n=== TEST COMPLETE ===\n")
