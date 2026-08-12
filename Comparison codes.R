# ============================================================
# 1. SETUP & DATA DOWNLOADING
# ============================================================
# Load necessary libraries
library(quantmod)
library(PerformanceAnalytics)

# Define the tickers
# ICLN = Clean Energy, EFA = Int'l Equity, ^GSPC = S&P 500 (Market), AGG = Bonds
tickers <- c("ICLN", "EFA", "^GSPC", "AGG")

# Download daily data from Yahoo Finance (2008-07-01 to 2023-12-31)
getSymbols(tickers, 
           src = "yahoo", 
           from = "2008-07-01", 
           to = "2023-12-31")

# ============================================================
# 2. DATA CLEANING & RETURN CALCULATION
# ============================================================
# Create a merged xts object of Adjusted Close prices
# We use Ad() to extract the Adjusted Close column automatically
prices <- merge(Ad(ICLN), Ad(EFA), Ad(GSPC), Ad(AGG))

# Fill any missing data (NA) with the previous day's value
prices <- na.locf(prices)

# Calculate Daily Log Returns
# method="log" ensures we get continuous returns for statistical modeling
returns_all <- CalculateReturns(prices, method = "log")

# Remove the first row (which is NA after calculating returns)
returns_all <- na.omit(returns_all)

# Rename columns for cleaner charts
colnames(returns_all) <- c("ICLN", "EFA", "Market (SPY)", "Bonds (AGG)")

# ============================================================
# 3. VISUAL 1: CORRELATION STRUCTURE MATRIX
# ============================================================
# This creates a matrix showing:
# - Diagonal: Histogram/Distribution of returns (shows Fat Tails)
# - Lower Triangle: Scatter plots with a red smooth line
# - Upper Triangle: Correlation coefficients (digits) with significance stars
chart.Correlation(returns_all, 
                  histogram = TRUE, 
                  pch = 19, 
                  main = "Correlation Structure: Diversification Potential")

# ============================================================
# 4. VISUAL 2: ASSET COMPARISON DASHBOARD
# ============================================================
# This creates a 3-panel professional chart:
# - Panel 1: Cumulative Return (Growth of $1)
# - Panel 2: Daily Returns (Volatility bars)
# - Panel 3: Drawdown (The "Underwater" chart showing losses)

charts.PerformanceSummary(returns_all, 
                          main = "Asset Performance Comparison (2008-2023)",
                          colorset = c("blue", "orange", "black", "green"), # Custom colors
                          lwd = 2, # Line width
                          ylog = TRUE) # Log scale for cumulative return (optional)

# ============================================================
# 5. VISUAL 3: Consolidated Boxplot
# ============================================================
# Create the professional consolidated boxplot
chart.Boxplot(returns_all,
              main = "Comparative Daily Log Returns (2008-2023)",
              xlab = "Log Returns",
              ylab = "Asset Class",
              element.color = "darkgray",
              as.is = TRUE) # Keeps the order of your columns
# ============================================================
# 5. BONUS: KEY STATISTICS TABLE
# ============================================================
# Generates a table of annualized stats to support the charts
table.AnnualizedReturns(returns_all, scale = 252, Rf = 0)

