# =========================
# 1) Packages
# =========================
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(zoo)  # for na.locf

# =========================
# 2) Download price data
# =========================
tickers <- c("ICLN", "EFA", "^GSPC")
getSymbols(tickers, src = "yahoo", from = "2008-07-01", to = "2023-12-31")

# =========================
# 3) Prices -> daily log returns
# =========================
# Create a merged xts object of Adjusted Close prices
# We use Ad() to extract the Adjusted Close column automatically
prices <- merge(Ad(ICLN), Ad(EFA), Ad(GSPC))

# Fill any missing data (NA) with the previous day's value
prices <- na.locf(prices)

# Calculate Daily Log Returns
# method="log" ensures we get continuous returns for statistical modeling
R <- CalculateReturns(prices, method = "log")

# Remove the first row (which is NA after calculating returns)
R <- na.omit(R)

# Rename columns for cleaner charts
colnames(R) <- c("ICLN", "EFA", "Market (SPY)")
# =========================
# 4) Parameters
# =========================
ci_level <- 0.95
tail_p   <- 0.95       # 5% left tail
MAR0     <- 0          # breakeven hurdle

# =========================
# 5) Downside risk table (no annualization)
# =========================
table.DownsideRisk(
  R      = R,
  ci     = ci_level,
  scale  = NA,      # set to 252 if you want annualized deviations
  MAR    = MAR0,    # try MAR0 and MAR5 for sensitivity
  p      = tail_p,
  digits = 4
)

# =========================
# 6) Tail risk metrics (VaR & ES at 95%)
# =========================
VaR(R, p = tail_p, method = "historical")
VaR(R, p = tail_p, method = "modified")   # Cornish-Fisher (skew/kurtosis)
VaR(R, p = tail_p, method = "gaussian")   # parametric normal

ES(R, p = tail_p, method = "historical")
ES(R, p = tail_p, method = "modified")
ES(R, p = tail_p, method = "gaussian")

# =========================
# 7) Downside-aware performance
# =========================
SortinoRatio(R, MAR = MAR0)


Omega(R, threshold = MAR0)

# Calculates the annualized Sharpe Ratio for all assets at a 0% risk-free rate
sharpe_ann <- SharpeRatio.annualized(R[, 1:3], 
                                     Rf = 0, 
                                     scale = 252)

# Display the results
print(sharpe_ann)
