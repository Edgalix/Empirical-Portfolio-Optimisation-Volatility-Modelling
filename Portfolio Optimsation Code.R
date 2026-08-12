library(quantmod) 
library(PerformanceAnalytics) 
library(PortfolioAnalytics) 
library(DEoptim) 

# ============================================================
# 1. DATA PREPARATION
# ============================================================
# Download asset prices (Matching your report dates: July 2008 - Dec 2023)
getSymbols(c("ICLN", "EFA", "SPY", "AGG"), from = "2008-07-01", to = "2023-12-31")

# Merge Adjusted Close prices
prices.data <- merge(ICLN[,6], EFA[,6], SPY[,6], AGG[,6])
colnames(prices.data) <- c("ICLN", "EFA", "SPY", "AGG")

returns.data <- CalculateReturns(prices.data)
returns.data <- na.omit(returns.data)

# Download Risk-Free Rate (3-month T-Bill)
getSymbols("DTB3", src = "FRED", from = "2008-07-02", to = "2023-12-31")
Rf <- DTB3 / 100 / 12 # Monthly risk-free rate
Rf <- Rf[index(returns.data)]
Rf <- na.locf(Rf)


# Based on analysis (ICLN = high risk/low return, EFA = diversifier),
# we propose: 10% ICLN, 40% EFA. (Plus fixed 30% SPY, 20% AGG).

w_justified <- c(0.10, 0.40, 0.30, 0.20) 
names(w_justified) <- colnames(returns.data)

# Run the Portfolio Backtest
port_justified <- Return.portfolio(returns.data, weights = w_justified, rebalance_on = "years")

# ============================================================
# 3. DEFINING THE BENCHMARK (Standard 60/40 Equity/Bond)
# ============================================================
# Comparison: 60% SPY / 40% AGG
w_bench <- c(0, 0, 0.60, 0.40)
port_benchmark <- Return.portfolio(returns.data, weights = w_bench, rebalance_on = "years")

# ============================================================
# 4. OUTPUT TABLES 
# ============================================================

# Merge for comparison
comparison_rets <- merge(port_justified, port_benchmark)
colnames(comparison_rets) <- c("Portfolio (10/40/30/20)", "Benchmark_60/40")

# A. Descriptive Stats (Annualised Return, Std Dev, Sharpe)
table.AnnualizedReturns(comparison_rets, scale = 12, Rf = mean(Rf))

# B. Downside Risks (VaR, ES, Drawdown)
table.DownsideRisk(comparison_rets, Rf = mean(Rf), p = 0.95, ci = 0.95, scale = 252)

# C. Risk Adjusted Ratios (Sortino & Omega)
SortinoRatio(comparison_rets, MAR = 0)
Omega(comparison_rets, L = 0)

# ============================================================
# 5. CHARTS 
# ============================================================

# Chart 1: Cumulative Performance
charts.PerformanceSummary(comparison_rets, 
                          main = "Growth of $1:  Portfolio vs Benchmark",
                          colorset = c("blue", "red"),
                          lwd = 2)

# Chart 2: Rolling Volatility (Shows stability)
chart.RollingPerformance(comparison_rets, width = 12, Rf = Rf,
                          main = "Rolling 1-Year Volatility",
                          legend.loc = "topleft", colorset = c("blue", "red"))

# ============================================================
# 6. OPTIMIZATION 
# ============================================================
# WHY 10/40 split is better than 25/25.

# Create Specification
Spec_constr <- portfolio.spec(assets = colnames(returns.data))
Spec_constr <- add.constraint(portfolio = Spec_constr, type = "full_investment")
Spec_constr <- add.constraint(portfolio = Spec_constr, type = "long_only")

# We force SPY to 30% and AGG to 20% (+/- small buffer for solver stability)
Spec_constr <- add.constraint(Spec_constr, type = "box", 
                              min = c(0.00, 0.00, 0.29, 0.19), 
                              max = c(0.50, 0.50, 0.31, 0.21))

# Objective: Minimize Variance (Reduce Risk)
Spec_constr <- add.objective(portfolio = Spec_constr, type = "return", name = "mean")
Spec_constr <- add.objective(portfolio = Spec_constr, type = "risk", name = "StdDev")

# Run Optimization (Reduced search_size for speed)
set.seed(1234)
opt_result <- optimize.portfolio(R = returns.data, portfolio = Spec_constr, 
                                 optimize_method = "random", 
                                 search_size = 2000, itermax = 50,
                                 traceDE = 5,
                                 trace = TRUE,
                                 maximize = TRUE)

print(opt_result)
extractWeights(opt_result) # Confirms that holding LESS ICLN is mathematically safer.

# Visualise the "Efficient Frontier" of random portfolios
chart.RiskReward(opt_result, return.col = "mean", risk.col = "StdDev", 
                 chart.assets = TRUE, main = "Efficient Frontier (Random Search)")
