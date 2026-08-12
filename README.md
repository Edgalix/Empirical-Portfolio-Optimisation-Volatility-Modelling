# Empirical Portfolio Optimisation & Volatility Modelling: ICLN vs EFA

Risk, return, and diversification analysis of the iShares Global Clean Energy ETF
(ICLN) and iShares MSCI EAFE ETF (EFA) against US equity (SPY) and bond (AGG)
benchmarks, July 2008 – December 2023.

## Question

Do ICLN (thematic clean-energy exposure) and EFA (developed-market international
equity) improve a traditional 60/40 portfolio's risk-adjusted performance and
diversification, or do they merely add correlated risk without compensation?

## Method

- **Data:** Daily adjusted close prices from Yahoo Finance (`quantmod`), July 2008–Dec 2023.
- **Distributional analysis:** descriptive statistics, skewness/kurtosis, box plots to
  test for fat tails and non-normality.
- **Systematic risk:** CAPM regression against SPY, including asymmetric (upside vs
  downside) beta.
- **Volatility dynamics:** GARCH(1,1) for clustering/persistence, GJR-GARCH for the
  leverage effect (asymmetric response to negative shocks), GARCH-M for risk premium
  testing.
- **Downside risk:** modified VaR and Expected Shortfall (95%), maximum drawdown,
  Sortino and Omega ratios.
- **Portfolio construction:** a constrained core-satellite allocation (30% SPY / 20%
  AGG / 40% EFA / 10% ICLN) benchmarked against a traditional 60/40 portfolio, plus a
  5,000-iteration Monte Carlo simulation to trace the efficient frontier.

## Headline result

- ICLN: 33.68% annualised volatility, -10.43% annualised return, 91.77% maximum
  drawdown, and a statistically insignificant GARCH-M risk premium — investors are not
  being compensated for the volatility they're taking on.
- EFA: 0.90 correlation with SPY — it behaves largely as a redundant, higher-friction
  copy of US equity exposure rather than a genuine diversifier.
- The constrained 10% ICLN / 40% EFA allocation underperformed a plain 60/40 benchmark
  on both return and drawdown.

## Known limitations / next steps

- All testing is in-sample; the portfolio allocation and efficient frontier have not
  been validated out-of-sample (e.g. optimise on 2008–2018, test on 2019–2023). This is
  the single highest-priority next step.
- Single static allocation rather than a rolling/rebalanced backtest.

## Run it

Written in R. Requires `quantmod`, `PerformanceAnalytics`, and `rugarch`.

```r
install.packages(c("quantmod", "PerformanceAnalytics", "rugarch"))
```

Then run the scripts in order: data setup → distributional analysis → CAPM → GARCH
models → downside risk → portfolio optimisation.
