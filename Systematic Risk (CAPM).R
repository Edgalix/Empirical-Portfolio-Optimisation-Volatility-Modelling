# Loads the quantmod package for financial data retrieval and manipulation
library(quantmod)

# Downloads Global Clean Energy(ICLN), Intl. Equity (EFA) and S&P500 (^GSPC) 
# price data from Yahoo Finance
tickers <- c("ICLN", "EFA", "^GSPC", "AGG")
getSymbols(tickers, src = "yahoo", from = "2008-07-01", to = "2023-12-31")

# Downloads the 3-Month Treasury Bill rate from the FRED database
getSymbols("DGS3MO",
           src  = "FRED",
           from = "2008-07-01",
           to   = "2023-12-31")

# Loads the zoo package for time-series data handling
library(zoo)

# Converts the annualized risk-free rate to a daily rate and forward-fills missing values
rf <- na.locf(DGS3MO) / 100 / 252

#DATA CLEANING
# Forward-fills missing values in ICLN index prices
ICLN_filled <- na.locf(ICLN)
# Forward-fills missing values in EFA index prices
EFA_filled <- na.locf(EFA)
# Forward-fills missing values in S&P 500 index prices
GSPC_filled <- na.locf(GSPC)
# Forward-fills missing values in AGG index prices
AGG_filled <- na.locf(AGG)

# Calculate Daily Log Returns for ETFs using closing prices
ICLN_logret <- diff(log(Cl(ICLN_filled)))
EFA_logret  <- diff(log(Cl(EFA_filled)))
mkt_logret  <- diff(log(Cl(GSPC_filled))) # Market Benchmark
bnd_logret  <- diff(log(Cl(AGG_filled))) # BOND Benchmark


# Merges ETF returns, market returns, and the risk-free rate into one dataset
df <- merge(ICLN_logret, EFA_logret, mkt_logret, bnd_logret, rf, all = FALSE)

# Renames columns for clarity
colnames(df) <- c("ICLN", "EFA", "Market","Bonds", "RF")

# Computes ETF excess returns over the risk-free rate
ICLN_excess <- ICLN_logret - rf
EFA_excess <- EFA_logret - rf


# Computes market excess returns over the risk-free rate
market_excess <- mkt_logret - rf    # This is your Market Premium

# Computes bond excess returns over the risk-free rate
bond_excess <- bnd_logret - rf    # This is your Market Premium

# RUN CAPM REGRESSIONS
# Regression for ICLN (Clean Energy)
model_ICLN <- lm(ICLN_excess ~ market_excess, data = df)  
summary(model_ICLN) # Displays regression results including alpha, beta, and statistical significance

# Regression for EFA (International Equity)
model_EFA <- lm(EFA_excess ~ market_excess)
summary(model_EFA)



# Loads the xts package for time-series object handling
library(xts)

# Merges excess return series and removes missing observations
df_capm <- na.omit(merge(market_excess, ICLN_excess, EFA_excess))

# Renames columns for plotting and analysis
colnames(df_capm) <- c("Market", "ICLN", "EFA")

# Converts the xts object into a standard data frame (removes date index)
df_capm <- data.frame(
  Market  = coredata(df_capm$Market),
  ICLN = coredata(df_capm$ICLN),
  EFA  = coredata(df_capm$EFA)
)

# ICLN vs Market excess returns
plot(df_capm$Market,
     df_capm$ICLN,
     main = "CAPM: ICLN Sensitivity to S&P500",
     xlab = "Market Excess Returns",
     ylab = "ICLN Excess Returns",
     pch  = 19,
     col  = rgb(0, 0, 1, 0.4))
# Adds the CAPM regression line to the scatter plot
abline(model_ICLN, col = "red", lwd = 2)

# EFA vs Market excess returns
plot(df_capm$Market,
     df_capm$EFA,
     main = "CAPM: EFA Sensitivity to S&P500",
     xlab = "Market Excess Returns",
     ylab = "EFA Excess Returns",
     pch  = 19,
     col  = rgb(0, 0, 1, 0.4))
# Adds the CAPM regression line to the scatter plot
abline(model_EFA, col = "red", lwd = 2)

# Estimates CAPM alpha and beta for the two stocks relative to the market
table.CAPM(df[,1:2, drop = FALSE],
           df[,3, drop = FALSE],
           Rf = df[,5, drop = FALSE])

