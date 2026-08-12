# Loads the quantmod package into the current R session
library(quantmod)

# Downloads historical daily stock price data for Intl. Equity (EFA) from Yahoo Finance
getSymbols("EFA", src = "yahoo", from = "2008-07-01", to = "2023-12-31")

# Calculates daily log returns using EFA’s closing prices
EFA_logret <- diff(log(Cl(EFA)))

# Displays the first few observations of the log return series
head(EFA_logret)

# Removes missing values generated during the return calculation
EFA_logret <- na.omit(EFA_logret)

# Renames the column for clarity and easier interpretation
colnames(EFA_logret) <- "EFA_Log_Return"

# Displays the first few rows of the cleaned log return data
head(EFA_logret)

# Plots EFA’s closing stock price over time as a line chart
plot(Cl(EFA), type = "l",
     main = "EFA Closing Price",
     xlab = "Date",
     ylab = "Price (USD)")

# Plots the time series of EFA log returns
plot(EFA_logret,
     main = "Log Returns of EFA",
     col = "blue")

# Creates a histogram showing the frequency distribution of log returns
hist(EFA_logret,
     breaks = 50,
     main = "Frequency Distribution of EFA Log Returns",
     xlab = "Log return",
     col = "lightblue",
     border = "white")

# Creates a density histogram of log returns
hist(EFA_logret,
     breaks = 50,
     freq = FALSE,
     main = "EFA Log Returns with Normal Curve",
     xlab = "Log return",
     col = "lightgray")

# Overlays a normal distribution curve using the sample mean and standard deviation
curve(dnorm(x,
            mean(EFA_logret, na.rm = TRUE),
            sd(EFA_logret, na.rm = TRUE)),
      add = TRUE,
      col = "red",
      lwd = 2)

# Loads the PerformanceAnalytics package
library(PerformanceAnalytics)

# Displays summary statistics such as mean, volatility, skewness, and kurtosis
table.Stats(EFA_logret)

