# Loads the quantmod package into the current R session
library(quantmod)

# Downloads historical daily stock price data for Clean Energy (GSPC) from Yahoo Finance
getSymbols("^GSPC", src = "yahoo", from = "2008-07-01", to = "2023-12-31")

# Calculates daily log returns using GSPC’s closing prices
GSPC_logret <- diff(log(Cl(GSPC)))

# Displays the first few observations of the log return series
head(GSPC_logret)

# Removes missing values generated during the return calculation
GSPC_logret <- na.omit(GSPC_logret)

# Renames the column for clarity and easier interpretation
colnames(GSPC_logret) <- "GSPC_Log_Return"

# Displays the first few rows of the cleaned log return data
head(GSPC_logret)

# Plots GSPC’s closing stock price over time as a line chart
plot(Cl(GSPC), type = "l",
     main = "GSPC Closing Price",
     xlab = "Date",
     ylab = "Price (USD)")

# Plots the time series of GSPC log returns
plot(GSPC_logret,
     main = "Log Returns of GSPC",
     col = "blue")

# Creates a histogram showing the frequency distribution of log returns
hist(GSPC_logret,
     breaks = 50,
     main = "Frequency Distribution of GSPC Log Returns",
     xlab = "Log return",
     col = "lightblue",
     border = "white")

# Creates a density histogram of log returns
hist(GSPC_logret,
     breaks = 50,
     freq = FALSE,
     main = "GSPC Log Returns with Normal Curve",
     xlab = "Log return",
     col = "lightgray")

# Overlays a normal distribution curve using the sample mean and standard deviation
curve(dnorm(x,
            mean(GSPC_logret, na.rm = TRUE),
            sd(GSPC_logret, na.rm = TRUE)),
      add = TRUE,
      col = "red",
      lwd = 2)

# Loads the PerformanceAnalytics package
library(PerformanceAnalytics)

# Displays summary statistics such as mean, volatility, skewness, and kurtosis
table.Stats(GSPC_logret)
