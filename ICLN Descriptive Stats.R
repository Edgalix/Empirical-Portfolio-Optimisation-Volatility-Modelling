# Loads the quantmod package into the current R session
library(quantmod)

# Downloads historical daily stock price data for Clean Energy (ICLN) from Yahoo Finance
getSymbols("ICLN", src = "yahoo", from = "2008-07-01", to = "2023-12-31")

# Calculates daily log returns using Apple’s closing prices
ICLN_logret <- diff(log(Cl(ICLN)))

# Displays the first few observations of the log return series
head(ICLN_logret)

# Removes missing values generated during the return calculation
ICLN_logret <- na.omit(ICLN_logret)

# Renames the column for clarity and easier interpretation
colnames(ICLN_logret) <- "ICLN_Log_Return"

# Displays the first few rows of the cleaned log return data
head(ICLN_logret)

# Plots Apple’s closing stock price over time as a line chart
plot(Cl(ICLN), type = "l",
     main = "ICLN Closing Price",
     xlab = "Date",
     ylab = "Price (USD)")

# Plots the time series of ICLN log returns
plot(ICLN_logret,
     main = "Log Returns of ICLN",
     col = "blue")

# Creates a histogram showing the frequency distribution of log returns
hist(ICLN_logret,
     breaks = 50,
     main = "Frequency Distribution of ICLN Log Returns",
     xlab = "Log return",
     col = "lightblue",
     border = "white")

# Creates a density histogram of log returns
hist(ICLN_logret,
     breaks = 50,
     freq = FALSE,
     main = "ICLN Log Returns with Normal Curve",
     xlab = "Log return",
     col = "lightgray")

# Overlays a normal distribution curve using the sample mean and standard deviation
curve(dnorm(x,
            mean(ICLN_logret, na.rm = TRUE),
            sd(ICLN_logret, na.rm = TRUE)),
      add = TRUE,
      col = "red",
      lwd = 2)

# Loads the PerformanceAnalytics package
library(PerformanceAnalytics)

# Displays summary statistics such as mean, volatility, skewness, and kurtosis
table.Stats(ICLN_logret)
