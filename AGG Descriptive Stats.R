# Loads the quantmod package into the current R session
library(quantmod)

# Downloads historical daily stock price data for Clean Energy (AGG) from Yahoo Finance
getSymbols("AGG", src = "yahoo", from = "2008-07-01", to = "2023-12-31")

# Calculates daily log returns using AGG’s adjusted closing prices
AGG_logret <- diff(log(Cl(AGG)))

# Displays the first few observations of the log return series
head(AGG_logret)

# Removes missing values generated during the return calculation
AGG_logret <- na.omit(AGG_logret)

# Renames the column for clarity and easier interpretation
colnames(AGG_logret) <- "AGG_Log_Return"

# Displays the first few rows of the cleaned log return data
head(AGG_logret)

# Plots AGG’s closing stock price over time as a line chart
plot(Cl(AGG), type = "l",
     main = "AGG Closing Price",
     xlab = "Date",
     ylab = "Price (USD)")

# Plots the time series of AGG log returns
plot(AGG_logret,
     main = "Log Returns of AGG",
     col = "blue")

# Creates a histogram showing the frequency distribution of log returns
hist(AGG_logret,
     breaks = 50,
     main = "Frequency Distribution of AGG Log Returns",
     xlab = "Log return",
     col = "lightblue",
     border = "white")

# Creates a density histogram of log returns
hist(AGG_logret,
     breaks = 50,
     freq = FALSE,
     main = "AGG Log Returns with Normal Curve",
     xlab = "Log return",
     col = "lightgray")

# Overlays a normal distribution curve using the sample mean and standard deviation
curve(dnorm(x,
            mean(AGG_logret, na.rm = TRUE),
            sd(AGG_logret, na.rm = TRUE)),
      add = TRUE,
      col = "red",
      lwd = 2)

# Loads the PerformanceAnalytics package
library(PerformanceAnalytics)

# Displays summary statistics such as mean, volatility, skewness, and kurtosis
table.Stats(AGG_logret)
