# Loads the rugarch package for estimating GARCH-family volatility models
library(rugarch)

# Loads quantmod for financial data retrieval
library(quantmod)

# Downloads EFA (EFA) daily price data from Yahoo Finance
getSymbols("EFA",
           src  = "yahoo",
           from = "2008-07-01",
           to   = "2023-12-31")

# Forward-fills missing observations in EFA price data
EFA_filled <- na.locf(EFA)

# Computes daily log returns from EFA closing prices
EFA_logreturn_diff <- diff(log(Cl(EFA_filled)))

# Removes missing values caused by differencing
EFA_logreturn <- na.omit(EFA_logreturn_diff)

# Plots the time series of EFA log returns
plot(EFA_logreturn)

# Plots a histogram to inspect the distribution of log returns
hist(EFA_logreturn, breaks = 60)

# Displays the autocorrelation function of log returns
acf(EFA_logreturn)

# Displays the autocorrelation function of squared log returns (volatility clustering)
acf(EFA_logreturn^2)

# Loads the FinTS package for ARCH/GARCH diagnostic tests
library(FinTS)

# Performs Engle’s ARCH test on log returns to detect conditional heteroskedasticity
ArchTest(as.numeric(EFA_logreturn), lags = 12)

# Fits an AR(1) model to the mean of log returns
mean_fit <- arima(EFA_logreturn, order = c(1, 0, 0))

# Extracts residuals from the mean equation
resid_mean <- residuals(mean_fit)

# Performs ARCH test on mean-adjusted residuals
ArchTest(as.numeric(resid_mean), lags = 12)

# Tests for autocorrelation in raw log returns
Box.test(EFA_logreturn, lag = 12, type = "Ljung-Box")

# Tests for autocorrelation in squared log returns
Box.test(EFA_logreturn^2, lag = 12, type = "Ljung-Box")

# Tests for autocorrelation in mean equation residuals
Box.test(resid_mean, lag = 12, type = "Ljung-Box")

# Tests for autocorrelation in squared residuals
Box.test(resid_mean^2, lag = 12, type = "Ljung-Box")

# Specifies a standard GARCH(1,1) model with AR(1) mean and normal errors
spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

# Fits the GARCH(1,1) model to EFA log returns
fit_garch <- ugarchfit(spec = spec_garch, data = EFA_logreturn)

# Displays parameter estimates and diagnostics
show(fit_garch)

# Produces standard diagnostic plots for the GARCH model
plot(fit_garch)

# Extracts conditional volatility estimates from the GARCH model
EFA_sigma <- sigma(fit_garch)

# Plots the conditional volatility from the GARCH(1,1) model
plot(EFA_sigma,
     main = "Conditional Volatility from GARCH(1,1)",
     ylab = "Sigma_t",
     xlab = "Date")

# Specifies a GJR-GARCH(1,1) model to allow asymmetric volatility responses
spec_gjrgarch <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

# Fits the GJR-GARCH model
fit_gjrgarch <- ugarchfit(spec = spec_gjrgarch, data = EFA_logreturn)

# Displays GJR-GARCH estimation results
show(fit_gjrgarch)

# Produces diagnostic plots for the GJR-GARCH model
plot(fit_gjrgarch)

# Extracts conditional volatility from the GJR-GARCH model
EFA_gjrsigma <- sigma(fit_gjrgarch)

# Plots conditional volatility from the GJR-GARCH model
plot(EFA_gjrsigma,
     main = "Conditional Volatility from GJR-GARCH(1,1)",
     ylab = "Sigma_t",
     xlab = "Date")

# Specifies a GARCH-in-Mean (GARCH-M) model where volatility enters the mean equation
spec_garchM <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 0),
                        include.mean = TRUE,
                        archm = TRUE,
                        archpow = 2),
  distribution.model = "norm"
)

# Fits the GARCH-M model
fit_garchM <- ugarchfit(spec = spec_garchM, data = EFA_logreturn)

# Displays GARCH-M estimation results
show(fit_garchM)

# Produces diagnostic plots for the GARCH-M model
plot(fit_garchM)

# Extracts conditional volatility from the GARCH-M model
EFA_garchmsigma <- sigma(fit_garchM)

# Plots conditional volatility from the GARCH-M model
plot(EFA_garchmsigma,
     main = "Conditional Volatility from GARCHM(1,1)",
     ylab = "Sigma_t",
     xlab = "Date")
