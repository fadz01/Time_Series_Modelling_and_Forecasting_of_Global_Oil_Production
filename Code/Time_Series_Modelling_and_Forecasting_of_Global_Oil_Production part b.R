# Load required libraries
library(forecast)
library(tseries)
library(lmtest)
library(ggplot2)
library(urca)
library(readr)
library(dplyr)
library(moments)

# STEP 1: Import the dataset and filter for 'World'
data <- read.csv("Assignment 2 dataset (Oil Production 1990-2023).csv")

# Filter only 'World' data
world_data <- filter(data, Entity == "World")

# STEP 2: Create a time series object
ts_oil <- ts(world_data$Oil.production, start = 1990, frequency = 1)

# Time series plot
plot(ts_oil, main = "Global Oil Production (1990–2023)",
     ylab = "Production (TWh)", xlab = "Year", col = "blue")

# Summary statistics
summary(ts_oil)
sd(ts_oil)
skewness(ts_oil)
sum(is.na(ts_oil))  # Check missing values

# STEP 3: Shape of the distribution
hist(ts_oil, main = "Distribution of Oil Production", 
     xlab = "TWh", col = "lightgreen", border = "black")
lines(density(ts_oil), col = "red", lwd = 2)

# STEP 4: Visual trend
autoplot(ts_oil) + ggtitle("Oil Production Time Series") +
  ylab("Production (TWh)") + xlab("Year")


# STEP 5: Check Stationarity – Augmented Dickey-Fuller Test
adf_test <- adf.test(ts_oil)
print(adf_test)

# If non-stationary, apply first differencing
diff_oil <- diff(ts_oil)
plot(diff_oil, main = "First Difference of Oil Production", 
     ylab = "Differenced TWh", xlab = "Year", col = "darkgreen")

# ADF test again after differencing
adf_diff <- adf.test(diff_oil)
print(adf_diff)

# ACF and PACF of differenced series
acf(diff_oil, main = "ACF of Differenced Series")
pacf(diff_oil, main = "PACF of Differenced Series")

#STEP 6:
diff_oil <- diff(ts_oil)
plot(diff_oil, main = "First Differenced Oil Production Series", ylab = "Differenced TWh", xlab = "Year")
adf.test(diff_oil)

#STEP 7:
acf(diff_oil, main = "ACF of First Differenced Series")
pacf(diff_oil, main = "PACF of First Differenced Series")

#STEP 8:
#1. Fit AR(p) to the Original Time Series
# Plot PACF for the original series to determine p-order
pacf(ts_oil, main = "PACF of Original Time Series")

# From PACF, let's assume p = 1 (based on the first significant spike in the PACF)
ar_model <- Arima(ts_oil, order = c(1, 0, 0))  # p = 1, d = 0, q = 0 (AR model)
summary(ar_model)

#2. Fit ARMA(p,q) to the Original Time Series
# Plot ACF and PACF for the original series
acf(ts_oil, main = "ACF of Original Time Series")
pacf(ts_oil, main = "PACF of Original Time Series")

# Assume from the plots that p = 1 and q = 1 (for example)
arma_model <- Arima(ts_oil, order = c(1, 0, 1))  # p = 1, d = 0, q = 1 (ARMA model)
summary(arma_model)

#3. Fit ARIMA(p,d,q) to the Original Time Series
# Fit ARIMA model (p = 1, d = 1, q = 1)
arima_model <- Arima(ts_oil, order = c(1, 1, 1))  # p = 1, d = 1, q = 1
summary(arima_model)

#4. Fit auto.arima to the Original Time Series
# Fit the auto.arima model
auto_model <- auto.arima(ts_oil)
summary(auto_model)

#Model Comparison
# Compare AIC and BIC for the models
cat("AR Model AIC:", AIC(ar_model), "BIC:", BIC(ar_model), "\n")
cat("ARMA Model AIC:", AIC(arma_model), "BIC:", BIC(arma_model), "\n")
cat("ARIMA Model AIC:", AIC(arima_model), "BIC:", BIC(arima_model), "\n")
cat("auto.arima Model AIC:", AIC(auto_model), "BIC:", BIC(auto_model), "\n")

#STEP 9:
# Load library for accuracy measures
library(forecast)

# Get accuracy measures for each model
acc_ar    <- accuracy(ar_model)
acc_arma  <- accuracy(arma_model)
acc_arima <- accuracy(arima_model)
acc_auto  <- accuracy(auto_model)

# Extract ACF1 separately
acf1_ar    <- acf(residuals(ar_model), plot = FALSE)$acf[2]
acf1_arma  <- acf(residuals(arma_model), plot = FALSE)$acf[2]
acf1_arima <- acf(residuals(arima_model), plot = FALSE)$acf[2]
acf1_auto  <- acf(residuals(auto_model), plot = FALSE)$acf[2]

# Build comparison table
model_comparison <- data.frame(
  Model = c("AR", "ARMA", "ARIMA", "auto.arima"),
  ME    = c(acc_ar[1, "ME"], acc_arma[1, "ME"], acc_arima[1, "ME"], acc_auto[1, "ME"]),
  RMSE  = c(acc_ar[1, "RMSE"], acc_arma[1, "RMSE"], acc_arima[1, "RMSE"], acc_auto[1, "RMSE"]),
  MAE   = c(acc_ar[1, "MAE"], acc_arma[1, "MAE"], acc_arima[1, "MAE"], acc_auto[1, "MAE"]),
  MPE   = c(acc_ar[1, "MPE"], acc_arma[1, "MPE"], acc_arima[1, "MPE"], acc_auto[1, "MPE"]),
  MAPE  = c(acc_ar[1, "MAPE"], acc_arma[1, "MAPE"], acc_arima[1, "MAPE"], acc_auto[1, "MAPE"]),
  MASE  = c(acc_ar[1, "MASE"], acc_arma[1, "MASE"], acc_arima[1, "MASE"], acc_auto[1, "MASE"]),
  ACF1  = c(acf1_ar, acf1_arma, acf1_arima, acf1_auto),
  AIC   = c(AIC(ar_model), AIC(arma_model), AIC(arima_model), AIC(auto_model))
)

print(model_comparison)

#STEP 10:

# Check residuals of the best model (replace with your chosen model if not auto_model)
checkresiduals(auto_model)

# Histogram and Q-Q plot of residuals
resid_vals <- residuals(auto_model)
hist(resid_vals, main = "Histogram of Residuals", col = "lightblue")
qqnorm(resid_vals)
qqline(resid_vals, col = "red")

#STEP 11:


# 12-step ahead forecast using best model
forecast_12 <- forecast(auto_model, h = 12, level = c(80, 95))

# Plot forecast with confidence intervals
autoplot(forecast_12) +
  autolayer(fitted(auto_model), series = "Fitted") +
  autolayer(ts_oil, series = "Observed") +
  ggtitle("12-Year Forecast of Global Oil Production") +
  ylab("Production (TWh)") + xlab("Year") +
  guides(colour = guide_legend(title = "Series"))

# View forecasted values with confidence intervals
print(forecast_12)


