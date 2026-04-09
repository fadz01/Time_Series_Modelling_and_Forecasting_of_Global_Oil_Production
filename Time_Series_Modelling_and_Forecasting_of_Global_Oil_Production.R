#step 1
time_series=read.csv(file.choose())

#step 2
summary(time_series)
head(time_series)
tail(time_series)


#step3

#histogram
hist(time_series$Oil.production..TWh., main = "Histogram of Oil Production", xlab = "Oil Production (TWh)", 
     col = "skyblue")

#boxplot

boxplot(time_series$Oil.production..TWh.)
#check Outliers
boxplot(time_series$Oil.production..TWh.)$out #boxplot tak nampak outlier
boxplot.stats(time_series$Oil.production..TWh.)$out #kalau ikut soalan assigment


#Step 4
#plot time series 
#based on time series ada oulier from the random shock
DV.ts=ts(DV,frequency = 1,start = c(1990)) #define as ts object
plot.ts(DV.ts,xlab="time(Year)",ylab="Oil..TWh..substituted.energy.")
length(DV)

acf(time_series$Oil.production..TWh.)
ggseasonplot(time_series$Oil.production..TWh.) # cannot run because no seasonality
oil_ts = ts(time_series$Oil.production..TWh., start = 1990, frequency = 1)
ggseasonplot(oil_ts)


#step 5 
oil_ts = ts(time_series$Oil.production..TWh., start = 1990, frequency = 1)
# 1. Naïve with Drift

fit_naive_drift = rwf(oil_ts, drift = TRUE)


# Show model summary
summary(fit_naive_drift)


# 2. Average Change

# Your original time series
oil_ts = ts(time_series$Oil.production..TWh., start = 1990, frequency = 1)

# Fit the model
avg_change = mean(diff(oil_ts))
first_value = oil_ts[1]
n = length(oil_ts)

# Generate fitted values
fitted_avg_change = first_value + (0:(n - 1)) * avg_change

# Show model summary
summary(fitted_avg_change)


# 3. Holt's Linear Trend Model

fit_holt = holt(oil_ts)



#Step6

fitted_values_holt = fitted(fit_holt)

plot(oil_ts,
     main = "Observed vs Fitted - Holt’s Linear Method",
     ylab = "Oil Production (TWh)", xlab = "Year",
     col = "black", lwd = 2)

lines(fitted_values_holt, col = "blue", lwd = 2)

legend("bottomright",
       legend = c("Observed", "Fitted (Holt’s Linear)"),
       col = c("black", "blue"),
       lty = 1,
       lwd = 2)


#Step7
library(forecast)
# Accuracy for Naïve with Drift
accuracy(fit_naive_drift)
# Accuracy for  Holt's Linear Trend Model
accuracy(fit_holt)
#Accuracy for average change
# Convert time series to numeric
actual <- as.numeric(oil_ts)
predicted <- as.numeric(fitted_avg_change)

# Compute accuracy metrics
me_val <- mean(predicted - actual)
mae_val <- mae(actual, predicted)
rmse_val <- rmse(actual, predicted)
mape_val <- mape(actual, predicted) * 100  # convert to %

# Print results
cat("Accuracy of Average Change Model:\n")
cat("Mean Error (ME):", round(me_val, 4), "\n")
cat("Mean Absolute Error (MAE):", round(mae_val, 4), "\n")
cat("Root Mean Squared Error (RMSE):", round(rmse_val, 4), "\n")
cat("Mean Absolute Percentage Error (MAPE):", round(mape_val, 2), "%\n")






#Step8
# Load additional libraries
library(forecast)
library(lmtest)         # For Breusch-Pagan and Durbin-Watson
library(tseries)        # For Jarque-Bera, ad.test
library(nortest)        # For Anderson-Darling
library(ggplot2)
library(lmtest)
library(car)   
# Fit the best model (Holt's Linear)
fit_holt = holt(ts(time_series$Oil.production..TWh., start = 1990, frequency = 1))

# Extract residuals and fitted values
res = residuals(fit_holt)
std_res = scale(res)                      # Standardized residuals
fit_vals = fitted(fit_holt)

#i)
# Scatterplot of residuals

# Scatter plot: Actual vs Fitted
# Create dataframe with actual and fitted values
df = data.frame(
  Actual = as.numeric(oil_ts),
  Fitted = as.numeric(fitted(fit_holt))
)

# Scatter plot
ggplot(df, aes(x = Actual, y = Fitted)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Actual vs Fitted Values (Holt’s Linear Model)",
    x = "Actual Oil Production",
    y = "Fitted Oil Production"
  ) +
  theme_minimal()

# Histogram
hist(res, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", breaks = 10)

#ii)
# Q-Q plot
qqnorm(res)
qqline(res, col = "red")

# Normality tests
ad.test(res)              # Anderson-Darling

#Kolmogorov-Smirnov test

# Run Kolmogorov-Smirnov test against standard normal distribution
ks.test(std_res , "pnorm")

#iii)
# Plot standardized residuals vs. fitted values
df = data.frame(
  Fitted = as.numeric(fit_vals),
  StdResid = as.numeric(std_res)
  )

ggplot(df, aes(x = Fitted, y = StdResid)) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Standardized Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Standardized Residuals"
  ) +
  theme_minimal()



# Breusch-Pagan test
bp_test = bptest(lm(res ~ fit_vals))
print(bp_test)



#iv)
# ACF plot
acf(res, main = "ACF of Residuals")

# Durbin-Watson test
durbinWatsonTest(lm(res ~ fit_vals) ) # simple test for autocorrelation


#v)
# Scatterplot: residual vs lagged residual
plot(res[-length(res)], res[-1],
     main = "Lag Plot of Residuals", xlab = "Lagged Residual", ylab = "Residual")
abline(h = 0, v = 0, col = "grey")

# Ljung-Box test (lag independence)
Box.test(res, lag = 10, type = "Ljung-Box")



#vi)
# Boxplot
boxplot(res, main = "Boxplot of Residuals")

# Identify outlier values
outliers = boxplot.stats(res)$out
print(outliers)



#vii)
# ACF of squared residuals
acf(res^2, main = "ACF of Squared Residuals")



#viii)
# Generate normal random data of same length
random_res= rnorm(length(res))

# Plot & tests on simulated residuals
par(mfrow = c(1,1))
plot(random_res, main = "Random Normal Series")
hist(random_res, main = "Histogram of Random Series")
qqnorm(random_res)
qqline(random_res)
acf(random_res, main = "ACF of Random Normal Residuals")
acf(random_res^2, main = "ACF of Squared Random Normal Residuals")
# Ljung-Box
Box.test(random_res, lag = 10, type = "Ljung-Box")
# Kolmogorov-Smirnov test
ks.test(std_res , "pnorm")
#durbinWatsonTest
durbinWatsonTest(lm(random_res ~ fit_vals) ) # simple test for autocorrelation
# Breusch-Pagan test
bptest(lm(random_res ~ fit_vals))



#step10

library(forecast)
library(ggplot2)

# Convert to time series
oil_ts = ts(time_series$Oil.production..TWh., start = 1990, frequency = 1)

# Fit Holt’s Linear model with 12-step forecast
fit_holt = holt(oil_ts, h = 12, level = c(80, 95))

# Plot
autoplot(fit_holt) +
  autolayer(oil_ts, series = "Observed") +
  autolayer(fitted(fit_holt), series = "Fitted") +
  labs(title = "Holt’s Linear Forecast with Observed and Fitted Values",
       x = "Year", y = "Oil Production (TWh)") +
  scale_color_manual(name = "Series",
                     values = c("Observed" = "black", "Fitted" = "blue")) +
  theme_minimal() +
  theme(legend.position = "right")


print(fit_holt)






#to resest output chart
par(mfrow = c(1,1))


detach("package:Metrics", unload = TRUE)
detach("package:car", unload = TRUE)
detach("package:tseries", unload = TRUE)
detach("package:nortest", unload = TRUE)

# 1. AR Model (Black vs Red)
plot(ts_oil, type = "l", col = "black", lwd = 2,
     main = "Observed vs Fitted - AR Model",
     ylab = "Oil Production (TWh)", xlab = "Year")
lines(fitted(ar_model), col = "red", lwd = 2)
legend("topleft", legend = c("Observed", "Fitted (AR)"),
       col = c("black", "red"), lty = 1, lwd = 2, bty = "n")

# 2. ARMA Model (Black vs Purple)
plot(ts_oil, type = "l", col = "black", lwd = 2,
     main = "Observed vs Fitted - ARMA Model",
     ylab = "Oil Production (TWh)", xlab = "Year")
lines(fitted(arma_model), col = "purple", lwd = 2)
legend("topleft", legend = c("Observed", "Fitted (ARMA)"),
       col = c("black", "purple"), lty = 1, lwd = 2, bty = "n")

# 3. ARIMA(1,1,1) Model (Black vs Green)
plot(ts_oil, type = "l", col = "black", lwd = 2,
     main = "Observed vs Fitted - ARIMA(1,1,1)",
     ylab = "Oil Production (TWh)", xlab = "Year")
lines(fitted(arima_model), col = "green", lwd = 2)
legend("topleft", legend = c("Observed", "Fitted (ARIMA)"),
       col = c("black", "green"), lty = 1, lwd = 2, bty = "n")

# 4. Auto ARIMA Model (Black vs Blue)
plot(ts_oil, type = "l", col = "black", lwd = 2,
     main = "Observed vs Fitted - Auto ARIMA",
     ylab = "Oil Production (TWh)", xlab = "Year")
lines(fitted(auto_model), col = "yellow", lwd = 2)
legend("topleft", legend = c("Observed", "Fitted (Auto ARIMA)"),
       col = c("black", "yellow"), lty = 1, lwd = 2, bty = "n")

#compare
# Load libraries
library(forecast)
library(ggplot2)

# Step 1: Time series conversion
oil_ts <- ts(time_series$Oil.production..TWh., start = 1990, frequency = 1)

# Step 2: Fit both models
fit_holt <- holt(oil_ts)
fit_auto_arima <- auto.arima(oil_ts)

# Step 3: Plot Observed vs Fitted (Both Models)
plot(oil_ts, type = "l", col = "black", lwd = 2,
     main = "Observed vs Fitted - Holt's Linear vs Auto ARIMA",
     ylab = "Oil Production (TWh)", xlab = "Year")

# Add fitted lines for both models
lines(fitted(fit_holt), col = "blue", lwd = 2)          # Holt's Linear
lines(fitted(fit_auto_arima), col = "yellow", lwd = 2)  # Auto ARIMA

# Add legend
legend("topleft", 
       legend = c("Observed", "Fitted (Holt's)", "Fitted (Auto ARIMA)"),
       col = c("black", "blue", "yellow"),
       lty = 1,
       lwd = 2,
       bty = "n")

