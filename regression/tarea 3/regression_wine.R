# Christian Badillo
# ---------------------- Libraries -----------------------------
library(tidyverse)
library(ggplot2)
library(corrplot)
# ---------------------- Data ----------------------------------
data.wine <- read.csv("vino_blanco.csv", header = TRUE, sep = ",")

# ---------------------- Data Exploration -----------------------
# Summary of the data
summary(data.wine)

# Boxplots
ggplot(data.wine, aes(x = "", y = density)) +
    geom_boxplot() +
    labs(title = "Boxplot of Density",
         y = "Density",
         x = "") +
    theme_minimal()


# Correlation matrix
correlation_matrix <- cor(data.wine)
correlation_matrix_log <- cor(log(data.wine))
corrplot(correlation_matrix_log, method = "number")

# Scatter plot
ggplot(data.wine, aes(y = residual.sugar, x = density)) +
  geom_point() +
  labs(title = "Scatter plot of Residual Sugar vs Density",
       y = "Residual Sugar",
       x = "Density") +
    xlim(0.98, 1.01) +
    ylim(0, 16)

ggplot(log(data.wine), aes(y = residual.sugar, x = density)) +
    geom_point() +
    labs(title = "Scatter plot of Log Residual Sugar vs Log Density",
         y = "Residual Sugar",
         x = "Density")

# -------------------- Regression -------------------------------
# Model
model <- lm(residual.sugar ~ density, data = data.wine)
summary(model)

## Residuals vs Fitted
ggplot(model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()

## Normal Q-Q
ggplot(model, aes(sample = .stdresid)) +
  stat_qq() +
  labs(title = "Normal Q-Q",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_minimal()

## Autocorrelation
acf(model$residuals, lag.max = 20)

# ---------------------------------------------------------------------
# Model Log
model_log <- lm(residual.sugar ~ density, data = log(data.wine))
summary(model_log)

## Residuals vs Fitted
ggplot(model_log, aes(x = .fitted, y = .resid)) +
    geom_point() +
    labs(title = "Residuals vs Fitted",
         x = "Fitted values",
         y = "Residuals") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal()

## Normal Q-Q
ggplot(model_log, aes(sample = .stdresid)) +
    stat_qq() +
    labs(title = "Normal Q-Q",
         x = "Theoretical Quantiles",
         y = "Standardized Residuals") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    theme_minimal()

## Autocorrelation
acf(model_log$residuals, lag.max = 20)

# -------------------------------------------------------------------------
## Analysis without outliers
# -------------------------------------------------------------------------
removeOutliers <- function(data, column_name) {
    Q1 <- quantile(data[[column_name]], 0.25)
    Q3 <- quantile(data[[column_name]], 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    return(data[data[[column_name]] >= lower_bound & data[[column_name]] <= upper_bound, ])
}

data_clean <- removeOutliers(data.wine, "density")
data_clean <- removeOutliers(data_clean, "residual.sugar")

# Scatter Plot Clean Data
ggplot(data_clean, aes(y = residual.sugar, x = density)) +
    geom_point() +
    labs(title = "Scatter plot of Residual Sugar vs Density",
         y = "Residual Sugar",
         x = "Density") +
    xlim(0.99, 1.005) +
    ylim(0.5, 4)

corrplot(cor(data_clean), method = "number")
# ----------------------------------------------------------------------------
# Model
model_clean <- lm(residual.sugar ~ density, data = data_clean)
summary(model_clean)

## Residuals vs Fitted
ggplot(model_clean, aes(x = .fitted, y = .resid)) +
    geom_point() +
    labs(title = "Residuals vs Fitted",
         x = "Fitted values",
         y = "Residuals") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal()

## Normal Q-Q
ggplot(model_clean, aes(sample = .stdresid)) +
    stat_qq() +
    labs(title = "Normal Q-Q",
         x = "Theoretical Quantiles",
         y = "Standardized Residuals") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    theme_minimal()

## Autocorrelation
acf(model_clean$residuals, lag.max = 20)
