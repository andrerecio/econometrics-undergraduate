# =============================================================================
# Regression with fixest — Simple Linear Regression
# Econometrics — Sapienza University of Rome
# Dataset: wage1 (Current Population Survey 1976)
# =============================================================================

# --- Installing new libraries --------------------------------

install.packages("fixest")       # regression with robust standard errors
install.packages("modelsummary") # regression tables


# --- Libraries ------------------------------------------------------------------

library(tidyverse)    # data manipulation and graphs
library(wooldridge)   # datasets
library(fixest)       # regression with robust standard errors
library(modelsummary) # regression tables


# --- Data -----------------------------------------------------------------------

data("wage1", package = "wooldridge")


# --- Data structure -------------------------------------------------------

# first 6 rows of the dataset
head(wage1)

# main variables:
# wage  — average hourly earnings (dollars, 1976)
# educ  — years of education


# --- Descriptive statistics ------------------------------------------------------

wage1 |>
  summarise(
    wage_mean   = mean(wage),
    wage_median = median(wage),
    wage_sd     = sd(wage),
    educ_mean   = mean(educ),
    educ_median = median(educ),
    educ_sd     = sd(educ)
  )


# --- Visualization ---------------------------------------------------------------

# Distribution of hourly wage
ggplot(wage1, aes(x = wage)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  labs(title = "Distribution of hourly wage",
       x = "Hourly wage (dollars)", y = "Frequency") +
  theme_minimal(base_size = 13)

# Distribution of years of education
ggplot(wage1, aes(x = educ)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  labs(title = "Distribution of years of education",
       x = "Years of education", y = "Frequency") +
  theme_minimal(base_size = 13)

# Scatter plot: relationship between wage and educ
# geom_smooth(method = "lm") adds the regression line
ggplot(wage1, aes(y = wage, x = educ)) +
  geom_point(color = "black", alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(x = "Years of education",
       y = "Hourly wage (dollars)") +
  theme_minimal(base_size = 13)


# --- Simple linear regression ----------------------------------------------------

# Model: wage_i = beta0 + beta1 * educ_i + u_i
#
# feols() from fixest estimates OLS with robust standard errors
# vcov = "hetero" (equivalent to "hc1") — robust to heteroskedasticity
# Stata: regress wage educ, robust

reg1 <- feols(wage ~ educ, data = wage1, vcov = "hetero")
reg1

# Alternative with base R's lm() — homoskedastic standard errors
# For robust errors with lm() you need library(sandwich) and vcovHC()
reg_lm <- lm(wage ~ educ, data = wage1)
summary(reg_lm)


# --- Regression table -------------------------------------------------------------

# modelsummary() produces a formatted table of the results
# gof_omit removes unnecessary fit statistics
modelsummary(list("wage" = reg1), gof_omit = "AIC|BIC|RMSE|R2 Adj.")

# Interpretation:
# beta1 = 0.54: an additional year of education is associated with an average
#               increase in hourly wage of approximately 0.54 dollars
# beta0 = -0.90: predicted average hourly wage for an individual with
#                zero years of education (little substantive meaning)
