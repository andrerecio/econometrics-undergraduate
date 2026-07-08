# ==============================================================================
# Tutorial 3 - Nonlinearity: logarithms
# Econometrics I
# ==============================================================================

# --- Packages ----------------------------------------------------------------

library(tidyverse)    # dplyr, ggplot2
library(modelsummary) # regression tables
library(fixest)       # estimate OLS with feols()
library(wooldridge)   # wage1 and campus datasets

# --- Data ---------------------------------------------------------------------

# Current Population Survey, 1976
data("wage1", package = "wooldridge")

# --- Introduction: log(wage) ~ educ -------------------------------------------

# Log-linear model:
# log(wage_i) = beta_0 + beta_1 * educ_i + u_i
reg_logwage <- feols(log(wage) ~ educ, data = wage1, vcov = "hetero")

# Prediction from the log-linear model and retransformation to the original scale
wage1 <- wage1 %>%
  mutate(
    logwage_pred = predict(reg_logwage),   # predicted values in log
    wage_pred    = exp(logwage_pred)        # predicted wage in dollars
  )

# Visual comparison: log-linear curve vs OLS line
ggplot(wage1, aes(x = educ)) +
  geom_line(aes(y = wage_pred), color = "#ff7f0e", linewidth = 1.5) +
  geom_smooth(aes(y = wage), method = "lm", se = FALSE,
              color = "#1f77b4", linewidth = 1.5) +
  labs(
    title = "Comparison: Linear vs Log-Linear Regression",
    x = "Years of education",
    y = "Predicted hourly wage in dollars"
  ) +
  theme_minimal()

# --- Log-linear vs levels -----------------------------------------------------

# Regression in levels for comparison
reg_wage <- feols(wage ~ educ, data = wage1, vcov = "hetero")

modelsummary(
  list("Level" = reg_wage, "Log-linear" = reg_logwage),
  title = "Dependent variable: wage or log(wage)",
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Interpretation:
# - log-linear: beta_1 * 100 = percentage change in wage for one additional year of educ
#   -> an additional year of education is associated with an 8.3% increase in hourly wage
# - R^2 is not comparable across the two regressions: Y is different
#   (wage vs log(wage))

# --- Log-log and linear-log ----------------------------------------------------

reg_logwagelog <- feols(log(wage) ~ log(educ), data = wage1, vcov = "hetero")
reg_wagelog    <- feols(wage ~ log(educ),      data = wage1, vcov = "hetero")

modelsummary(
  list("Log-linear" = reg_logwage,
       "Log-log"   = reg_logwagelog,
       "Lin-log" = reg_wagelog),
  title = "Dependent Variable: log(wage) or wage",
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Interpretation:
# - log-linear: +1 year of educ -> +8.3% in wage
# - log-log:    +1% in educ     -> +0.825% in wage (elasticity)
# - linear-log: +1% in educ     -> +0.053 dollars in wage

# --- Change in units of measurement: Y in tens of dollars ------------------------

# With Y in logs, changing units of measurement leaves the coefficients unchanged:
# only the intercept changes (it decreases by log(10)).
# In the linear-log model, the coefficient also changes because Y is in tens.

reg_logwage_dec    <- feols(log(wage/10) ~ educ,      data = wage1, vcov = "hetero")
reg_logwagelog_dec <- feols(log(wage/10) ~ log(educ), data = wage1, vcov = "hetero")
reg_wagelog_dec    <- feols(wage/10 ~ log(educ),      data = wage1, vcov = "hetero")

modelsummary(
  list("Log-linear" = reg_logwage_dec,
       "Log-log"   = reg_logwagelog_dec,
       "Lin-log" = reg_wagelog_dec),
  title = "Dependent Variable: log(wage/10) or wage/10",
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# --- Change in units of measurement: X in months of education -----------------------

# educ_monthly = educ * 12
wage1 <- wage1 %>%
  mutate(educ_monthly = educ * 12)

reg_logwage_monthly    <- feols(log(wage) ~ educ_monthly,      data = wage1, vcov = "hetero")
reg_logwagelog_monthly <- feols(log(wage) ~ log(educ_monthly), data = wage1, vcov = "hetero")
reg_wagelog_monthly    <- feols(wage ~ log(educ_monthly),      data = wage1, vcov = "hetero")

modelsummary(
  list("Log-linear" = reg_logwage_monthly,
       "Log-log"   = reg_logwagelog_monthly,
       "Lin-log" = reg_wagelog_monthly),
  title = "Dependent Variable: log(wage) or wage (educ in months)",
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Notes on the intercepts:
# log(educ * 12) = log(educ) + log(12), so the term log(12)
# is absorbed by the intercept.
# - log-log:   beta_0' = -0.445 - 0.825 * log(12) ≈ -2.495
# - linear-log: beta_0' = -7.460 - 5.330 * log(12) ≈ -20.70

# --- Dummy Variables and Logs ----------------------------------------------

# Construct three mutually exclusive dummy variables.
# Omitted category: single men (female = 0 & married = 0)
wage1 <- wage1 %>%
  mutate(
    marrmale   = ifelse(female == 0 & married == 1, 1, 0),
    marrfemale = ifelse(female == 1 & married == 1, 1, 0),
    singfem    = ifelse(female == 1 & married == 0, 1, 0)
  )

reg_logwage_dummy <- feols(
  log(wage) ~ marrmale + marrfemale + singfem + educ + exper + tenure,
  data = wage1, vcov = "hetero"
)

modelsummary(
  list("Log Wage" = reg_logwage_dummy),
  title = "Log(wage) with Marital Status and Gender Dummies",
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Interpretation (percentage difference relative to single men,
# holding educ, exper, and tenure constant):
# - marrmale:   married men earn about 29.2% more
# - marrfemale: married women earn about 12.0% less
# - singfem:    single women earn about 9.7% less

# --- Example: Campus Crime -----------------------------------------------------

# Dataset: 97 colleges and universities
# crime  = annual number of crimes on campus
# enroll = number of students enrolled
# police = number of campus police officers
data("campus", package = "wooldridge")

reg_log_crime  <- feols(log(crime) ~ log(enroll),               data = campus, vcov = "hetero")
reg_log_crime2 <- feols(log(crime) ~ log(enroll) + log(police), data = campus, vcov = "hetero")

modelsummary(
  list("Log Crime" = reg_log_crime,
       "Log Crime (with police)" = reg_log_crime2),
  title = "Campus Crime",
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Interpretation:
# - +1% in enrollment -> +1.27% in crimes (without controlling for police)
# - +1% in enrollment -> +0.92% in crimes, holding police constant
#
# Limitation: we are ignoring other factors (city size, local income, etc.)
# that may be correlated with both enroll and crime.
