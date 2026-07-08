# Tutorial 4 - Nonlinearity: polynomials

# Setup ----
library(tidyverse)
library(knitr)
library(tinytable)
library(modelsummary)
library(fixest)
library(wooldridge)

data("wage1", package = "wooldridge")


# Introduction: regression of wage on exper + exper^2 ----
# Note: in fixest the ^ operator is exponentiation, so exper^2 == I(exper^2).
# In lm/glm/lme4, I(exper^2) is required; without I(), the quadratic
# term would be silently ignored.
reg_wage_exper <- feols(wage ~ exper + exper^2, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_exper), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Plot: linear vs quadratic polynomial ----
# 1. Predicted values
wage1 <- wage1 %>%
  mutate(wageexp_pred = predict(reg_wage_exper))

# 2. Filter to exper <= 40
wage_cut <- wage1 %>% filter(exper <= 40)

# 3. Find the maximum point of the wageexp_pred curve
punto_max <- wage_cut %>%
  filter(wageexp_pred == max(wageexp_pred, na.rm = TRUE))

# 4. Plot
ggplot(wage_cut, aes(x = exper)) +
  geom_line(aes(y = wageexp_pred), color = "#ff7f0e", linewidth = 1.5) +              # estimated curve
  geom_smooth(aes(y = wage), method = "lm", se = FALSE, color = "#1f77b4", linewidth = 1.5) + # OLS line
  geom_point(data = punto_max, aes(y = wageexp_pred), color = "#2ca02c", size = 5) +
  labs(
    title = "Comparison: Linear vs Quadratic Polynomial",
    x = "Years of Experience",
    y = "Hourly wage in $"
  ) +
  theme_minimal()


# Polynomial with controls (educ, tenure) ----
reg_wage_exper2 <- feols(wage ~ exper + exper^2 + educ + tenure, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_exper, "Wage" = reg_wage_exper2), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Finding the peak: first derivative and marginal effect ----
# dy/dx = b1 + 2*b2*x; peak at x* = -b1 / (2*b2)
b1 <- coef(reg_wage_exper)["exper"]
b2 <- coef(reg_wage_exper)["I(exper^2)"]
xstar <- -b1 / (2 * b2)

deriv_df <- tibble(
  exper = seq(0, 50, length.out = 200),
  d = b1 + 2 * b2 * exper
)

ggplot(deriv_df, aes(x = exper, y = d)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = xstar, color = "grey60", linewidth = 0.4, linetype = "dashed") +
  geom_line(color = "#ff7f0e", linewidth = 1.5) +
  annotate("point", x = xstar, y = 0, color = "#2ca02c", size = 5) +
  labs(
    title = "First Derivative: Marginal Effect of One Year of Experience",
    x = "Years of experience",
    y = "Estimated marginal effect"
  ) +
  theme_minimal()


# Logs and Polynomials ----
reg_logwage_exper <- feols(log(wage) ~ exper + exper^2 + educ + tenure, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_exper, "Log Wage" = reg_logwage_exper), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Deviation from the mean: simple regression ----
# By centering exper, the intercept becomes the predicted wage for an individual
# with experience equal to the mean (rather than zero years of experience).
wage1 <- wage1 %>%
  mutate(exper_center = exper - mean(exper, na.rm = TRUE))

reg_experc <- feols(wage ~ exper_center, data = wage1, vcov = "hetero")
reg_exper <- feols(wage ~ exper, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_experc, "Wage" = reg_exper), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Deviation from the mean and polynomials ----
# With the polynomial, the coefficient on exper_center represents the marginal effect
# when exper is equal to its mean (rather than zero).
reg_wagepolcenter <- feols(wage ~ educ + exper_center + exper_center^2 + tenure, data = wage1, vcov = "hetero")
reg_wagepol <- feols(wage ~ educ + exper + exper^2 + tenure, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wagepolcenter, "Wage" = reg_wagepol), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Descriptive statistics for exper ----
desc_exper <- wage1 %>%
  summarise(
    Mean = mean(exper, na.rm = TRUE),
    Q1 = quantile(exper, 0.25, na.rm = TRUE),
    Mediana = median(exper, na.rm = TRUE),
    Q3 = quantile(exper, 0.75, na.rm = TRUE)
  )

tt(desc_exper, digits = 3)
