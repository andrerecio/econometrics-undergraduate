# =============================================================================
# Tutorial 2 - Multiple Regression
# =============================================================================

# -----------------------------------------------------------------------------
# Setup: packages and data
# -----------------------------------------------------------------------------

library(tidyverse)
library(knitr)
library(modelsummary)
library(wooldridge)
library(fixest)

# Load the wage1 dataset from the wooldridge package
data("wage1", package = "wooldridge")


# -----------------------------------------------------------------------------
# 1. Multiple Regression: educ, exper, tenure
# -----------------------------------------------------------------------------

# Model 1: simple regression (educ only)
reg_wage1 <- feols(wage ~ educ, data = wage1, vcov = "hetero")

# Model 2: add exper (years of labor-market experience)
reg_wage2 <- feols(wage ~ educ + exper, data = wage1, vcov = "hetero")

# Model 3: add tenure (years with the current employer)
reg_wage3 <- feols(wage ~ educ + exper + tenure, data = wage1, vcov = "hetero")

# Compare the three models
modelsummary(
  list("Wage" = reg_wage1, "Wage" = reg_wage2, "Wage" = reg_wage3),
  gof_omit = "AIC|BIC|RMSE"
)


# -----------------------------------------------------------------------------
# 2. Correlation between educ and exper
# -----------------------------------------------------------------------------

# The correlation between educ and exper is negative: people with more
# education tend to enter the labor market later and thus have less experience.
# Consequently, the coefficient on educ in the simple model is biased downward
# (it underestimates the effect of education on wage).
cor(wage1$educ, wage1$exper)


# -----------------------------------------------------------------------------
# 3. Include a dummy variable: female
# -----------------------------------------------------------------------------

# Model 4: add the female dummy to the model with educ, exper, and tenure
reg_wage4 <- feols(wage ~ educ + exper + tenure + female,
                   data = wage1, vcov = "hetero")

modelsummary(
  list("Wage" = reg_wage3, "Wage" = reg_wage4),
  gof_omit = "AIC|BIC|RMSE"
)


# -----------------------------------------------------------------------------
# 4. Changing units of measurement: monthly wage
# -----------------------------------------------------------------------------

# Monthly wage: assume full-time employment
# 7 hours/day x 5 days/week x 4 weeks/month = 140 hours/month
wage1 <- wage1 %>%
  mutate(wage_monthly = wage * 140)

# Estimate the same model with wage_monthly as the dependent variable
reg_wage_dummy3 <- feols(wage_monthly ~ educ + exper + tenure + female,
                         data = wage1, vcov = "hetero")

# Comparison: monthly-model coefficients equal hourly-model coefficients x 140
modelsummary(
  list("Wage" = reg_wage4, "Wage Monthly" = reg_wage_dummy3),
  gof_omit = "AIC|BIC|RMSE"
)


# -----------------------------------------------------------------------------
# 5. Dummy Variable: Wage and Gender
# -----------------------------------------------------------------------------

# Simple regression: female only (as in Tutorial 1)
reg_wage_dummy <- feols(wage ~ female, data = wage1, vcov = "hetero")

# Regression controlling for educ, exper, and tenure
reg_wage_dummy2 <- feols(wage ~ female + educ + exper + tenure,
                         data = wage1, vcov = "hetero")

modelsummary(
  list("Wage (Tutorial 1)" = reg_wage_dummy, "Wage" = reg_wage_dummy2),
  gof_omit = "AIC|BIC|RMSE"
)


# -----------------------------------------------------------------------------
# 6. Multiple-category dummies: marital status and gender
# -----------------------------------------------------------------------------

# Create three dummies. The omitted reference group is single men.
#  - marrmale:   married man
#  - marrfemale: married woman
#  - singfem:    single woman
wage1 <- wage1 %>%
  mutate(
    marrmale   = ifelse(female == 0 & married == 1, 1, 0),
    marrfemale = ifelse(female == 1 & married == 1, 1, 0),
    singfem    = ifelse(female == 1 & married == 0, 1, 0)
  )

# Dummy-variable means equal the corresponding sample proportions
statdummy <- wage1 %>%
  summarise(
    marrmale_mean   = mean(marrmale,   na.rm = TRUE),
    marrfemale_mean = mean(marrfemale, na.rm = TRUE),
    singfem_mean    = mean(singfem,    na.rm = TRUE)
  )
statdummy

# Regression with the three dummies and controls
# Interpret each dummy coefficient relative to the reference group (single men).
reg_wage_sm <- feols(
  wage ~ marrmale + marrfemale + singfem + educ + exper + tenure,
  data = wage1, vcov = "hetero"
)

modelsummary(list("Wage" = reg_wage_sm), gof_omit = "AIC|BIC|RMSE")


# -----------------------------------------------------------------------------
# 7. The dummy-variable trap
# -----------------------------------------------------------------------------

# Create the male dummy: 1 for men and 0 for women
wage1 <- wage1 %>%
  mutate(male = 1 - female)

# Including female and male with the other dummies creates perfect
# collinearity. R automatically removes redundant variables.
reg_wage_sm_dummy <- feols(
  wage ~ marrmale + marrfemale + singfem + female + male,
  data = wage1, vcov = "hetero"
)
reg_wage_sm_dummy


# -----------------------------------------------------------------------------
# 7.1 Dummy variables for geographic regions
# -----------------------------------------------------------------------------

# The wage1 dataset contains three regional dummies: northcen, south, and west.
# The fourth region (Northeast) is the omitted reference group: observations
# for which all three dummy variables equal zero.

# Count observations by region
# Note: use new names (n_*) to avoid dplyr name masking. If the output
# variables had the same names as the original variables, dplyr would replace
# them on the next line and produce incorrect results.
wage1 |>
  summarise(
    n_northcen  = sum(northcen),
    n_south     = sum(south),
    n_west      = sum(west),
    n_northeast = sum(1 - northcen - south - west)
  ) |>
  tt(caption = "Observations by region")

# The four categories sum to the total number of observations (526) because
# the regions are mutually exclusive and exhaustive.


# Descriptive statistics by region
# Reconstruct a categorical region variable from the dummies with case_when(),
# then aggregate by group.
wage1 |>
  mutate(
    region = case_when(
      northcen == 1 ~ "North Central",
      south    == 1 ~ "South",
      west     == 1 ~ "West",
      TRUE          ~ "Northeast"   # default: all dummies equal zero
    )
  ) |>
  group_by(region) |>
  summarise(
    n            = n(),
    mean_wage   = mean(wage),
    median_wage = median(wage),
    mean_educ   = mean(educ)
  ) |>
  arrange(desc(mean_wage)) |>    # sort by decreasing mean wage
  tt(caption = "Descriptive statistics by region") |>
  format_tt(digits = 3) |>
  theme_tt("striped")

# These are unadjusted differences: they do not account for regional
# differences in workforce composition (education, experience, gender, tenure).


# Regression with regional dummies
# Include only 3 of the 4 regional dummies to avoid perfect collinearity with
# the intercept (the dummy-variable trap). Northeast remains the reference group.
reg_region <- feols(
  wage ~ educ + exper + tenure + female + northcen + south + west,
  data = wage1, vcov = "hetero"
)

# Interpret each regional dummy coefficient as the mean difference relative to
# the Northeast, holding education, experience, tenure, and gender constant.
modelsummary(
  list("Wage" = reg_region),
  gof_omit = "AIC|BIC|RMSE|R2 Adj.",
  title = "Regression with Regional Dummies (Northeast = Reference Group)"
)



# -----------------------------------------------------------------------------
# 8. Testing joint hypotheses
# -----------------------------------------------------------------------------

# Test H0: the coefficients on marrmale, marrfemale, and singfem all equal zero
# (there are no wage differences across categories after holding educ, exper,
# and tenure constant).

# Option A: wald() from the fixest package
wald(reg_wage_sm, keep = "marrmale|marrfemale|singfem")

# Option B: linearHypothesis() from the car package (used more often)
# install.packages("car")  # if not already installed
library(car)

# Explicit F test
linearHypothesis(
  reg_wage_sm,
  c("marrmale = 0", "marrfemale = 0", "singfem = 0"),
  test = "F"
)

# Without test = "F" (the default is the chi-squared statistic)
linearHypothesis(
  reg_wage_sm,
  c("marrmale = 0", "marrfemale = 0", "singfem = 0")
)
