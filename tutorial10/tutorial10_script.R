# Tutorial 10 - Regression with instrumental variables (part 2)

# ====================================================================
# Setup
# ====================================================================

library(tidyverse)
library(knitr)
library(kableExtra)
library(modelsummary)
library(fixest)
library(wooldridge)
library(car)


# ====================================================================
# Angrist and Evans (1998) - children and maternal labor supply
# ====================================================================

data("labsup", package = "wooldridge")
head(labsup)


# --- IV model: samesex as an instrument for kids ---

iv_labsup <- feols(hours ~ educ + age + black + hispan | kids ~ samesex,
                   data = labsup, vcov = "hetero")

# First stage
summary(iv_labsup, stage = 1)

# Instrument relevance test
fitstat(iv_labsup, type = "ivwald")

# Same test using linearHypothesis on the manually estimated first stage
first_stage <- feols(kids ~ samesex + educ + age + black + hispan,
                     data = labsup, vcov = "hetero")

linearHypothesis(first_stage, "samesex = 0")

# Second stage
summary(iv_labsup)

# Comparison with OLS estimates
ols_labsup <- feols(hours ~ kids + educ + age + black + hispan,
                    data = labsup, vcov = "hetero")

modelsummary(
  list("OLS" = ols_labsup,
       "IV" = iv_labsup),
  gof_omit = "IC|Adj|RMSE"
)


# --- Twin birth as an instrument: multi2nd ---

iv_labsup_twins <- feols(hours ~ educ + age + black + hispan | kids ~ multi2nd,
                         data = labsup, vcov = "hetero")

# First stage
summary(iv_labsup_twins, stage = 1)

# Relevance test
fitstat(iv_labsup_twins, type = "ivwald")

# Using linearHypothesis
first_stage_twins <- feols(kids ~ multi2nd + educ + age + black + hispan,
                           data = labsup, vcov = "hetero")

linearHypothesis(first_stage_twins, "multi2nd = 0")

# Second stage
summary(iv_labsup_twins, stage = 2)

# Comparison of the two instruments
modelsummary(
  list("Hours (IV samesex)"  = iv_labsup,
       "Hours (IV multi2nd)" = iv_labsup_twins),
  gof_omit = "IC|Adj|RMSE"
)


# ====================================================================
# Overidentification
# ====================================================================

# Overidentified model using both instruments
iv_labsup_twins_over <- feols(hours ~ educ + age + black + hispan |
                                kids ~ multi2nd + samesex,
                              data = labsup, vcov = "hetero")
iv_labsup_twins_over

# First stage
summary(iv_labsup_twins_over, stage = 1)

# Joint relevance test (fitstat uses the F statistic: stat = Wald / m)
fitstat(iv_labsup_twins_over, type = "ivwald")

# Using linearHypothesis: compare the Wald statistic with the threshold m x 10 = 20
first_stage_over <- feols(kids ~ multi2nd + samesex + educ + age + black + hispan,
                          data = labsup, vcov = "hetero")

linearHypothesis(first_stage_over, c("multi2nd = 0", "samesex = 0"))


# --- J test of overidentifying restrictions ---
# Same procedure used for Card: 2SLS residuals, auxiliary regression,
# J = m * F, with the p-value from chi2(m - l). Here m = 2, l = 1 -> J ~ chi2(1) under H0
labsup <- labsup |> mutate(uhat = resid(iv_labsup_twins_over))

Jreg_labsup <- feols(uhat ~ educ + age + black + hispan + samesex + multi2nd,
                     data = labsup, vcov = "iid")

linearHypothesis(Jreg_labsup, c("samesex = 0", "multi2nd = 0"))

# The 5% critical value for chi2(1) is 3.84 -> J = 0.589 does not reject H0
# Correct p-value with df = m - l = 1
1 - pchisq(0.5892, df = 1)

# Verification using fitstat (internal procedure with the correct degrees of freedom)
fitstat(iv_labsup_twins_over, "sargan")
