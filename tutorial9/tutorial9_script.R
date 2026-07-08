# Tutorial 9 - Regression with instrumental variables (IV)

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
# Card (1995) - return to education using nearc4 as an instrument
# ====================================================================

data("card", package = "wooldridge")
head(card)


# --- Exercise: regress wage and log(wage) on educ + exper ---

wage_educ    <- feols(wage      ~ educ + exper, data = card, vcov = "hetero")
logwage_educ <- feols(log(wage) ~ educ + exper, data = card, vcov = "hetero")

modelsummary(
  list("Wage" = wage_educ, "Log Wage" = logwage_educ),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- Two-stage least squares (TSLS) ---

# Simple IV (one instrument, no controls)
iv_card <- feols(log(wage) ~ 1 | educ ~ nearc4, data = card, vcov = "hetero")
iv_card

# First stage computed manually
fs_card <- feols(educ ~ nearc4, data = card, vcov = "hetero")
fs_card

card$educ_hat <- predict(fs_card)

# Second stage computed manually (standard errors are NOT correct)
sstage_card <- feols(log(wage) ~ educ_hat, data = card, vcov = "hetero")
sstage_card


# --- Direct algebraic derivation: beta_IV = cov(Z,Y) / cov(Z,X) ---

cov(card$nearc4, card$lwage) / cov(card$nearc4, card$educ)


# --- Derivation from the reduced form: beta_IV = gamma_hat / pi_hat ---

rf_card <- feols(log(wage) ~ nearc4, data = card, vcov = "hetero")
rf_card

beta_fs_1 <- coef(fs_card)["nearc4"]   # first-stage coefficient
beta_rf_1 <- coef(rf_card)["nearc4"]   # reduced-form coefficient
beta_rf_1 / beta_fs_1


# --- General model: IV with included exogenous variables (W) ---

iv_card_controls <- feols(log(wage) ~ exper + expersq + black + smsa + south |
                             educ ~ nearc4,
                           data = card, vcov = "hetero")
summary(iv_card_controls)

# OLS versus IV comparison
ols_card_controls <- feols(log(wage) ~ exper + expersq + black + smsa + south + educ,
                            data = card, vcov = "hetero")

modelsummary(
  list("Log Wage (OLS)" = ols_card_controls, "Log Wage (IV)" = iv_card_controls),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- Why geographic controls are needed: IV with regional indicators ---

iv_card_geo <- feols(log(wage) ~ exper + expersq + black + smsa +
                       reg662 + reg663 + reg664 + reg665 +
                       reg666 + reg667 + reg668 + reg669 |
                       educ ~ nearc4,
                     data = card, vcov = "hetero")

modelsummary(
  list("IV (baseline)" = iv_card_controls, "IV (+ geography)" = iv_card_geo),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- Overidentification: two instruments (nearc4 and nearc2) ---

iv_card_overid <- feols(log(wage) ~ exper + expersq + black + smsa + south |
                          educ ~ nearc4 + nearc2,
                        data = card, vcov = "hetero")
iv_card_overid


# J test of overidentifying restrictions (Sargan): manual procedure
# Warning: the correct degrees of freedom are m - l, NOT m
card <- card |> mutate(uhat = resid(iv_card_overid))

Jreg_card <- feols(uhat ~ exper + expersq + black + smsa + south + nearc4 + nearc2,
                   data = card, vcov = "iid")
lh <- linearHypothesis(Jreg_card, c("nearc4 = 0", "nearc2 = 0"))
lh

# Correct p-value using chi2(m - l) = chi2(1)
pchisq(2.6461, df = 1, lower.tail = FALSE)

# Verification with fitstat (internal procedure with correct d.f.)
fitstat(iv_card_overid, type = c("sargan"))


# --- Weak instruments: first-stage F test of relevance with controls ---

# One instrument (nearc4)
fs_card_ctrl <- feols(educ ~ nearc4 + exper + expersq + black + smsa + south,
                      data = card, vcov = "hetero")
linearHypothesis(fs_card_ctrl, "nearc4=0", test = "F")

# Two instruments (nearc4 + nearc2)
fs_card_overid <- feols(educ ~ nearc4 + nearc2 + exper + expersq + black + smsa + south,
                        data = card, vcov = "hetero")
linearHypothesis(fs_card_overid, c("nearc4=0", "nearc2=0"), test = "F")

# Verification with fitstat
fitstat(iv_card_overid, type = c("ivwald"))
