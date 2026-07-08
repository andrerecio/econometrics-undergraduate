# Esercitazione 9 - Regressione con variabili strumentali (IV)

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
# Card (1995) - rendimento dell'istruzione con nearc4 come strumento
# ====================================================================

data("card", package = "wooldridge")
head(card)


# --- Esercizio: regressione bivariata wage e log(wage) su educ + exper ---

wage_educ    <- feols(wage      ~ educ + exper, data = card, vcov = "hetero")
logwage_educ <- feols(log(wage) ~ educ + exper, data = card, vcov = "hetero")

modelsummary(
  list("Wage" = wage_educ, "Log Wage" = logwage_educ),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- Minimi quadrati a due stadi (TSLS) ---

# IV semplice (un solo strumento, nessun controllo)
iv_card <- feols(log(wage) ~ 1 | educ ~ nearc4, data = card, vcov = "hetero")
iv_card

# Primo stadio "a mano"
fs_card <- feols(educ ~ nearc4, data = card, vcov = "hetero")
fs_card

card$educ_hat <- predict(fs_card)

# Secondo stadio "a mano" (errori standard NON corretti)
sstage_card <- feols(log(wage) ~ educ_hat, data = card, vcov = "hetero")
sstage_card


# --- Derivazione algebrica diretta: beta_IV = cov(Z,Y) / cov(Z,X) ---

cov(card$nearc4, card$lwage) / cov(card$nearc4, card$educ)


# --- Derivazione dalla forma ridotta: beta_IV = gamma_hat / pi_hat ---

rf_card <- feols(log(wage) ~ nearc4, data = card, vcov = "hetero")
rf_card

beta_fs_1 <- coef(fs_card)["nearc4"]   # coefficiente del primo stadio
beta_rf_1 <- coef(rf_card)["nearc4"]   # coefficiente della forma ridotta
beta_rf_1 / beta_fs_1


# --- Modello generale: IV con variabili esogene incluse (W) ---

iv_card_controlli <- feols(log(wage) ~ exper + expersq + black + smsa + south |
                             educ ~ nearc4,
                           data = card, vcov = "hetero")
summary(iv_card_controlli)

# Confronto OLS vs IV
ols_card_controlli <- feols(log(wage) ~ exper + expersq + black + smsa + south + educ,
                            data = card, vcov = "hetero")

modelsummary(
  list("Log Wage (OLS)" = ols_card_controlli, "Log Wage (IV)" = iv_card_controlli),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- Perché servono i controlli geografici: IV con dummy regionali ---

iv_card_geo <- feols(log(wage) ~ exper + expersq + black + smsa +
                       reg662 + reg663 + reg664 + reg665 +
                       reg666 + reg667 + reg668 + reg669 |
                       educ ~ nearc4,
                     data = card, vcov = "hetero")

modelsummary(
  list("IV (base)" = iv_card_controlli, "IV (+ geografia)" = iv_card_geo),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- Sovraidentificazione: due strumenti (nearc4 e nearc2) ---

iv_card_overid <- feols(log(wage) ~ exper + expersq + black + smsa + south |
                          educ ~ nearc4 + nearc2,
                        data = card, vcov = "hetero")
iv_card_overid


# Test J di sovraidentificazione (Sargan): procedura manuale
# Attenzione: i gradi di libertà corretti sono m - l, NON m
card <- card |> mutate(uhat = resid(iv_card_overid))

Jreg_card <- feols(uhat ~ exper + expersq + black + smsa + south + nearc4 + nearc2,
                   data = card, vcov = "iid")
lh <- linearHypothesis(Jreg_card, c("nearc4 = 0", "nearc2 = 0"))
lh

# p-value corretto con chi2(m - l) = chi2(1)
pchisq(2.6461, df = 1, lower.tail = FALSE)

# Verifica con fitstat (procedura interna con d.f. giusti)
fitstat(iv_card_overid, type = c("sargan"))


# --- Strumenti deboli: test di rilevanza F sul primo stadio con controlli ---

# Un solo strumento (nearc4)
fs_card_ctrl <- feols(educ ~ nearc4 + exper + expersq + black + smsa + south,
                      data = card, vcov = "hetero")
linearHypothesis(fs_card_ctrl, "nearc4=0", test = "F")

# Due strumenti (nearc4 + nearc2)
fs_card_overid <- feols(educ ~ nearc4 + nearc2 + exper + expersq + black + smsa + south,
                        data = card, vcov = "hetero")
linearHypothesis(fs_card_overid, c("nearc4=0", "nearc2=0"), test = "F")

# Verifica con fitstat
fitstat(iv_card_overid, type = c("ivwald"))
