# Esercitazione 10 - Regressione con variabili strumentali (parte 2)

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
# Angrist and Evans (1998) - figli e offerta di lavoro della madre
# ====================================================================

data("labsup", package = "wooldridge")
head(labsup)


# --- Modello IV: samesex come strumento per kids ---

iv_labsup <- feols(hours ~ educ + age + black + hispan | kids ~ samesex,
                   data = labsup, vcov = "hetero")

# Primo stadio
summary(iv_labsup, stage = 1)

# Test di rilevanza dello strumento
fitstat(iv_labsup, type = "ivwald")

# Stesso test con linearHypothesis sul primo stadio stimato a mano
first_stage <- feols(kids ~ samesex + educ + age + black + hispan,
                     data = labsup, vcov = "hetero")

linearHypothesis(first_stage, "samesex = 0")

# Secondo stadio
summary(iv_labsup)

# Confronto con stima OLS
ols_labsup <- feols(hours ~ kids + educ + age + black + hispan,
                    data = labsup, vcov = "hetero")

modelsummary(
  list("OLS" = ols_labsup,
       "IV" = iv_labsup),
  gof_omit = "IC|Adj|RMSE"
)


# --- Parto gemellare come strumento: multi2nd ---

iv_labsup_twins <- feols(hours ~ educ + age + black + hispan | kids ~ multi2nd,
                         data = labsup, vcov = "hetero")

# Primo stadio
summary(iv_labsup_twins, stage = 1)

# Test di rilevanza
fitstat(iv_labsup_twins, type = "ivwald")

# Con linearHypothesis
first_stage_twins <- feols(kids ~ multi2nd + educ + age + black + hispan,
                           data = labsup, vcov = "hetero")

linearHypothesis(first_stage_twins, "multi2nd = 0")

# Secondo stadio
summary(iv_labsup_twins, stage = 2)

# Confronto tra i due strumenti
modelsummary(
  list("Hours (IV samesex)"  = iv_labsup,
       "Hours (IV multi2nd)" = iv_labsup_twins),
  gof_omit = "IC|Adj|RMSE"
)


# ====================================================================
# Sovraidentificazione
# ====================================================================

# Modello sovraidentificato con entrambi gli strumenti
iv_labsup_twins_over <- feols(hours ~ educ + age + black + hispan |
                                kids ~ multi2nd + samesex,
                              data = labsup, vcov = "hetero")
iv_labsup_twins_over

# Primo stadio
summary(iv_labsup_twins_over, stage = 1)

# Test di rilevanza congiunto (fitstat usa la F: stat = Wald / m)
fitstat(iv_labsup_twins_over, type = "ivwald")

# Con linearHypothesis: Wald da confrontare con la soglia m x 10 = 20
first_stage_over <- feols(kids ~ multi2nd + samesex + educ + age + black + hispan,
                          data = labsup, vcov = "hetero")

linearHypothesis(first_stage_over, c("multi2nd = 0", "samesex = 0"))


# --- Test J di sovraidentificazione ---
# Stessa procedura usata in Card: residui TSLS, regressione ausiliaria,
# J = m * F, p-value con chi2(m - l). Qui m = 2, l = 1 -> J ~ chi2(1) sotto H0
labsup <- labsup |> mutate(uhat = resid(iv_labsup_twins_over))

Jreg_labsup <- feols(uhat ~ educ + age + black + hispan + samesex + multi2nd,
                     data = labsup, vcov = "iid")

linearHypothesis(Jreg_labsup, c("samesex = 0", "multi2nd = 0"))

# Valore critico chi2(1) al 5% = 3.84 -> J = 0.589 non rifiuta H0
# p-value corretto con df = m - l = 1
1 - pchisq(0.5892, df = 1)

# Verifica con fitstat (procedura interna con d.f. giusti)
fitstat(iv_labsup_twins_over, "sargan")
