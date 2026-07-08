# =============================================================================
# Esercitazione 2 — Regressione lineare semplice
# Econometria — Sapienza Università di Roma
# Dataset: wage1 (Current Population Survey 1976)
# =============================================================================

# --- Installazione nuove librerie --------------------------------

install.packages("fixest")       # regressione con errori standard robusti
install.packages("modelsummary") # tabelle di regressione


# --- Librerie -----------------------------------------------------------------

library(tidyverse)    # manipolazione dati e grafici
library(wooldridge)   # dataset
library(fixest)       # regressione con errori standard robusti
library(modelsummary) # tabelle di regressione


# --- Dati ---------------------------------------------------------------------

data("wage1", package = "wooldridge")


# --- Struttura dei dati -------------------------------------------------------

# prime 6 righe del dataset
head(wage1)

# variabili principali:
# wage  — retribuzione oraria media (dollari, 1976)
# educ  — anni di istruzione


# --- Statistiche descrittive --------------------------------------------------

wage1 |>
  summarise(
    wage_mean   = mean(wage),
    wage_median = median(wage),
    wage_sd     = sd(wage),
    educ_mean   = mean(educ),
    educ_median = median(educ),
    educ_sd     = sd(educ)
  )


# --- Visualizzazione ----------------------------------------------------------

# Distribuzione del salario orario
ggplot(wage1, aes(x = wage)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  labs(title = "Distribuzione del salario orario",
       x = "Salario orario (dollari)", y = "Frequenza") +
  theme_minimal(base_size = 13)

# Distribuzione degli anni di istruzione
ggplot(wage1, aes(x = educ)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  labs(title = "Distribuzione degli anni di istruzione",
       x = "Anni di istruzione", y = "Frequenza") +
  theme_minimal(base_size = 13)

# Scatter plot: relazione tra wage ed educ
# geom_smooth(method = "lm") aggiunge la retta di regressione
ggplot(wage1, aes(y = wage, x = educ)) +
  geom_point(color = "black", alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(x = "Anni di istruzione",
       y = "Salario orario (dollari)") +
  theme_minimal(base_size = 13)


# --- Regressione lineare semplice ---------------------------------------------

# Modello: wage_i = beta0 + beta1 * educ_i + u_i
#
# feols() di fixest stima OLS con errori standard robusti
# vcov = "hetero" (equivalente a "hc1") — robusti all'eteroschedasticità
# Stata: regress wage educ, robust

reg1 <- feols(wage ~ educ, data = wage1, vcov = "hetero")
reg1

# Alternativa con lm() base di R — errori standard omoschedastici
# Per errori robusti con lm() serve library(sandwich) e vcovHC()
reg_lm <- lm(wage ~ educ, data = wage1)
summary(reg_lm)


# --- Tabella della regressione ------------------------------------------------

# modelsummary() produce una tabella formattata dei risultati
# gof_omit rimuove le statistiche di fit non necessarie
modelsummary(list("wage" = reg1), gof_omit = "AIC|BIC|RMSE|R2 Adj.")

# Interpretazione:
# beta1 = 0.54: un anno aggiuntivo di istruzione è associato a un aumento
#               medio del salario orario di circa 0.54 dollari
# beta0 = -0.90: salario orario medio predetto per un individuo con
#                zero anni di istruzione (scarso significato empirico)