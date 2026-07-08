# ==============================================================================
# Esercitazione N.3 - Nonlinearità: logaritmi
# Econometria I
# ==============================================================================

# --- Pacchetti ----------------------------------------------------------------

library(tidyverse)    # dplyr, ggplot2
library(modelsummary) # tabelle di regressione
library(fixest)       # stima OLS con feols()
library(wooldridge)   # dataset wage1 e campus

# --- Dati ---------------------------------------------------------------------

# Current Population Survey, 1976
data("wage1", package = "wooldridge")

# --- Introduzione: log(wage) ~ educ -------------------------------------------

# Modello log-lineare:
# log(wage_i) = beta_0 + beta_1 * educ_i + u_i
reg_logwage <- feols(log(wage) ~ educ, data = wage1, vcov = "hetero")

# Previsione dal modello log-lineare e ri-trasformazione in scala originale
wage1 <- wage1 %>%
  mutate(
    logwage_pred = predict(reg_logwage),   # valori predetti in log
    wage_pred    = exp(logwage_pred)        # salario predetto in dollari
  )

# Confronto visivo: curva log-lineare vs retta OLS
ggplot(wage1, aes(x = educ)) +
  geom_line(aes(y = wage_pred), color = "#ff7f0e", linewidth = 1.5) +
  geom_smooth(aes(y = wage), method = "lm", se = FALSE,
              color = "#1f77b4", linewidth = 1.5) +
  labs(
    title = "Confronto: regressione lineare vs log-lineare",
    x = "Anni di istruzione",
    y = "Salario orario predetto in dollari"
  ) +
  theme_minimal()

# --- Log-lineare vs livello -----------------------------------------------------

# Regressione in livelli per confronto
reg_wage <- feols(wage ~ educ, data = wage1, vcov = "hetero")

modelsummary(
  list("Livello" = reg_wage, "Log-lineare" = reg_logwage),
  title = "Variabile dipendente: wage o log(wage)",
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Interpretazione:
# - log-lineare: beta_1 * 100 = variazione % di wage per 1 anno in più di educ
#   -> un anno in più di istruzione è associato a +8.3% del salario orario
# - R^2 non è confrontabile tra le due regressioni: la Y è diversa
#   (wage vs log(wage))

# --- Log-log e lineare-log ------------------------------------------------------

reg_logwagelog <- feols(log(wage) ~ log(educ), data = wage1, vcov = "hetero")
reg_wagelog    <- feols(wage ~ log(educ),      data = wage1, vcov = "hetero")

modelsummary(
  list("Log-lineare" = reg_logwage,
       "Log-log"   = reg_logwagelog,
       "Lineare-log" = reg_wagelog),
  title = "Variabile dipendente: log(wage) o wage",
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Interpretazione:
# - log-lineare:  +1 anno di educ -> +8.3% di wage
# - log-log:    +1% di educ    -> +0.825% di wage  (elasticità)
# - lineare-log:  +1% di educ    -> +0.053 dollari di wage

# --- Cambio di unità di misura: Y in decine di dollari ------------------------

# Con Y in log, il cambio di unità di misura lascia invariati i coefficienti:
# solo l'intercetta cambia (diminuisce di log(10)).
# Nel modello lineare-log anche il coefficiente cambia perché Y è in decine.

reg_logwage_dec    <- feols(log(wage/10) ~ educ,      data = wage1, vcov = "hetero")
reg_logwagelog_dec <- feols(log(wage/10) ~ log(educ), data = wage1, vcov = "hetero")
reg_wagelog_dec    <- feols(wage/10 ~ log(educ),      data = wage1, vcov = "hetero")

modelsummary(
  list("Log-lineare" = reg_logwage_dec,
       "Log-log"   = reg_logwagelog_dec,
       "Lineare-log" = reg_wagelog_dec),
  title = "Variabile dipendente: log(wage/10) o wage/10",
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# --- Cambio di unità di misura: X in mesi di istruzione -----------------------

# educ_monthly = educ * 12
wage1 <- wage1 %>%
  mutate(educ_monthly = educ * 12)

reg_logwage_monthly    <- feols(log(wage) ~ educ_monthly,      data = wage1, vcov = "hetero")
reg_logwagelog_monthly <- feols(log(wage) ~ log(educ_monthly), data = wage1, vcov = "hetero")
reg_wagelog_monthly    <- feols(wage ~ log(educ_monthly),      data = wage1, vcov = "hetero")

modelsummary(
  list("Log-lineare" = reg_logwage_monthly,
       "Log-log"   = reg_logwagelog_monthly,
       "Lineare-log" = reg_wagelog_monthly),
  title = "Variabile dipendente: log(wage) o wage (educ in mesi)",
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Nota sulle intercette:
# log(educ * 12) = log(educ) + log(12), quindi il termine log(12)
# viene assorbito dall'intercetta.
# - log-log:   beta_0' = -0.445 - 0.825 * log(12) ≈ -2.495
# - lineare-log: beta_0' = -7.460 - 5.330 * log(12) ≈ -20.70

# --- Variabili dummy e logaritmi ----------------------------------------------

# Costruiamo tre dummy mutuamente esclusive.
# Categoria omessa: uomini single (female = 0 & married = 0)
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
  title = "Log(wage) con dummy di stato civile e genere",
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Interpretazione (differenza % rispetto agli uomini single,
# a parità di educ, exper e tenure):
# - marrmale:   uomini sposati guadagnano ~29.2% in più
# - marrfemale: donne sposate guadagnano ~12.0% in meno
# - singfem:    donne single guadagnano ~9.7% in meno

# --- Esempio: Campus Crime ----------------------------------------------------

# Dataset: 97 college e università
# crime  = numero annuale di crimini nel campus
# enroll = numero di iscritti
# police = numero di agenti di polizia del campus
data("campus", package = "wooldridge")

reg_log_crime  <- feols(log(crime) ~ log(enroll),               data = campus, vcov = "hetero")
reg_log_crime2 <- feols(log(crime) ~ log(enroll) + log(police), data = campus, vcov = "hetero")

modelsummary(
  list("Log Crime" = reg_log_crime,
       "Log Crime (con police)" = reg_log_crime2),
  title = "Campus Crime",
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Interpretazione:
# - +1% di iscritti -> +1.27% di crimini (senza controllo per police)
# - +1% di iscritti -> +0.92% di crimini a parità di police
#
# Limite: stiamo ignorando altri fattori (dimensione città, reddito locale, ecc.)
# che potrebbero essere correlati sia con enroll sia con crime.
