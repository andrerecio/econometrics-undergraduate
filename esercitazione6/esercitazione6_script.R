# Esercitazione N.6 - Linear Probability Model (LPM)
# Andrea Recine


# Setup ------------------------------------------------------------------------

library(tidyverse)
library(knitr)
library(modelsummary)
library(fixest)
library(wooldridge)


# Dataset alcohol --------------------------------------------------------------

# Carichiamo i dati alcohol dal pacchetto wooldridge
data("alcohol", package = "wooldridge")
head(alcohol)

# Statistiche descrittive
datasummary_skim(alcohol)

# Stima del LPM: employ ~ abuse + educ + age + married
# Errori standard robusti all'eteroschedasticità (intrinseca nel LPM)
reg_lpm_alcohol <- feols(employ ~ abuse + educ + age + married,
                         data = alcohol,
                         vcov = "hetero")
modelsummary(list("Employ" = reg_lpm_alcohol),
             gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Probabilità predette ---------------------------------------------------------

# Uomo di 30 anni, sposato, 12 anni di istruzione, non abusa di alcol
predict(reg_lpm_alcohol,
        newdata = data.frame(abuse = 0, educ = 12, age = 30, married = 1))

# Uomo di 30 anni, non sposato, 10 anni di istruzione, abusa di alcol
predict(reg_lpm_alcohol,
        newdata = data.frame(abuse = 1, educ = 10, age = 30, married = 0))


# Dataset crime1 - Probabilità di essere arrestato -----------------------------

# Carichiamo i dati crime1 dal pacchetto wooldridge
data("crime1", package = "wooldridge")
head(crime1)

# Creazione della dummy 'arrested': 1 se l'individuo è stato arrestato
# almeno una volta nel 1986, 0 altrimenti
crime1 <- crime1 %>%
  mutate(arrested = ifelse(narr86 > 0, 1, 0))

# Stima del LPM con errori standard robusti all'eteroschedasticità
# Categoria omessa per le dummy etniche: uomini bianchi/altra etnia
# (black = 0 e hispan = 0). I coefficienti di black e hispan si leggono
# rispetto a questo gruppo di riferimento.
reg_lpm_crime <- feols(arrested ~ pcnv + avgsen + tottime + ptime86 + qemp86 + black + hispan,
                       data = crime1,
                       vcov = "hetero")
modelsummary(list("Arrested" = reg_lpm_crime),
             gof_omit = "AIC|BIC|RMSE|R2 Adj.")
