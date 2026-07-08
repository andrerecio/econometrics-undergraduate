# =============================================================================
# Esercitazione N.2 - Regressione Multipla
# =============================================================================

# -----------------------------------------------------------------------------
# Setup: librerie e dati
# -----------------------------------------------------------------------------

library(tidyverse)
library(knitr)
library(modelsummary)
library(wooldridge)
library(fixest)

# Carichiamo il dataset wage1 dal pacchetto wooldridge
data("wage1", package = "wooldridge")


# -----------------------------------------------------------------------------
# 1. Regressione multipla: educ, exper, tenure
# -----------------------------------------------------------------------------

# Modello 1: regressione semplice (solo educ)
reg_wage1 <- feols(wage ~ educ, data = wage1, vcov = "hetero")

# Modello 2: aggiungiamo exper (anni di esperienza nel mercato del lavoro)
reg_wage2 <- feols(wage ~ educ + exper, data = wage1, vcov = "hetero")

# Modello 3: aggiungiamo tenure (anni con l'attuale datore di lavoro)
reg_wage3 <- feols(wage ~ educ + exper + tenure, data = wage1, vcov = "hetero")

# Confronto dei tre modelli
modelsummary(
  list("Wage" = reg_wage1, "Wage" = reg_wage2, "Wage" = reg_wage3),
  gof_omit = "AIC|BIC|RMSE"
)


# -----------------------------------------------------------------------------
# 2. Correlazione tra educ e exper
# -----------------------------------------------------------------------------

# La correlazione fra educ e exper è negativa: chi ha più anni di istruzione
# tende ad entrare nel mercato del lavoro più tardi e quindi ha meno esperienza.
# Conseguenza: nel modello semplice il coefficiente di educ è distorto verso
# il basso (sottostima l'effetto dell'istruzione sul salario).
cor(wage1$educ, wage1$exper)


# -----------------------------------------------------------------------------
# 3. Includiamo una dummy: female
# -----------------------------------------------------------------------------

# Modello 4: aggiungiamo la dummy female al modello con educ, exper, tenure
reg_wage4 <- feols(wage ~ educ + exper + tenure + female,
                   data = wage1, vcov = "hetero")

modelsummary(
  list("Wage" = reg_wage3, "Wage" = reg_wage4),
  gof_omit = "AIC|BIC|RMSE"
)


# -----------------------------------------------------------------------------
# 4. Cambio unità di misura: wage mensile
# -----------------------------------------------------------------------------

# Salario mensile: assumiamo lavoratori a tempo pieno
# 7 ore/giorno x 5 giorni/settimana x 4 settimane/mese = 140 ore/mese
wage1 <- wage1 %>%
  mutate(wage_monthly = wage * 140)

# Stimiamo lo stesso modello con wage_monthly come variabile dipendente
reg_wage_dummy3 <- feols(wage_monthly ~ educ + exper + tenure + female,
                         data = wage1, vcov = "hetero")

# Confronto: i coefficienti del modello mensile sono i coefficienti orari x 140
modelsummary(
  list("Wage" = reg_wage4, "Wage Monthly" = reg_wage_dummy3),
  gof_omit = "AIC|BIC|RMSE"
)


# -----------------------------------------------------------------------------
# 5. Dummy: wage e genere
# -----------------------------------------------------------------------------

# Regressione semplice: solo female (come in Esercitazione 1)
reg_wage_dummy <- feols(wage ~ female, data = wage1, vcov = "hetero")

# Regressione con controlli per educ, exper, tenure
reg_wage_dummy2 <- feols(wage ~ female + educ + exper + tenure,
                         data = wage1, vcov = "hetero")

modelsummary(
  list("Wage (Esercitazione 1)" = reg_wage_dummy, "Wage" = reg_wage_dummy2),
  gof_omit = "AIC|BIC|RMSE"
)


# -----------------------------------------------------------------------------
# 6. Dummy con categorie multiple: stato civile e genere
# -----------------------------------------------------------------------------

# Creiamo tre dummy. Il gruppo di riferimento (omesso) sono gli uomini single.
#  - marrmale:   uomo sposato
#  - marrfemale: donna sposata
#  - singfem:    donna single
wage1 <- wage1 %>%
  mutate(
    marrmale   = ifelse(female == 0 & married == 1, 1, 0),
    marrfemale = ifelse(female == 1 & married == 1, 1, 0),
    singfem    = ifelse(female == 1 & married == 0, 1, 0)
  )

# Le medie delle dummy corrispondono alle proporzioni nel campione
statdummy <- wage1 %>%
  summarise(
    marrmale_mean   = mean(marrmale,   na.rm = TRUE),
    marrfemale_mean = mean(marrfemale, na.rm = TRUE),
    singfem_mean    = mean(singfem,    na.rm = TRUE)
  )
statdummy

# Regressione con le tre dummy + controlli
# Il coefficiente di ciascuna dummy va interpretato rispetto al gruppo
# di riferimento (uomini single).
reg_wage_sm <- feols(
  wage ~ marrmale + marrfemale + singfem + educ + exper + tenure,
  data = wage1, vcov = "hetero"
)

modelsummary(list("Wage" = reg_wage_sm), gof_omit = "AIC|BIC|RMSE")


# -----------------------------------------------------------------------------
# 7. Trappola delle variabili dummy
# -----------------------------------------------------------------------------

# Creiamo la dummy male = 1 per gli uomini, 0 per le donne
wage1 <- wage1 %>%
  mutate(male = 1 - female)

# Includendo female e male insieme alle altre dummy si genera collinearità
# perfetta. R rimuove automaticamente le variabili ridondanti.
reg_wage_sm_dummy <- feols(
  wage ~ marrmale + marrfemale + singfem + female + male,
  data = wage1, vcov = "hetero"
)
reg_wage_sm_dummy


# -----------------------------------------------------------------------------
# 7.1 Dummy per regione geografica
# -----------------------------------------------------------------------------

# Il dataset wage1 contiene tre dummy regionali: northcen, south, west.
# La quarta regione (Northeast) è il gruppo di riferimento omesso,
# cioè le osservazioni per cui tutte e tre le dummy sono uguali a zero.

# Conteggio delle osservazioni per regione
# Nota: usiamo nomi nuovi (n_*) per evitare il name masking di dplyr.
# Se chiamassimo le variabili di output come quelle originali (northcen, 
# south, west), dplyr le "sostituirebbe" nella riga successiva 
# producendo risultati sbagliati.
wage1 |>
  summarise(
    n_northcen  = sum(northcen),
    n_south     = sum(south),
    n_west      = sum(west),
    n_northeast = sum(1 - northcen - south - west)
  ) |>
  tt(caption = "Osservazioni per regione")

# La somma delle quattro categorie coincide con il numero totale di 
# osservazioni (526), perché le regioni sono mutuamente esclusive 
# ed esaustive.


# Statistiche descrittive per regione
# Ricostruiamo una variabile categorica "regione" a partire dalle dummy
# usando case_when(), poi aggreghiamo per gruppo.
wage1 |>
  mutate(
    regione = case_when(
      northcen == 1 ~ "North Central",
      south    == 1 ~ "South",
      west     == 1 ~ "West",
      TRUE          ~ "Northeast"   # default: tutte le dummy a zero
    )
  ) |>
  group_by(regione) |>
  summarise(
    n            = n(),
    wage_media   = mean(wage),
    wage_mediana = median(wage),
    educ_media   = mean(educ)
  ) |>
  arrange(desc(wage_media)) |>    # ordina per salario medio decrescente
  tt(caption = "Statistiche descrittive per regione") |>
  format_tt(digits = 3) |>
  theme_tt("striped")

# Queste sono differenze grezze: non tengono conto delle differenze 
# nella composizione della forza lavoro (istruzione, esperienza, 
# genere, tenure) tra le regioni.


# Regressione con dummy regionali
# Includiamo solo 3 dummy su 4 regioni per evitare collinearità 
# perfetta con l'intercetta (trappola delle dummy). Il Northeast 
# resta implicito come gruppo di riferimento.
reg_region <- feols(
  wage ~ educ + exper + tenure + female + northcen + south + west,
  data = wage1, vcov = "hetero"
)

# Ciascun coefficiente sulle dummy regionali va letto come differenza 
# media rispetto al Northeast, a parità di istruzione, esperienza, 
# tenure e genere.
modelsummary(
  list("Wage" = reg_region),
  gof_omit = "AIC|BIC|RMSE|R2 Adj.",
  title = "Regressione con dummy regionali (Northeast = gruppo di riferimento)"
)



# -----------------------------------------------------------------------------
# 8. Test di ipotesi congiunte
# -----------------------------------------------------------------------------

# Vogliamo testare H0: i coefficienti di marrmale, marrfemale e singfem
# sono tutti uguali a zero (cioè non ci sono differenze salariali fra
# le categorie a parità di educ, exper, tenure).

# Opzione A: funzione wald() del pacchetto fixest
wald(reg_wage_sm, keep = "marrmale|marrfemale|singfem")

# Opzione B: linearHypothesis() del pacchetto car (la useremo più spesso)
# install.packages("car")  # se non già installato
library(car)

# Test F esplicito
linearHypothesis(
  reg_wage_sm,
  c("marrmale = 0", "marrfemale = 0", "singfem = 0"),
  test = "F"
)

# Senza specificare test = "F" (di default usa la statistica chi-quadro)
linearHypothesis(
  reg_wage_sm,
  c("marrmale = 0", "marrfemale = 0", "singfem = 0")
)