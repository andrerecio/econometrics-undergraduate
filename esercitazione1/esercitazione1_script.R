# ==============================================================================
# Esercitazione N.1 - Regressione Lineare Semplice
# Econometria I
# ==============================================================================

# install.packages(kableExtra)

# --- Pacchetti ----------------------------------------------------------------

library(tidyverse)    # dplyr, tidyr, ggplot2, readr
library(knitr)        # tabelle con kable()
library(kableExtra)   # stili per le tabelle
library(modelsummary) # tabelle di regressione
library(wooldridge)   # dataset wage1
library(fixest)       # stima OLS con feols()

# --- Dati ---------------------------------------------------------------------

# Current Population Survey, 1976
data("wage1", package = "wooldridge")

# Prime osservazioni
head(wage1)

# Variabili di interesse:
# wage = retribuzione media oraria in dollari (1976)
# educ = anni di istruzione

# --- Statistiche descrittive --------------------------------------------------

stat <- wage1 %>%
  summarise(
    wage_mean   = mean(wage, na.rm = TRUE),
    wage_median = median(wage, na.rm = TRUE),
    wage_sd     = sd(wage, na.rm = TRUE),
    educ_mean   = mean(educ, na.rm = TRUE),
    educ_median = median(educ, na.rm = TRUE)
  )

stat %>%
  kable(caption = "Statistiche descrittive", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# --- Grafici ------------------------------------------------------------------

# Istogramma di wage
ggplot(wage1, aes(x = wage)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 15) +
  labs(title = "Distribuzione di wage", x = "Wage", y = "Frequenza") +
  theme_minimal()

# Istogramma di educ
ggplot(wage1, aes(x = educ)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 15) +
  labs(title = "Distribuzione dell'istruzione",
       x = "Anni di Istruzione", y = "Frequenza") +
  theme_minimal()

# Scatterplot wage vs educ
ggplot(wage1, aes(y = wage, x = educ)) +
  geom_point(color = "black") +
  theme_minimal()

# Scatterplot con retta di regressione
ggplot(wage1, aes(y = wage, x = educ)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "skyblue", se = FALSE) +
  theme_minimal()

# --- Regressione lineare semplice ---------------------------------------------

# wage_i = beta_0 + beta_1 * educ_i + u_i

# Errori standard robusti all'eteroschedasticità
reg1 <- feols(wage ~ educ, data = wage1, vcov = "hetero")
reg1

# Errori standard omoschedastici (equivalente a lm())
reg1_ho <- feols(wage ~ educ, data = wage1)
reg1_ho

# --- Eteroschedasticità -------------------------------------------------------

# Varianza di wage per valori specifici di educ
educ_valori <- c(4, 8, 12, 16)

varianza_valori <- wage1 %>%
  filter(educ %in% educ_valori) %>%
  group_by(educ) %>%
  summarise(varianza = var(wage, na.rm = TRUE)) %>%
  ungroup()

ggplot(varianza_valori, aes(x = educ, y = varianza)) +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "Varianza del salario per valori specifici di istruzione",
       x = "Anni di istruzione", y = "Varianza di wage") +
  theme_minimal()

# Varianza di wage per 10 gruppi di educ
varianza_educ <- wage1 %>%
  mutate(gruppo_educ = cut(educ, breaks = 10)) %>%
  group_by(gruppo_educ) %>%
  summarise(
    varianza   = var(wage, na.rm = TRUE),
    media_educ = mean(educ, na.rm = TRUE)
  )

ggplot(varianza_educ, aes(x = media_educ, y = varianza)) +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "Varianza del salario per livello di istruzione",
       x = "Anni di istruzione (media per gruppo)",
       y = "Varianza di wage") +
  theme_minimal()

# Confronto errori standard robusti vs omoschedastici
modelsummary(
  list("Wage (robusti)" = reg1, "Wage (omosched.)" = reg1_ho),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# --- Cambio di unità di misura ------------------------------------------------

# Variabile dipendente: wage in centinaia di dollari
wage1 <- wage1 %>%
  mutate(wage_100 = wage / 100)

reg2 <- feols(wage_100 ~ educ, data = wage1, vcov = "hetero")

modelsummary(
  list("Wage ($)" = reg1, "Wage (centinaia $)" = reg2),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Variabile dipendente: wage mensile (7h x 5gg x 4 sett = 140)
wage1 <- wage1 %>%
  mutate(wage_monthly = wage * 140)

reg3 <- feols(wage_monthly ~ educ, data = wage1, vcov = "hetero")

modelsummary(
  list("Wage orario" = reg1, "Wage mensile" = reg3),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Variabile indipendente: educ in mesi
wage1 <- wage1 %>%
  mutate(educ_mesi = educ * 12)

reg4 <- feols(wage ~ educ_mesi, data = wage1, vcov = "hetero")

modelsummary(
  list("Wage (educ anni)" = reg1, "Wage (educ mesi)" = reg4),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# --- Intervalli di confidenza e test di ipotesi -------------------------------

# IC al 95%: beta_1 +/- 1.96 * SE(beta_1)
# 0.541 +/- 1.96 * 0.061 = [0.422, 0.660]
confint(reg1)

# --- Variabili dummy ----------------------------------------------------------

# Tabella di frequenza
tabledummy <- table(wage1$female)
names(tabledummy) <- c("Maschio", "Femmina")

kable(tabledummy, caption = "Numero di Maschi e Femmine") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Statistiche descrittive per genere
stat_genere <- wage1 %>%
  group_by(female) %>%
  summarize(
    wage_mean   = mean(wage, na.rm = TRUE),
    wage_median = median(wage, na.rm = TRUE),
    wage_sd     = sd(wage, na.rm = TRUE),
    educ_mean   = mean(educ, na.rm = TRUE),
    educ_median = median(educ, na.rm = TRUE)
  )

stat_genere %>%
  kable(caption = "Statistiche descrittive per genere", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Media della dummy = proporzione di female nel campione
mean(wage1$female)  # 252/526 ≈ 0.48

# Regressione con dummy female
# wage_i = beta_0 + beta_1 * female_i + u_i
reg_female <- feols(wage ~ female, data = wage1, vcov = "hetero")

modelsummary(
  list("Wage" = reg_female),
  gof_omit = "AIC|BIC|RMSE|R2 Adj.",
  title = "La variabile dipendente è Wage"
)

# Cambio di categoria di riferimento: dummy male
wage1 <- wage1 %>%
  mutate(male = ifelse(female == 1, 0, 1))

# Modo equivalente: male = 1 - female

reg_male <- feols(wage ~ male, data = wage1, vcov = "hetero")

modelsummary(
  list("Female = 1" = reg_female, "Male = 1" = reg_male),
  gof_omit = "AIC|BIC|RMSE|R2 Adj.",
  title = "Female = 1 (col. 1) vs Male = 1 (col. 2)"
)

# Multicollinearità perfetta: male + female = 1 = intercetta
# feols rimuove automaticamente una delle due variabili
reg2dummy <- feols(wage ~ male + female, data = wage1, vcov = "hetero")
reg2dummy