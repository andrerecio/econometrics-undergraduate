# ==============================================================================
# Esercitazioni di Econometria — Introduzione a R
# Sapienza Università di Roma
# ==============================================================================

# --- Installazione pacchetti (solo la prima volta) ----------------------------

install.packages("tidyverse")
install.packages("wooldridge")
#install.packages("ggplot2")
#install.packages("dplyr")

# --- Caricare i pacchetti -----------------------------------------------------

library(tidyverse)
library(wooldridge)

# ==============================================================================
# 1. Primi passi
# ==============================================================================

x <- 1:10
y <- x^2
plot(x, y)

# ==============================================================================
# 2. Dataset: wage1
# ==============================================================================

data("wage1", package = "wooldridge")
str(wage1)

# ==============================================================================
# 3. Manipolare i dati con dplyr
# ==============================================================================

# Filtriamo le osservazioni con più di 12 anni di istruzione
wage_educ_higher <- wage1 |>
  filter(educ > 12)

head(wage_educ_higher)

# ==============================================================================
# 4. Statistiche descrittive
# ==============================================================================

wage1 |>
  summarise(
    wage_mean   = mean(wage),
    wage_median = median(wage),
    wage_sd     = sd(wage),
    educ_mean   = mean(educ)
  )

# --- Per gruppi ---------------------------------------------------------------

# female = 0 → uomini, female = 1 → donne
wage1 |>
  group_by(female) |>
  summarise(
    wage_mean = mean(wage),
    wage_sd   = sd(wage),
    n         = n()
  )

# ==============================================================================
# 5. Intervallo di confidenza per la media
# ==============================================================================

# --- Calcolo a mano -----------------------------------------------------------

n    <- length(wage1$wage)
xbar <- mean(wage1$wage)
s    <- sd(wage1$wage)
se   <- s / sqrt(n)

t_crit   <- qt(0.975, df = n - 1)  # uguale a circa 1.96

ci_lower <- xbar - t_crit * se
ci_upper <- xbar + t_crit * se

c(media = xbar, lower = ci_lower, upper = ci_upper)

# --- Verifica con t.test() ----------------------------------------------------

t.test(wage1$wage)


# --- Verifica della differenza tra medie --------------------------------------

# Statistiche descrittive per gruppo
# female = 0 → uomini, female = 1 → donne
wagediff <- wage1 |>
  group_by(female) |>
  summarise(
    wage_mean = mean(wage),  # media salariale per gruppo
    wage_sd   = sd(wage),    # deviazione standard per gruppo
    n         = n()          # numerosità per gruppo
  )
wagediff

# Errore standard della differenza tra medie
# SE = sqrt(s1^2/n1 + s2^2/n2)
se <- sqrt(wagediff$wage_sd[1]^2 / wagediff$n[1] +
             wagediff$wage_sd[2]^2 / wagediff$n[2])

# Statistica t: (media_uomini - media_donne) / SE
# H0: mu_uomini - mu_donne = 0
# H1: mu_uomini - mu_donne ≠ 0
t_stat <- (wagediff$wage_mean[1] - wagediff$wage_mean[2]) / se
t_stat

# Rifiutiamo H0 al 5% se |t| > 1.96

# ==============================================================================
# 6. Grafici con ggplot2
# ==============================================================================

# Distribuzione del salario orario
ggplot(wage1, aes(x = wage)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  labs(
    title = "Distribuzione del salario orario",
    x = "Salario orario (dollari)",
    y = "Frequenza"
  ) +
  theme_minimal(base_size = 15)

# ==============================================================================
# 7. Polizia e crimine
# ==============================================================================

# Carichiamo il dataset da GitHub
# In alternativa, scaricatelo e importatelo con Import Dataset
url_data <- "https://raw.githubusercontent.com/andrerecio/econometria-triennale/main/intro/crime2_clean.csv"

crime <- read_csv(url_data, show_col_types = FALSE, na = ".")
glimpse(crime)

# --- Creiamo variabili per 100.000 abitanti -----------------------------------

crime_sub <- crime |>
  filter(year == 1991) |>
  mutate(
    violent = (murder + rape + robbery + assault) / citypop * 100000,
    police  = sworn / citypop * 100000
  ) |>
  drop_na(violent, police)

# --- Scatter plot: polizia vs crimine violento --------------------------------

ggplot(crime_sub, aes(x = police, y = violent)) +
  geom_point(alpha = 0.8) +
  labs(
    x = "Poliziotti per 100.000 abitanti",
    y = "Crimini violenti per 100.000 abitanti"
  ) +
  theme_minimal(base_size = 20)

# ==============================================================================
# 8. Covarianza e correlazione
# ==============================================================================

# --- Con le funzioni R --------------------------------------------------------

cov(crime_sub$police, crime_sub$violent)
cor(crime_sub$police, crime_sub$violent)

# --- Calcolo a mano ----------------------------------------------------------

x <- crime_sub$police
y <- crime_sub$violent

cov_manual <- sum((x - mean(x)) * (y - mean(y))) / (length(x) - 1)
cor_manual <- cov_manual / (sd(x) * sd(y))

c(covarianza = round(cov_manual, 2), correlazione = round(cor_manual, 4))