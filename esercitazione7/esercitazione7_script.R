# Esercitazione N.7 - Modelli Probit e Logit
# Andrea Recine


# Setup ------------------------------------------------------------------------

library(tidyverse)
library(knitr)
library(modelsummary)
library(fixest)
library(wooldridge)
library(marginaleffects)


# Modello Probit ---------------------------------------------------------------

# Carichiamo i dati alcohol dal pacchetto wooldridge
data(alcohol, package = "wooldridge")

# Stima del modello Probit con la stessa specificazione del LPM
# I coefficienti NON sono effetti marginali: vanno trasformati con phi(Xb) * beta
# beta_j dà solo il segno dell'effetto marginale, non la grandezza
probit_alcohol <- feglm(employ ~ abuse + educ + age + married,
                        data = alcohol,
                        family = "probit")
modelsummary(list("Employ (Probit)" = probit_alcohol),
             gof_omit = "AIC|BIC|RMSE")


# Modello Logit ----------------------------------------------------------------

# Stima del modello Logit con la stessa specificazione
# Probit e Logit producono risultati simili; differiscono soprattutto nelle code
# (il Logit ha code più pesanti)
logit_alcohol <- feglm(employ ~ abuse + educ + age + married,
                       data = alcohol,
                       family = "logit")
modelsummary(list("Employ (Logit)" = logit_alcohol),
             gof_omit = "AIC|BIC|RMSE")


# Confronto grafico delle curve di link ----------------------------------------

# Le tre funzioni Pr(Y=1 | X*beta) in funzione dell'indice lineare X*beta
# - LPM: retta, può uscire da [0,1]
# - Probit e Logit: forma a S, sempre in [0,1]
# - Logit: code più pesanti del Probit
z <- seq(-5, 5, length.out = 400)
curve_df <- tibble(
  z = z,
  Probit = pnorm(z),
  Logit  = plogis(z),
  LPM    = 0.5 + 0.1 * z
) |>
  pivot_longer(-z, names_to = "Modello", values_to = "p")

ggplot(curve_df, aes(x = z, y = p, color = Modello)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "grey60") +
  scale_color_manual(values = c("LPM" = "skyblue",
                                "Probit" = "darkblue",
                                "Logit" = "lightblue")) +
  labs(x = expression(X * beta), y = "Pr(Y = 1)",
       title = "LPM, Probit e Logit") +
  theme_minimal()


# Calcolo manuale della probabilità predetta -----------------------------------

# Estraiamo i coefficienti del Probit per costruire l'indice lineare X*beta
b <- coef(probit_alcohol)
b

# Individuo A: age=35, educ=14, married=1, abuse=0
xb_A <- b["(Intercept)"] + b["abuse"]*0 + b["educ"]*14 +
        b["age"]*35 + b["married"]*1
unname(xb_A)

# Probabilità predetta: applichiamo Phi (pnorm) all'indice lineare
prob_A <- pnorm(xb_A)
unname(prob_A)

# Individuo B: profilo "opposto" (non sposato, abusa, poca istruzione,
# età avanzata). Qui X*beta < 0, quindi pnorm(X*beta) < 0.5
xb_B <- b["(Intercept)"] + b["abuse"]*1 + b["educ"]*4 +
        b["age"]*55 + b["married"]*0
unname(xb_B)

prob_B <- pnorm(xb_B)
unname(prob_B)

# Regola pratica: Phi(0) = 0.5 e Phi è strettamente crescente
# => segno(X*beta) determina se la probabilità predetta è >, =, < 0.5
# Stessa regola vale per il Logit (con Lambda al posto di Phi)

# Verifica: predict() restituisce gli stessi valori dei due individui
predict(probit_alcohol,
        newdata = data.frame(abuse   = c(0,  1),
                             educ    = c(14, 4),
                             age     = c(35, 55),
                             married = c(1,  0)))

# Grafico: posizione dei due individui sulla curva Phi
# Linee tratteggiate su X*beta=0 e Phi=0.5 evidenziano il punto di simmetria
punti <- tibble(
  individuo = c("A", "B"),
  xb        = c(xb_A, xb_B),
  p         = c(prob_A, prob_B)
)

ggplot(tibble(z = seq(-3, 3, length.out = 400)),
       aes(x = z, y = pnorm(z))) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0,   linetype = "dashed", color = "grey50") +
  geom_point(data = punti, aes(x = xb, y = p),
             color = "skyblue", size = 3) +
  geom_text(data = punti, aes(x = xb, y = p, label = individuo),
            nudge_y = 0.05, fontface = "bold") +
  labs(x = expression(X * beta), y = expression(Phi(X * beta)),
       title = "Indice lineare e probabilità predetta") +
  theme_minimal()


# Odds ratio -------------------------------------------------------------------

# Nel Logit:
#   odds(X)   = p/(1-p) = exp(X*beta)
#   log-odds  = X*beta  (lineare nelle X)
#   OR_j      = odds(X_j+1) / odds(X_j) = exp(beta_j)
# L'OR dipende solo da beta_j: è costante rispetto ai livelli di X
# Attenzione: l'odds ratio NON è una probabilità
exp(coef(logit_alcohol)) |> round(3)


# Effetti Marginali Medi (AME) - calcolo manuale -------------------------------

# Per una dummy X_j, l'effetto marginale è la differenza tra la probabilità
# predetta quando X_j=1 e quando X_j=0. L'AME è la media di queste differenze
# su tutti gli individui del campione.
# Idea: creare due copie del dataset, una con abuse=1 e una con abuse=0,
# poi predire la probabilità per ciascuna e fare la differenza media
alcohol_tmp1 <- alcohol
alcohol_tmp0 <- alcohol
alcohol_tmp1$abuse <- 1
alcohol_tmp0$abuse <- 0

# AME manuale per il modello Probit
proba_employ_probit    <- predict(probit_alcohol, newdata = alcohol_tmp1)
proba_nonemploy_probit <- predict(probit_alcohol, newdata = alcohol_tmp0)
ame_manual_probit <- mean(proba_employ_probit - proba_nonemploy_probit)
print(paste("Effetto marginale medio manuale (Probit):",
            round(ame_manual_probit, 4)))

# AME manuale per il modello Logit
proba_employ_logit    <- predict(logit_alcohol, newdata = alcohol_tmp1)
proba_nonemploy_logit <- predict(logit_alcohol, newdata = alcohol_tmp0)
ame_manual_logit <- mean(proba_employ_logit - proba_nonemploy_logit)
print(paste("Effetto marginale medio manuale (Logit):",
            round(ame_manual_logit, 4)))


# Effetti Marginali Medi (AME) - con marginaleffects ---------------------------

# avg_slopes() senza newdata calcola gli effetti marginali medi
# (effetto per ogni osservazione, poi media)
# Vantaggio: errori standard automatici (metodo delta)
all_effects_logit  <- avg_slopes(logit_alcohol)
all_effects_probit <- avg_slopes(probit_alcohol)

all_effects_logit
all_effects_probit

# Tabella di confronto AME Logit vs AME Probit
modelsummary(list("Logit (AME)"  = all_effects_logit,
                  "Probit (AME)" = all_effects_probit),
             gof_omit = "AIC|BIC|RMSE|R2|Adj")

# Interpretazione AME (Probit):
# - abuse:   -1.9 p.p. di probabilità di essere occupato
# - educ:    +1.5 p.p. per anno aggiuntivo di istruzione
# - married: +8.9 p.p. rispetto a chi non è sposato


# Effetti Marginali al Punto Medio (MEM) ---------------------------------------

# avg_slopes() con newdata = "mean" calcola gli effetti marginali in un
# unico punto: quello con tutte le X poste alla media campionaria
mem_probit <- avg_slopes(probit_alcohol, newdata = "mean")
mem_logit  <- avg_slopes(logit_alcohol,  newdata = "mean")

# Tabella di confronto AME vs MEM per entrambi i modelli
# AME è generalmente preferito (media degli effetti individuali);
# MEM è meno interpretabile per le dummy (media frazionaria non corrisponde
# a un individuo realmente osservato)
modelsummary(
  list("Probit (AME)" = all_effects_probit,
       "Probit (MEM)" = mem_probit,
       "Logit (AME)"  = all_effects_logit,
       "Logit (MEM)"  = mem_logit),
  gof_omit = "AIC|BIC|RMSE|R2|Adj"
)

# Distribuzione degli effetti marginali individuali (Logit) con MEM
slopes_logit <- slopes(logit_alcohol)

ggplot(slopes_logit, aes(x = estimate)) +
  geom_histogram(bins = 60, fill = "lightblue", color = "white") +
  geom_vline(data = mem_logit, aes(xintercept = estimate),
             color = "darkblue", linewidth = 0.8) +
  facet_wrap(contrast ~ term, scales = "free") +
  labs(title = "Distribuzione degli effetti marginali individuali (Logit)",
       subtitle = "La linea verticale è l'effetto marginale al punto medio (MEM)",
       x = "Effetto marginale",
       y = "Frequenza") +
  theme_minimal()

# Stesso grafico ma con AME come linea verticale
# L'AME cade nel "centro di massa" della distribuzione (media aritmetica)
ggplot(slopes_logit, aes(x = estimate)) +
  geom_histogram(bins = 60, fill = "lightblue", color = "white") +
  geom_vline(data = all_effects_logit, aes(xintercept = estimate),
             color = "darkblue", linewidth = 0.8) +
  facet_wrap(contrast ~ term, scales = "free") +
  labs(title = "Distribuzione degli effetti marginali individuali (Logit)",
       subtitle = "La linea verticale è l'effetto marginale medio (AME)",
       x = "Effetto marginale",
       y = "Frequenza") +
  theme_minimal()


# Confronto LPM, Probit e Logit - dataset alcohol ------------------------------

# Stima del LPM con errori standard robusti all'eteroschedasticità
reg_lpm_alcohol <- feols(employ ~ abuse + educ + age + married,
                         data = alcohol,
                         vcov = "hetero")

# Sul dataset alcohol i tre modelli danno risultati molto simili
modelsummary(list("Employ (LPM)"        = reg_lpm_alcohol,
                  "Employ AME (Probit)" = all_effects_probit,
                  "Employ AME (Logit)"  = all_effects_logit),
             gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Confronto LPM, Probit e Logit - dataset crime1 -------------------------------

# Carichiamo i dati crime1 e creiamo la dummy 'arrested'
# (1 se arrestato almeno una volta nel 1986, 0 altrimenti)
data("crime1", package = "wooldridge")
head(crime1)

crime1 <- crime1 %>%
  mutate(arrested = ifelse(narr86 > 0, 1, 0))

# LPM con errori standard robusti
reg_lpm_crime <- feols(arrested ~ pcnv + avgsen + tottime + ptime86 + qemp86 + black + hispan,
                       data = crime1,
                       vcov = "hetero")
modelsummary(list("Arrested" = reg_lpm_crime),
             gof_omit = "AIC|BIC|RMSE|R2 Adj.")

# Probit e Logit (coefficienti grezzi, non confrontabili col LPM)
probit_crime <- feglm(arrested ~ pcnv + avgsen + tottime + ptime86 + qemp86 + black + hispan,
                      data = crime1,
                      family = "probit")
logit_crime  <- feglm(arrested ~ pcnv + avgsen + tottime + ptime86 + qemp86 + black + hispan,
                      data = crime1,
                      family = "logit")
modelsummary(list("Arrested (LPM)"    = reg_lpm_crime,
                  "Arrested (Probit)" = probit_crime,
                  "Arrested (Logit)"  = logit_crime),
             gof_omit = "AIC|BIC|RMSE|R2 Adj.")

# AME di Probit e Logit per renderli confrontabili col LPM
ame_crime_probit <- avg_slopes(probit_crime)
ame_crime_logit  <- avg_slopes(logit_crime)

ame_crime_probit
ame_crime_logit

modelsummary(list("Arrested (LPM)"        = reg_lpm_crime,
                  "Arrested (Probit AME)" = ame_crime_probit,
                  "Arrested (Logit AME)"  = ame_crime_logit),
             gof_omit = "AIC|BIC|RMSE|R2 Adj.")

# La probabilità media di essere arrestato è lontana da 0.5
# -> i coefficienti grezzi di Probit/Logit hanno scala diversa dal LPM,
#    ma gli AME diventano confrontabili e qualitativamente concordi
mean(crime1$arrested) |> round(3)
