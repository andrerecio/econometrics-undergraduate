# Esercitazione 4 — Nonlinearità: polinomi

# Setup ----
library(tidyverse)
library(knitr)
library(tinytable)
library(modelsummary)
library(fixest)
library(wooldridge)

data("wage1", package = "wooldridge")


# Introduzione: regressione di wage su exper + exper^2 ----
# Nota: in fixest l'operatore ^ è elevamento a potenza, quindi exper^2 == I(exper^2).
# In lm/glm/lme4 invece serve I(exper^2): senza I() il termine quadratico
# verrebbe ignorato silenziosamente.
reg_wage_exper <- feols(wage ~ exper + exper^2, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_exper), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Grafico: lineare vs polinomio quadratico ----
# 1. Previsione dei valori stimati
wage1 <- wage1 %>%
  mutate(wageexp_pred = predict(reg_wage_exper))

# 2. Filtro per exper ≤ 40
wage_cut <- wage1 %>% filter(exper <= 40)

# 3. Trova il punto massimo della curva wageexp_pred
punto_max <- wage_cut %>%
  filter(wageexp_pred == max(wageexp_pred, na.rm = TRUE))

# 4. Grafico
ggplot(wage_cut, aes(x = exper)) +
  geom_line(aes(y = wageexp_pred), color = "#ff7f0e", linewidth = 1.5) +              # curva stimata
  geom_smooth(aes(y = wage), method = "lm", se = FALSE, color = "#1f77b4", linewidth = 1.5) + # retta OLS
  geom_point(data = punto_max, aes(y = wageexp_pred), color = "#2ca02c", size = 5) +
  labs(
    title = "Confronto: lineare vs polinomio quadratico",
    x = "Anni di Esperienza",
    y = "Salario orario in $"
  ) +
  theme_minimal()


# Polinomio con controlli (educ, tenure) ----
reg_wage_exper2 <- feols(wage ~ exper + exper^2 + educ + tenure, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_exper, "Wage" = reg_wage_exper2), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Trovare il picco: derivata prima ed effetto marginale ----
# dy/dx = b1 + 2*b2*x   →   picco in x* = -b1 / (2*b2)
b1 <- coef(reg_wage_exper)["exper"]
b2 <- coef(reg_wage_exper)["I(exper^2)"]
xstar <- -b1 / (2 * b2)

deriv_df <- tibble(
  exper = seq(0, 50, length.out = 200),
  d = b1 + 2 * b2 * exper
)

ggplot(deriv_df, aes(x = exper, y = d)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = xstar, color = "grey60", linewidth = 0.4, linetype = "dashed") +
  geom_line(color = "#ff7f0e", linewidth = 1.5) +
  annotate("point", x = xstar, y = 0, color = "#2ca02c", size = 5) +
  labs(
    title = "Derivata prima: effetto marginale di un anno di esperienza",
    x = "Anni di esperienza",
    y = "Effetto marginale stimato"
  ) +
  theme_minimal()


# Log e polinomi ----
reg_logwage_exper <- feols(log(wage) ~ exper + exper^2 + educ + tenure, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_exper, "Log Wage" = reg_logwage_exper), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# In deviazione dalla media: regressione semplice ----
# Centrando exper, l'intercetta diventa il salario predetto per un individuo
# con esperienza pari alla media (invece che con zero anni di esperienza).
wage1 <- wage1 %>%
  mutate(exper_center = exper - mean(exper, na.rm = TRUE))

reg_experc <- feols(wage ~ exper_center, data = wage1, vcov = "hetero")
reg_exper <- feols(wage ~ exper, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_experc, "Wage" = reg_exper), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Deviazione dalla media e polinomi ----
# Con il polinomio, il coefficiente di exper_center rappresenta l'effetto marginale
# quando exper è uguale alla media (invece che quando è uguale a zero).
reg_wagepolcenter <- feols(wage ~ educ + exper_center + exper_center^2 + tenure, data = wage1, vcov = "hetero")
reg_wagepol <- feols(wage ~ educ + exper + exper^2 + tenure, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wagepolcenter, "Wage" = reg_wagepol), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Statistiche descrittive di exper ----
desc_exper <- wage1 %>%
  summarise(
    Media = mean(exper, na.rm = TRUE),
    Q1 = quantile(exper, 0.25, na.rm = TRUE),
    Mediana = median(exper, na.rm = TRUE),
    Q3 = quantile(exper, 0.75, na.rm = TRUE)
  )

tt(desc_exper, digits = 3)
