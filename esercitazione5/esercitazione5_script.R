# Esercitazione 5 - Nonlinearità: interazioni


# Setup -------------------------------------------------------------------

library(tidyverse)
library(knitr)
library(modelsummary)
library(fixest)
library(wooldridge)
data("wage1", package = "wooldridge")


# Categorie multiple: tre dummy mutuamente esclusive ----------------------
# Categoria omessa: uomini single (female = 0, married = 0)

wage1 <- wage1 %>%
  mutate(
    marrmale = ifelse(female == 0 & married == 1, 1, 0),
    marrfemale = ifelse(female == 1 & married == 1, 1, 0),
    singfem = ifelse(female == 1 & married == 0, 1, 0)
  )

reg_wage_sm1 <- feols(wage ~ marrmale + marrfemale + singfem, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_sm1), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Interazione tra due variabili dummy: female * married -------------------

reg_wage_marrfe <- feols(wage ~ female * married, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_marrfe), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Modello speculare: male * married ---------------------------------------
# La categoria omessa diventa: donne single

wage1 <- wage1 %>%
  mutate(male = 1 - female)

reg_wage_marrmal <- feols(wage ~ male * married, data = wage1, vcov = "hetero")
modelsummary(
  list("Wage (Female = 1)" = reg_wage_marrfe, "Wage (Male = 1)" = reg_wage_marrmal),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# Interazione tra dummy e continua: educ * female -------------------------

reg_wage_educfe <- feols(wage ~ educ * female, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_educfe), gof_omit = "AIC|BIC|RMSE|R2 Adj.")

# install.packages("marginaleffects")
library(marginaleffects)
plot_predictions(reg_wage_educfe,
                 condition = c("educ", "female")) +
  labs(title = "Salario predetto per livelli di istruzione e genere",
       x = "Anni di istruzione",
       y = "Salario predetto")


# Interazione tra due variabili continue: educ * exper --------------------

reg_wage_educexper <- feols(wage ~ educ * exper, data = wage1, vcov = "hetero")
reg_wage_educexper2 <- feols(wage ~ educ * exper + female + tenure, data = wage1, vcov = "hetero")
modelsummary(
  list("Wage" = reg_wage_educexper, "Wage" = reg_wage_educexper2),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Statistiche descrittive di exper per valutare l'effetto marginale
# in valori rilevanti (mediana, media, terzo quartile)
datasummary(exper ~ Min + P25 + Median + Mean + P75 + Max, data = wage1)


# Centratura: educ_center e exper_center ----------------------------------

wage1 <- wage1 %>%
  mutate(
    educ_center = educ - mean(educ, na.rm = TRUE),
    exper_center = exper - mean(exper, na.rm = TRUE)
  )


# Interazione tra continue centrate: educ_center * exper_center -----------
# Il coefficiente di educ_center rappresenta l'effetto di un anno
# aggiuntivo di istruzione quando exper è alla media.

reg_wage_educexper_center <- feols(wage ~ educ_center * exper_center, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_educexper_center), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Interazione dummy e continua centrata: educ_center * female -------------
# Il coefficiente di female rappresenta la differenza salariale
# tra donne e uomini quando educ è alla media.

reg_wage_educfe_center <- feols(wage ~ educ_center * female, data = wage1, vcov = "hetero")
modelsummary(
  list("Wage" = reg_wage_educfe, "Wage" = reg_wage_educfe_center),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)
