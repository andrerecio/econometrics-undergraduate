# Tutorial 5 - Nonlinearity: interactions


# Setup -------------------------------------------------------------------

library(tidyverse)
library(knitr)
library(modelsummary)
library(fixest)
library(wooldridge)
data("wage1", package = "wooldridge")


# Multiple categories: three mutually exclusive dummy variables ----------------------
# Omitted category: single men (female = 0, married = 0)

wage1 <- wage1 %>%
  mutate(
    marrmale = ifelse(female == 0 & married == 1, 1, 0),
    marrfemale = ifelse(female == 1 & married == 1, 1, 0),
    singfem = ifelse(female == 1 & married == 0, 1, 0)
  )

reg_wage_sm1 <- feols(wage ~ marrmale + marrfemale + singfem, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_sm1), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Interaction between two dummy variables: female * married -------------------

reg_wage_marrfe <- feols(wage ~ female * married, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_marrfe), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Mirror model: male * married ---------------------------------------
# The omitted category becomes: single women

wage1 <- wage1 %>%
  mutate(male = 1 - female)

reg_wage_marrmal <- feols(wage ~ male * married, data = wage1, vcov = "hetero")
modelsummary(
  list("Wage (Female = 1)" = reg_wage_marrfe, "Wage (Male = 1)" = reg_wage_marrmal),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# Interaction between a dummy variable and a continuous variable: educ * female -------------------------

reg_wage_educfe <- feols(wage ~ educ * female, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_educfe), gof_omit = "AIC|BIC|RMSE|R2 Adj.")

# install.packages("marginaleffects")
library(marginaleffects)
plot_predictions(reg_wage_educfe,
                 condition = c("educ", "female")) +
  labs(title = "Predicted Wage by Education and Gender",
       x = "Years of education",
       y = "Predicted wage")


# Interaction between two continuous variables: educ * exper --------------------

reg_wage_educexper <- feols(wage ~ educ * exper, data = wage1, vcov = "hetero")
reg_wage_educexper2 <- feols(wage ~ educ * exper + female + tenure, data = wage1, vcov = "hetero")
modelsummary(
  list("Wage" = reg_wage_educexper, "Wage" = reg_wage_educexper2),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Descriptive statistics for exper to evaluate the marginal effect
# at relevant values (median, mean, third quartile)
datasummary(exper ~ Min + P25 + Median + Mean + P75 + Max, data = wage1)


# Centering: educ_center and exper_center --------------------------------

wage1 <- wage1 %>%
  mutate(
    educ_center = educ - mean(educ, na.rm = TRUE),
    exper_center = exper - mean(exper, na.rm = TRUE)
  )


# Interaction between centered continuous variables: educ_center * exper_center -----------
# The coefficient on educ_center represents the effect of one
# additional year of education when exper is at its mean.

reg_wage_educexper_center <- feols(wage ~ educ_center * exper_center, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_educexper_center), gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Interaction between a dummy variable and a centered continuous variable: educ_center * female -------------
# The coefficient on female represents the wage difference
# between women and men when educ is at its mean.

reg_wage_educfe_center <- feols(wage ~ educ_center * female, data = wage1, vcov = "hetero")
modelsummary(
  list("Wage" = reg_wage_educfe, "Wage" = reg_wage_educfe_center),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)
