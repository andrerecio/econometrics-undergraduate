# Tutorial 6 - Linear Probability Model (LPM)
# Andrea Recine


# Setup ------------------------------------------------------------------------

library(tidyverse)
library(knitr)
library(modelsummary)
library(fixest)
library(wooldridge)


# Dataset alcohol --------------------------------------------------------------

# Load the alcohol data set from the wooldridge package
data("alcohol", package = "wooldridge")
head(alcohol)

# Descriptive statistics
datasummary_skim(alcohol)

# Estimate the LPM: employ ~ abuse + educ + age + married
# Heteroskedasticity-robust standard errors (heteroskedasticity is inherent in the LPM)
reg_lpm_alcohol <- feols(employ ~ abuse + educ + age + married,
                         data = alcohol,
                         vcov = "hetero")
modelsummary(list("Employ" = reg_lpm_alcohol),
             gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Predicted probabilities -----------------------------------------------------

# 30-year-old married man, 12 years of education, does not abuse alcohol
predict(reg_lpm_alcohol,
        newdata = data.frame(abuse = 0, educ = 12, age = 30, married = 1))

# 30-year-old unmarried man, 10 years of education, abuses alcohol
predict(reg_lpm_alcohol,
        newdata = data.frame(abuse = 1, educ = 10, age = 30, married = 0))


# crime1 data set - Probability of being arrested -----------------------------

# Load the crime1 data set from the wooldridge package
data("crime1", package = "wooldridge")
head(crime1)

# Create the 'arrested' dummy: 1 if the individual was arrested
# at least once in 1986, 0 otherwise
crime1 <- crime1 %>%
  mutate(arrested = ifelse(narr86 > 0, 1, 0))

# Estimate the LPM with heteroskedasticity-robust standard errors
# Omitted category for the ethnicity dummies: White men/other ethnicity
# (black = 0 and hispan = 0). The coefficients on black and hispan are interpreted
# relative to this reference group.
reg_lpm_crime <- feols(arrested ~ pcnv + avgsen + tottime + ptime86 + qemp86 + black + hispan,
                       data = crime1,
                       vcov = "hetero")
modelsummary(list("Arrested" = reg_lpm_crime),
             gof_omit = "AIC|BIC|RMSE|R2 Adj.")
