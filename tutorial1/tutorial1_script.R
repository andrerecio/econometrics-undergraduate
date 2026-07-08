# ==============================================================================
# Tutorial 1 - Simple Linear Regression
# Econometrics I
# ==============================================================================

# install.packages(kableExtra)

# --- Packages ----------------------------------------------------------------

library(tidyverse)    # dplyr, tidyr, ggplot2, readr
library(knitr)        # tables with kable()
library(kableExtra)   # table styles
library(modelsummary) # regression tables
library(wooldridge)   # dataset wage1
library(fixest)       # estimate OLS with feols()

# --- Data ---------------------------------------------------------------------

# Current Population Survey, 1976
data("wage1", package = "wooldridge")

# First observations
head(wage1)

# Variables of interest:
# wage = average hourly earnings in 1976 dollars
# educ = years of education

# --- Descriptive Statistics ---------------------------------------------------

stat <- wage1 %>%
  summarise(
    wage_mean   = mean(wage, na.rm = TRUE),
    wage_median = median(wage, na.rm = TRUE),
    wage_sd     = sd(wage, na.rm = TRUE),
    educ_mean   = mean(educ, na.rm = TRUE),
    educ_median = median(educ, na.rm = TRUE)
  )

stat %>%
  kable(caption = "Descriptive statistics", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# --- Graphs -------------------------------------------------------------------

# Histogram of wage
ggplot(wage1, aes(x = wage)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 15) +
  labs(title = "Distribution of wage", x = "Wage", y = "Frequency") +
  theme_minimal()

# Histogram of educ
ggplot(wage1, aes(x = educ)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 15) +
  labs(title = "Distribution of education",
       x = "Years of education", y = "Frequency") +
  theme_minimal()

# Scatterplot wage vs educ
ggplot(wage1, aes(y = wage, x = educ)) +
  geom_point(color = "black") +
  theme_minimal()

# Scatterplot with regression line
ggplot(wage1, aes(y = wage, x = educ)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "skyblue", se = FALSE) +
  theme_minimal()

# --- Simple Linear Regression -------------------------------------------------

# wage_i = beta_0 + beta_1 * educ_i + u_i

# Heteroskedasticity-robust standard errors
reg1 <- feols(wage ~ educ, data = wage1, vcov = "hetero")
reg1

# Homoskedastic standard errors (equivalent to lm())
reg1_ho <- feols(wage ~ educ, data = wage1)
reg1_ho

# --- Heteroskedasticity -------------------------------------------------------

# Variance of wage for specific values of educ
educ_valori <- c(4, 8, 12, 16)

varianza_valori <- wage1 %>%
  filter(educ %in% educ_valori) %>%
  group_by(educ) %>%
  summarise(varianza = var(wage, na.rm = TRUE)) %>%
  ungroup()

ggplot(varianza_valori, aes(x = educ, y = varianza)) +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "Variance of wage for specific education values",
       x = "Years of education", y = "Variance of wage") +
  theme_minimal()

# Variance of wage for 10 educ groups
varianza_educ <- wage1 %>%
  mutate(gruppo_educ = cut(educ, breaks = 10)) %>%
  group_by(gruppo_educ) %>%
  summarise(
    varianza   = var(wage, na.rm = TRUE),
    media_educ = mean(educ, na.rm = TRUE)
  )

ggplot(varianza_educ, aes(x = media_educ, y = varianza)) +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "Variance of wage by education level",
       x = "Years of education (group mean)",
       y = "Variance of wage") +
  theme_minimal()

# Comparison of robust and homoskedastic standard errors
modelsummary(
  list("Wage (robust)" = reg1, "Wage (homosked.)" = reg1_ho),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# --- Changing Units of Measurement -------------------------------------------

# Dependent variable: wage in hundreds of dollars
wage1 <- wage1 %>%
  mutate(wage_100 = wage / 100)

reg2 <- feols(wage_100 ~ educ, data = wage1, vcov = "hetero")

modelsummary(
  list("Wage ($)" = reg1, "Wage (hundreds of $)" = reg2),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Dependent variable: monthly wage (7h x 5 days x 4 weeks = 140)
wage1 <- wage1 %>%
  mutate(wage_monthly = wage * 140)

reg3 <- feols(wage_monthly ~ educ, data = wage1, vcov = "hetero")

modelsummary(
  list("Hourly wage" = reg1, "Monthly wage" = reg3),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# Independent variable: educ in months
wage1 <- wage1 %>%
  mutate(educ_mesi = educ * 12)

reg4 <- feols(wage ~ educ_mesi, data = wage1, vcov = "hetero")

modelsummary(
  list("Wage (educ years)" = reg1, "Wage (educ months)" = reg4),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)

# --- Confidence Intervals and Hypothesis Tests --------------------------------

# 95% CI: beta_1 +/- 1.96 * SE(beta_1)
# 0.541 +/- 1.96 * 0.061 = [0.422, 0.660]
confint(reg1)

# --- Dummy Variables ----------------------------------------------------------

# Frequency table
tabledummy <- table(wage1$female)
names(tabledummy) <- c("Male", "Female")

kable(tabledummy, caption = "Number of Men and Women") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Descriptive statistics by gender
stat_gender <- wage1 %>%
  group_by(female) %>%
  summarize(
    wage_mean   = mean(wage, na.rm = TRUE),
    wage_median = median(wage, na.rm = TRUE),
    wage_sd     = sd(wage, na.rm = TRUE),
    educ_mean   = mean(educ, na.rm = TRUE),
    educ_median = median(educ, na.rm = TRUE)
  )

stat_gender %>%
  kable(caption = "Descriptive statistics by gender", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Mean of the dummy = proportion of female observations in the sample
mean(wage1$female)  # 252/526 ≈ 0.48

# Regression with the female dummy
# wage_i = beta_0 + beta_1 * female_i + u_i
reg_female <- feols(wage ~ female, data = wage1, vcov = "hetero")

modelsummary(
  list("Wage" = reg_female),
  gof_omit = "AIC|BIC|RMSE|R2 Adj.",
  title = "The dependent variable is Wage"
)

# Changing the reference category: male dummy
wage1 <- wage1 %>%
  mutate(male = ifelse(female == 1, 0, 1))

# Equivalent method: male = 1 - female

reg_male <- feols(wage ~ male, data = wage1, vcov = "hetero")

modelsummary(
  list("Female = 1" = reg_female, "Male = 1" = reg_male),
  gof_omit = "AIC|BIC|RMSE|R2 Adj.",
  title = "Female = 1 (col. 1) vs Male = 1 (col. 2)"
)

# Perfect multicollinearity: male + female = 1 = intercept
# feols automatically removes one of the two variables
reg2dummy <- feols(wage ~ male + female, data = wage1, vcov = "hetero")
reg2dummy
