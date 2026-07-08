# ==============================================================================
# Econometrics Tutorials — Introduction to R
# Sapienza University of Rome
# ==============================================================================

# --- Installing packages (first time only) ------------------------------------

install.packages("tidyverse")
install.packages("wooldridge")
#install.packages("ggplot2")
#install.packages("dplyr")

# --- Loading packages ----------------------------------------------------------

library(tidyverse)
library(wooldridge)

# ==============================================================================
# 1. First steps
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
# 3. Data manipulation with dplyr
# ==============================================================================

# Filter observations with more than 12 years of education
wage_educ_higher <- wage1 |>
  filter(educ > 12)

head(wage_educ_higher)

# ==============================================================================
# 4. Descriptive statistics
# ==============================================================================

wage1 |>
  summarise(
    wage_mean   = mean(wage),
    wage_median = median(wage),
    wage_sd     = sd(wage),
    educ_mean   = mean(educ)
  )

# --- By group -------------------------------------------------------------------

# female = 0 → men, female = 1 → women
wage1 |>
  group_by(female) |>
  summarise(
    wage_mean = mean(wage),
    wage_sd   = sd(wage),
    n         = n()
  )

# ==============================================================================
# 5. Confidence interval for the mean
# ==============================================================================

# --- Manual computation ---------------------------------------------------------

n    <- length(wage1$wage)
xbar <- mean(wage1$wage)
s    <- sd(wage1$wage)
se   <- s / sqrt(n)

t_crit   <- qt(0.975, df = n - 1)  # approximately equal to 1.96

ci_lower <- xbar - t_crit * se
ci_upper <- xbar + t_crit * se

c(mean = xbar, lower = ci_lower, upper = ci_upper)

# --- Check with t.test() --------------------------------------------------------

t.test(wage1$wage)


# --- Testing the difference in means --------------------------------------------

# Descriptive statistics by group
# female = 0 → men, female = 1 → women
wagediff <- wage1 |>
  group_by(female) |>
  summarise(
    wage_mean = mean(wage),  # mean wage by group
    wage_sd   = sd(wage),    # standard deviation by group
    n         = n()          # group size
  )
wagediff

# Standard error of the difference in means
# SE = sqrt(s1^2/n1 + s2^2/n2)
se <- sqrt(wagediff$wage_sd[1]^2 / wagediff$n[1] +
             wagediff$wage_sd[2]^2 / wagediff$n[2])

# t statistic: (mean_men - mean_women) / SE
# H0: mu_men - mu_women = 0
# H1: mu_men - mu_women ≠ 0
t_stat <- (wagediff$wage_mean[1] - wagediff$wage_mean[2]) / se
t_stat

# We reject H0 at the 5% level if |t| > 1.96

# ==============================================================================
# 6. Graphs with ggplot2
# ==============================================================================

# Distribution of hourly wage
ggplot(wage1, aes(x = wage)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  labs(
    title = "Distribution of hourly wage",
    x = "Hourly wage (dollars)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 15)

# ==============================================================================
# 7. Police and crime
# ==============================================================================

# Load the dataset from GitHub
# Alternatively, download it and import it with Import Dataset
url_data <- "https://raw.githubusercontent.com/andrerecio/econometrics-undergraduate/main/introduction/crime2_clean.csv"

crime <- read_csv(url_data, show_col_types = FALSE, na = ".")
glimpse(crime)

# --- Create variables per 100,000 inhabitants -----------------------------------

crime_sub <- crime |>
  filter(year == 1991) |>
  mutate(
    violent = (murder + rape + robbery + assault) / citypop * 100000,
    police  = sworn / citypop * 100000
  ) |>
  drop_na(violent, police)

# --- Scatter plot: police vs violent crime --------------------------------------

ggplot(crime_sub, aes(x = police, y = violent)) +
  geom_point(alpha = 0.8) +
  labs(
    x = "Police officers per 100,000 inhabitants",
    y = "Violent crimes per 100,000 inhabitants"
  ) +
  theme_minimal(base_size = 20)

# ==============================================================================
# 8. Covariance and correlation
# ==============================================================================

# --- Using R functions ----------------------------------------------------------

cov(crime_sub$police, crime_sub$violent)
cor(crime_sub$police, crime_sub$violent)

# --- Manual computation --------------------------------------------------------

x <- crime_sub$police
y <- crime_sub$violent

cov_manual <- sum((x - mean(x)) * (y - mean(y))) / (length(x) - 1)
cor_manual <- cov_manual / (sd(x) * sd(y))

c(covariance = round(cov_manual, 2), correlation = round(cor_manual, 4))
