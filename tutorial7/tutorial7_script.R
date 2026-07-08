# Tutorial 7 - Probit and Logit Models
# Andrea Recine


# Setup ------------------------------------------------------------------------

library(tidyverse)
library(knitr)
library(modelsummary)
library(fixest)
library(wooldridge)
library(marginaleffects)


# Probit Model ----------------------------------------------------------------

# Load the alcohol data from the wooldridge package
data(alcohol, package = "wooldridge")

# Estimate the Probit model using the same specification as the LPM
# The coefficients are NOT marginal effects: they must be scaled by phi(Xb)
# beta_j determines only the sign of the marginal effect, not its magnitude
probit_alcohol <- feglm(employ ~ abuse + educ + age + married,
                        data = alcohol,
                        family = "probit")
modelsummary(list("Employ (Probit)" = probit_alcohol),
             gof_omit = "AIC|BIC|RMSE")


# Logit Model -----------------------------------------------------------------

# Estimate the Logit model using the same specification
# Probit and Logit produce similar results; they differ mainly in the tails
# (the logistic distribution has heavier tails)
logit_alcohol <- feglm(employ ~ abuse + educ + age + married,
                       data = alcohol,
                       family = "logit")
modelsummary(list("Employ (Logit)" = logit_alcohol),
             gof_omit = "AIC|BIC|RMSE")


# Comparison of the Link Functions -------------------------------------------

# The three functions Pr(Y=1 | X*beta) as functions of the linear index X*beta
# - LPM: a line that can fall outside [0,1]
# - Probit and Logit: S-shaped and always in [0,1]
# - Logit: heavier tails than Probit
z <- seq(-5, 5, length.out = 400)
curve_df <- tibble(
  z = z,
  Probit = pnorm(z),
  Logit  = plogis(z),
  LPM    = 0.5 + 0.1 * z
) |>
  pivot_longer(-z, names_to = "Model", values_to = "p")

ggplot(curve_df, aes(x = z, y = p, color = Model)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "grey60") +
  scale_color_manual(values = c("LPM" = "skyblue",
                                "Probit" = "darkblue",
                                "Logit" = "lightblue")) +
  labs(x = expression(X * beta), y = "Pr(Y = 1)",
       title = "LPM, Probit, and Logit") +
  theme_minimal()


# Manual Calculation of the Predicted Probability ----------------------------

# Extract the Probit coefficients to construct the linear index X*beta
b <- coef(probit_alcohol)
b

# Individual A: age=35, educ=14, married=1, abuse=0
xb_A <- b["(Intercept)"] + b["abuse"]*0 + b["educ"]*14 +
        b["age"]*35 + b["married"]*1
unname(xb_A)

# Predicted probability: apply Phi (pnorm) to the linear index
prob_A <- pnorm(xb_A)
unname(prob_A)

# Individual B: contrasting profile (not married, abuses alcohol, little
# education, and older). Here X*beta < 0, so pnorm(X*beta) < 0.5
xb_B <- b["(Intercept)"] + b["abuse"]*1 + b["educ"]*4 +
        b["age"]*55 + b["married"]*0
unname(xb_B)

prob_B <- pnorm(xb_B)
unname(prob_B)

# Rule: Phi(0) = 0.5 and Phi is strictly increasing
# => sign(X*beta) determines whether the predicted probability is >, =, or < 0.5
# The same rule applies to Logit (with Lambda instead of Phi)

# Check: predict() returns the same values for the two individuals
predict(probit_alcohol,
        newdata = data.frame(abuse   = c(0,  1),
                             educ    = c(14, 4),
                             age     = c(35, 55),
                             married = c(1,  0)))

# Plot the two individuals on the Phi curve
# Dashed lines at X*beta=0 and Phi=0.5 highlight the symmetry point
points <- tibble(
  individual = c("A", "B"),
  xb        = c(xb_A, xb_B),
  p         = c(prob_A, prob_B)
)

ggplot(tibble(z = seq(-3, 3, length.out = 400)),
       aes(x = z, y = pnorm(z))) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0,   linetype = "dashed", color = "grey50") +
  geom_point(data = points, aes(x = xb, y = p),
             color = "skyblue", size = 3) +
  geom_text(data = points, aes(x = xb, y = p, label = individual),
            nudge_y = 0.05, fontface = "bold") +
  labs(x = expression(X * beta), y = expression(Phi(X * beta)),
       title = "Linear Index and Predicted Probability") +
  theme_minimal()


# Odds Ratios -----------------------------------------------------------------

# In the Logit model:
#   odds(X)   = p/(1-p) = exp(X*beta)
#   log-odds  = X*beta  (linear in X)
#   OR_j      = odds(X_j+1) / odds(X_j) = exp(beta_j)
# The OR depends only on beta_j: it is constant across values of X
# Warning: an odds ratio is NOT a probability
exp(coef(logit_alcohol)) |> round(3)


# Average Marginal Effects (AME) - Manual Calculation -------------------------

# For a dummy variable X_j, the marginal effect is the difference between the
# predicted probabilities when X_j=1 and X_j=0. The AME averages these
# differences over all individuals in the sample.
# Create two copies of the dataset, one with abuse=1 and one with abuse=0,
# predict the probability for each copy, and calculate the average difference
alcohol_tmp1 <- alcohol
alcohol_tmp0 <- alcohol
alcohol_tmp1$abuse <- 1
alcohol_tmp0$abuse <- 0

# Manual AME for the Probit model
proba_employ_probit    <- predict(probit_alcohol, newdata = alcohol_tmp1)
proba_nonemploy_probit <- predict(probit_alcohol, newdata = alcohol_tmp0)
ame_manual_probit <- mean(proba_employ_probit - proba_nonemploy_probit)
print(paste("Manual average marginal effect (Probit):",
            round(ame_manual_probit, 4)))

# Manual AME for the Logit model
proba_employ_logit    <- predict(logit_alcohol, newdata = alcohol_tmp1)
proba_nonemploy_logit <- predict(logit_alcohol, newdata = alcohol_tmp0)
ame_manual_logit <- mean(proba_employ_logit - proba_nonemploy_logit)
print(paste("Manual average marginal effect (Logit):",
            round(ame_manual_logit, 4)))


# Average Marginal Effects (AME) Using marginaleffects ------------------------

# avg_slopes() without newdata calculates average marginal effects
# (an effect for each observation, followed by the average)
# Advantage: automatic standard errors using the delta method
all_effects_logit  <- avg_slopes(logit_alcohol)
all_effects_probit <- avg_slopes(probit_alcohol)

all_effects_logit
all_effects_probit

# Table comparing Logit and Probit AMEs
modelsummary(list("Logit (AME)"  = all_effects_logit,
                  "Probit (AME)" = all_effects_probit),
             gof_omit = "AIC|BIC|RMSE|R2|Adj")

# AME interpretation (Probit):
# - abuse:   -1.9 percentage points in the probability of employment
# - educ:    +1.5 percentage points per additional year of education
# - married: +8.9 percentage points relative to not being married


# Marginal Effects at the Mean (MEM) ------------------------------------------

# avg_slopes() with newdata = "mean" calculates marginal effects at the single
# point where every X equals its sample mean
mem_probit <- avg_slopes(probit_alcohol, newdata = "mean")
mem_logit  <- avg_slopes(logit_alcohol,  newdata = "mean")

# Table comparing AME and MEM for both models
# AME averages the individual effects; MEM is less interpretable for dummy
# variables because a fractional mean does not describe an observed individual
modelsummary(
  list("Probit (AME)" = all_effects_probit,
       "Probit (MEM)" = mem_probit,
       "Logit (AME)"  = all_effects_logit,
       "Logit (MEM)"  = mem_logit),
  gof_omit = "AIC|BIC|RMSE|R2|Adj"
)

# Distribution of individual marginal effects (Logit) with MEM
slopes_logit <- slopes(logit_alcohol)

ggplot(slopes_logit, aes(x = estimate)) +
  geom_histogram(bins = 60, fill = "lightblue", color = "white") +
  geom_vline(data = mem_logit, aes(xintercept = estimate),
             color = "darkblue", linewidth = 0.8) +
  facet_wrap(contrast ~ term, scales = "free") +
  labs(title = "Distribution of Individual Marginal Effects (Logit)",
       subtitle = "The vertical line is the marginal effect at the mean (MEM)",
       x = "Marginal effect",
       y = "Frequency") +
  theme_minimal()

# Same plot with the AME as the vertical line
# The AME is the arithmetic mean of the distribution
ggplot(slopes_logit, aes(x = estimate)) +
  geom_histogram(bins = 60, fill = "lightblue", color = "white") +
  geom_vline(data = all_effects_logit, aes(xintercept = estimate),
             color = "darkblue", linewidth = 0.8) +
  facet_wrap(contrast ~ term, scales = "free") +
  labs(title = "Distribution of Individual Marginal Effects (Logit)",
       subtitle = "The vertical line is the average marginal effect (AME)",
       x = "Marginal effect",
       y = "Frequency") +
  theme_minimal()


# Comparing LPM, Probit, and Logit - alcohol Dataset --------------------------

# Estimate the LPM with heteroskedasticity-robust standard errors
reg_lpm_alcohol <- feols(employ ~ abuse + educ + age + married,
                         data = alcohol,
                         vcov = "hetero")

# The three models produce very similar results for the alcohol dataset
modelsummary(list("Employ (LPM)"        = reg_lpm_alcohol,
                  "Employ AME (Probit)" = all_effects_probit,
                  "Employ AME (Logit)"  = all_effects_logit),
             gof_omit = "AIC|BIC|RMSE|R2 Adj.")


# Comparing LPM, Probit, and Logit - crime1 Dataset ---------------------------

# Load the crime1 data and create the arrested dummy variable
# (1 if arrested at least once in 1986, 0 otherwise)
data("crime1", package = "wooldridge")
head(crime1)

crime1 <- crime1 %>%
  mutate(arrested = ifelse(narr86 > 0, 1, 0))

# LPM with heteroskedasticity-robust standard errors
reg_lpm_crime <- feols(arrested ~ pcnv + avgsen + tottime + ptime86 + qemp86 + black + hispan,
                       data = crime1,
                       vcov = "hetero")
modelsummary(list("Arrested" = reg_lpm_crime),
             gof_omit = "AIC|BIC|RMSE|R2 Adj.")

# Probit and Logit (raw coefficients are not comparable with the LPM)
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

# Probit and Logit AMEs, which are comparable with the LPM
ame_crime_probit <- avg_slopes(probit_crime)
ame_crime_logit  <- avg_slopes(logit_crime)

ame_crime_probit
ame_crime_logit

modelsummary(list("Arrested (LPM)"        = reg_lpm_crime,
                  "Arrested (Probit AME)" = ame_crime_probit,
                  "Arrested (Logit AME)"  = ame_crime_logit),
             gof_omit = "AIC|BIC|RMSE|R2 Adj.")

# The average probability of arrest is far from 0.5
# -> raw Probit/Logit coefficients use a different scale from the LPM,
#    but the AMEs are comparable and qualitatively consistent
mean(crime1$arrested) |> round(3)
