# ============================================================
# CLT for OLS — Monte Carlo Simulation
# ============================================================
#
# Goal: show that β̂₁ is normally distributed
# even when the errors are NOT normal.
#
# The true model is:  y = 2 + 3x + u
# where u follows a centered exponential distribution
# (strongly skewed, not normal).
#
# We repeat the OLS estimation n_sim times on different samples,
# save each β̂₁, and then look at the distribution:
# thanks to the Central Limit Theorem, it will be normal.
# ============================================================

library(fixest)

set.seed(1234)

# --- Simulation parameters ---
beta0 <- 2              # true intercept
beta1 <- 3              # true coefficient (the one we care about)
n     <- 300            # observations per sample
n_sim <- 10000          # how many times we repeat the experiment

# --- Vector where we store the β̂₁ values ---
beta1_hat <- numeric(n_sim)

# --- Simulation ---
for (i in 1:n_sim) {

  # 1. Generate x (regressor, fixed)
  x <- 1 + 4 * runif(n)

  # 2. Generate the errors: centered exponential (mean = 0, but skewed!)
  #    rexp(n) has mean 1, so we subtract 1 to center it at zero
  #    u <- runif(n, -1, 1) would be uniform
  u <- rexp(n) - 1

  # 3. Generate y from the true model
  y <- beta0 + beta1 * x + u

  # 4. Estimate OLS with feols
  dat <- data.frame(y = y, x = x)
  fit <- feols(y ~ x, data = dat)

  # 5. Store β̂₁ (second coefficient)
  beta1_hat[i] <- coef(fit)["x"]
}


# --- Descriptive statistics ---
cat("─────────────────────────────────\n")
cat("  n =", n, ",  n_sim =", n_sim, "simulations\n")
cat("─────────────────────────────────\n")
cat("  Mean   β̂₁  =", round(mean(beta1_hat), 4), "  (true:", beta1, ")\n")
cat("  Std    β̂₁  =", round(sd(beta1_hat), 4), "\n")
cat("  Skewness   =", round(mean(((beta1_hat - mean(beta1_hat)) / sd(beta1_hat))^3), 4),
    "  (→ 0 if the CLT works)\n")
cat("─────────────────────────────────\n")

# --- Histogram + theoretical normal curve (ggplot2) ---
library(ggplot2)

ggplot(data.frame(b = beta1_hat), aes(x = b)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60, fill = "steelblue", alpha = 0.6, color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean(beta1_hat), sd = sd(beta1_hat)),
                color = "red", linewidth = 1) +
  geom_vline(xintercept = beta1, linetype = "dashed") +
  labs(title = paste0("The CLT in action: distribution of β̂₁  (n = ", n, ", n_sim = ", n_sim, ")"),
       x = expression(hat(beta)[1]),
       y = "Density") +
  theme_minimal()


