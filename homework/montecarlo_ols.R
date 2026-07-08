# ============================================================
# CLT per OLS — Simulazione Monte Carlo
# ============================================================
#
# Obiettivo: mostrare che β̂₁ si distribuisce normalmente
# anche quando gli errori NON sono normali.
#
# Il modello vero è:  y = 2 + 3x + u
# dove u segue una distribuzione esponenziale centrata
# (fortemente asimmetrica, non normale).
#
# Ripetiamo la stima OLS n_sim volte su campioni diversi,
# salviamo ogni β̂₁, e poi guardiamo la distribuzione:
# grazie al Teorema del Limite Centrale, sarà normale.
# ============================================================

library(fixest)

set.seed(1234)

# --- Parametri della simulazione ---
beta0 <- 2              # vera intercetta
beta1 <- 3              # vero coefficiente (quello che ci interessa)
n     <- 300            # osservazioni per campione
n_sim <- 10000          # quante volte ripetiamo l'esperimento

# --- Vettore dove salviamo i β̂₁ ---
beta1_hat <- numeric(n_sim)

# --- Simulazione ---
for (i in 1:n_sim) {

  # 1. Generiamo x (regressore, fisso)
  x <- 1 + 4 * runif(n)

  # 2. Generiamo gli errori: esponenziale centrata (media = 0, ma asimmetrica!)
  #    rexp(n) ha media 1, quindi sottraiamo 1 per centrare a zero
  #    u <- runif(n, -1, 1) sarebbe uniforme
  u <- rexp(n) - 1

  # 3. Generiamo y dal modello vero
  y <- beta0 + beta1 * x + u

  # 4. Stimiamo OLS con feols
  dat <- data.frame(y = y, x = x)
  fit <- feols(y ~ x, data = dat)

  # 5. Salviamo β̂₁ (secondo coefficiente)
  beta1_hat[i] <- coef(fit)["x"]
}


# --- Statistiche descrittive ---
cat("─────────────────────────────────\n")
cat("  n =", n, ",  n_sim =", n_sim, "simulazioni\n")
cat("─────────────────────────────────\n")
cat("  Media  β̂₁  =", round(mean(beta1_hat), 4), "  (vero:", beta1, ")\n")
cat("  Std    β̂₁  =", round(sd(beta1_hat), 4), "\n")
cat("  Skewness   =", round(mean(((beta1_hat - mean(beta1_hat)) / sd(beta1_hat))^3), 4),
    "  (→ 0 se CLT funziona)\n")
cat("─────────────────────────────────\n")

# --- Istogramma + curva normale teorica (ggplot2) ---
library(ggplot2)

ggplot(data.frame(b = beta1_hat), aes(x = b)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60, fill = "steelblue", alpha = 0.6, color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean(beta1_hat), sd = sd(beta1_hat)),
                color = "red", linewidth = 1) +
  geom_vline(xintercept = beta1, linetype = "dashed") +
  labs(title = paste0("CLT in azione: distribuzione di β̂₁  (n = ", n, ", n_sim = ", n_sim, ")"),
       x = expression(hat(beta)[1]),
       y = "Densità") +
  theme_minimal()



