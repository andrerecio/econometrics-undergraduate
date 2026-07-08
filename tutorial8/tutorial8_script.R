# Tutorial 8 - regression with panel data

library(tidyverse)
library(knitr)
library(kableExtra)
library(modelsummary)
library(fixest)
library(wooldridge)


# wagepan dataset: 545 workers from 1980 to 1987 (T=8)
data("wagepan", package = "wooldridge")
datasummary_skim(wagepan)
head(wagepan)


# ============================================================
# Pooled OLS (benchmark)
# ============================================================
reg_pooled <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married,
                    data = wagepan, vcov = "hetero")
reg_pooled


# ============================================================
# T=2: Individual fixed effects
# ============================================================

# Subsample with only two periods: 1980 and 1987
wagepan_2periods <- wagepan %>% filter(year %in% c(1980, 1987))
head(wagepan_2periods)


# --- (1) First differences (without an intercept) ---
wagepan_2periods_fd <- wagepan_2periods %>%
  arrange(nr, year) %>%
  group_by(nr) %>%
  mutate(
    d_lwage   = lwage   - lag(lwage),
    d_union   = union   - lag(union),
    d_exper   = exper   - lag(exper),
    d_expersq = expersq - lag(expersq),
    d_educ    = educ    - lag(educ),
    d_black   = black   - lag(black),
    d_hisp    = hisp    - lag(hisp),
    d_married = married - lag(married)
  ) %>%
  ungroup() %>%
  filter(!is.na(d_lwage))

reg_fd <- feols(d_lwage ~ -1 + d_union + d_exper + d_expersq + d_educ + d_black + d_hisp + d_married,
                data = wagepan_2periods_fd, vcov = "hetero")
reg_fd


# --- (2) Within transformation: deviations from individual means ---
wagepan_2periods_dm <- wagepan_2periods %>%
  group_by(nr) %>%
  mutate(
    lwage_dm   = lwage   - mean(lwage,   na.rm = TRUE),
    union_dm   = union   - mean(union,   na.rm = TRUE),
    exper_dm   = exper   - mean(exper,   na.rm = TRUE),
    expersq_dm = expersq - mean(expersq, na.rm = TRUE),
    educ_dm    = educ    - mean(educ,    na.rm = TRUE),
    black_dm   = black   - mean(black,   na.rm = TRUE),
    hisp_dm    = hisp    - mean(hisp,    na.rm = TRUE),
    married_dm = married - mean(married, na.rm = TRUE)
  ) %>%
  ungroup()

reg_within <- feols(lwage_dm ~ union_dm + exper_dm + expersq_dm + educ_dm + black_dm + hisp_dm + married_dm,
                    data = wagepan_2periods_dm, vcov = "hetero")
reg_within


# --- (3) LSDV: individual indicators using as.factor(nr) ---
# Placing as.factor(nr) first makes feols remove educ/black/hisp (as with |nr), not the indicators
reg_lsdv <- feols(lwage ~ as.factor(nr) + union + exper + expersq + educ + black + hisp + married,
                  data = wagepan_2periods, vcov = "hetero")
etable(reg_lsdv, keep = c("union", "exper", "expersq"),
       order = c("union", "exper", "expersq"), signif.code = NA)   # 545 indicators hidden


# --- (4) feols with |nr ---
reg_fe_ind <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married | nr,
                    data = wagepan_2periods, vcov = "hetero")
reg_fe_ind


# ============================================================
# T=2: Individual and time fixed effects
# ============================================================

# First differences WITH an intercept = individual FE + time FE (Stock & Watson, p. 283)
reg_fd_int <- feols(d_lwage ~ d_union + d_exper + d_expersq + d_educ + d_black + d_hisp + d_married,
                    data = wagepan_2periods_fd, vcov = "hetero")
reg_fd_int

reg_fe_indtime <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married | nr + year,
                        data = wagepan_2periods, vcov = "hetero")
reg_fe_indtime

modelsummary(
  list("FD with intercept" = reg_fd_int, "|nr + year" = reg_fe_indtime),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# ============================================================
# T>2: Full panel
# ============================================================

# --- Within transformation for the full panel ---
wagepan_dm <- wagepan %>%
  group_by(nr) %>%
  mutate(
    lwage_dm   = lwage   - mean(lwage,   na.rm = TRUE),
    union_dm   = union   - mean(union,   na.rm = TRUE),
    exper_dm   = exper   - mean(exper,   na.rm = TRUE),
    expersq_dm = expersq - mean(expersq, na.rm = TRUE),
    educ_dm    = educ    - mean(educ,    na.rm = TRUE),
    black_dm   = black   - mean(black,   na.rm = TRUE),
    hisp_dm    = hisp    - mean(hisp,    na.rm = TRUE),
    married_dm = married - mean(married, na.rm = TRUE)
  ) %>%
  ungroup()

reg_within_full <- feols(lwage_dm ~ -1 + union_dm + exper_dm + expersq_dm + educ_dm + black_dm + hisp_dm + married_dm,
                         data = wagepan_dm, vcov = "hetero")
reg_within_full


# --- |nr for the full panel ---
reg_fe_ind_full <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married | nr,
                         data = wagepan, vcov = "hetero")
reg_fe_ind_full


# --- First differences for the full panel (T-1 = 7 differences per individual) ---
wagepan_fd <- wagepan %>%
  arrange(nr, year) %>%
  group_by(nr) %>%
  mutate(
    d_lwage   = lwage   - lag(lwage),
    d_union   = union   - lag(union),
    d_exper   = exper   - lag(exper),
    d_expersq = expersq - lag(expersq),
    d_educ    = educ    - lag(educ),
    d_black   = black   - lag(black),
    d_hisp    = hisp    - lag(hisp),
    d_married = married - lag(married)
  ) %>%
  ungroup() %>%
  filter(!is.na(d_lwage))

reg_fd_full <- feols(d_lwage ~ -1 + d_union + d_exper + d_expersq + d_educ + d_black + d_hisp + d_married,
                     data = wagepan_fd, vcov = "hetero")
reg_fd_full


# Comparison: demeaning = |nr (= 0.082), but FD differs (= 0.043)
modelsummary(
  list("Within" = reg_within_full, "|nr" = reg_fe_ind_full, "FD" = reg_fd_full),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- Individual and time FE for the full panel ---
reg_fe_indtime_full <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married | nr + year,
                             data = wagepan, vcov = "hetero")
reg_fe_indtime_full


# Final wagepan comparison
modelsummary(
  list("Pooled OLS" = reg_pooled, "FE | nr" = reg_fe_ind_full, "FE | nr+year" = reg_fe_indtime_full),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- Clustered standard errors: heteroskedasticity-robust versus cluster-robust ---
# "hetero" corrects for heteroskedasticity; "cluster" also corrects for serial
# correlation within individuals. The union coefficient is unchanged; only its SE changes.
reg_fe_cluster <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married | nr + year,
                        data = wagepan, vcov = "cluster")
modelsummary(
  list("FE (hetero)" = reg_fe_indtime_full, "FE (cluster, nr)" = reg_fe_cluster),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# ============================================================
# Case study: crime2 (46 cities, T=2: 1982 and 1987)
# ============================================================
data("crime2", package = "wooldridge")
head(crime2)

# crime2 has no city column: rows are ordered in city pairs (1982, 1987)
crime2 <- crime2 %>% mutate(city = rep(1:(nrow(crime2)/2), each = 2))


# --- Pooled OLS ---
regc_pooled <- feols(crmrte ~ unem, data = crime2, vcov = "hetero")
regc_pooled


# --- Time fixed effects (three equivalent methods) ---
regc_d87       <- feols(crmrte ~ unem + d87,   data = crime2, vcov = "hetero")
regc_fe_year   <- feols(crmrte ~ unem | year,  data = crime2, vcov = "hetero")

crime2_dm_year <- crime2 %>%
  group_by(year) %>%
  mutate(crmrte_dm = crmrte - mean(crmrte),
         unem_dm   = unem   - mean(unem)) %>%
  ungroup()
regc_within_year <- feols(crmrte_dm ~ -1 + unem_dm, data = crime2_dm_year, vcov = "hetero")

modelsummary(
  list("Indicator d87" = regc_d87, "|year" = regc_fe_year, "Within year" = regc_within_year),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- City fixed effects ---
# ccrmrte and cunem already contain the 1987-1982 first differences for each city
regc_fd      <- feols(ccrmrte ~ -1 + cunem, data = crime2, vcov = "hetero")
regc_fe_city <- feols(crmrte ~ unem | city,  data = crime2, vcov = "hetero")

modelsummary(
  list("FD (no intercept)" = regc_fd, "|city" = regc_fe_city),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- Individual and time FE: FD with an intercept = |city + year ---
regc_fd_int  <- feols(ccrmrte ~ cunem,           data = crime2, vcov = "hetero")
regc_fe_full <- feols(crmrte ~ unem | city + year, data = crime2, vcov = "hetero")

modelsummary(
  list("FD with intercept" = regc_fd_int, "|city + year" = regc_fe_full),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)
