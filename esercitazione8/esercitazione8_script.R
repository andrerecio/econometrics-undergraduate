# Esercitazione 8 - Regressione con dati panel

library(tidyverse)
library(knitr)
library(kableExtra)
library(modelsummary)
library(fixest)
library(wooldridge)


# Dataset wagepan: 545 lavoratori dal 1980 al 1987 (T=8)
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
# T=2: Effetti fissi individuali
# ============================================================

# Sottocampione con due soli periodi: 1980 e 1987
wagepan_2periods <- wagepan %>% filter(year %in% c(1980, 1987))
head(wagepan_2periods)


# --- (1) First difference (senza intercetta) ---
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


# --- (2) Within: deviazione dalla media individuale ---
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


# --- (3) LSDV: dummy individuali con as.factor(nr) ---
# as.factor(nr) per primo => feols rimuove educ/black/hisp (come |nr), non le dummy
reg_lsdv <- feols(lwage ~ as.factor(nr) + union + exper + expersq + educ + black + hisp + married,
                  data = wagepan_2periods, vcov = "hetero")
etable(reg_lsdv, keep = c("union", "exper", "expersq"),
       order = c("union", "exper", "expersq"), signif.code = NA)   # 545 dummy nascoste


# --- (4) feols con |nr ---
reg_fe_ind <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married | nr,
                    data = wagepan_2periods, vcov = "hetero")
reg_fe_ind


# ============================================================
# T=2: Effetti fissi individuali + temporali
# ============================================================

# First difference CON intercetta = ind FE + time FE (Stock & Watson p.283)
reg_fd_int <- feols(d_lwage ~ d_union + d_exper + d_expersq + d_educ + d_black + d_hisp + d_married,
                    data = wagepan_2periods_fd, vcov = "hetero")
reg_fd_int

reg_fe_indtime <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married | nr + year,
                        data = wagepan_2periods, vcov = "hetero")
reg_fe_indtime

modelsummary(
  list("FD con intercetta" = reg_fd_int, "|nr + year" = reg_fe_indtime),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# ============================================================
# T>2: Panel completo
# ============================================================

# --- Within sul panel completo ---
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


# --- |nr sul panel completo ---
reg_fe_ind_full <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married | nr,
                         data = wagepan, vcov = "hetero")
reg_fe_ind_full


# --- First difference sul panel completo (T-1 = 7 differenze per individuo) ---
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


# Confronto: demean = |nr (= 0.082), ma FD diverso (= 0.043)
modelsummary(
  list("Within" = reg_within_full, "|nr" = reg_fe_ind_full, "FD" = reg_fd_full),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- FE individuali + temporali sul panel completo ---
reg_fe_indtime_full <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married | nr + year,
                             data = wagepan, vcov = "hetero")
reg_fe_indtime_full


# Confronto finale wagepan
modelsummary(
  list("Pooled OLS" = reg_pooled, "FE | nr" = reg_fe_ind_full, "FE | nr+year" = reg_fe_indtime_full),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- Errori standard clustered: hetero vs cluster ---
# hetero corregge l'eteroschedasticita; cluster corregge anche la correlazione
# seriale entro l'individuo. Il coeff. di union non cambia, solo la s.e.
reg_fe_cluster <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married | nr + year,
                        data = wagepan, vcov = "cluster")
modelsummary(
  list("FE (hetero)" = reg_fe_indtime_full, "FE (cluster, nr)" = reg_fe_cluster),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# ============================================================
# Caso di studio: crime2 (46 città, T=2: 1982 e 1987)
# ============================================================
data("crime2", package = "wooldridge")
head(crime2)

# crime2 non ha colonna city: le righe sono ordinate a coppie (82, 87) per città
crime2 <- crime2 %>% mutate(city = rep(1:(nrow(crime2)/2), each = 2))


# --- Pooled OLS ---
regc_pooled <- feols(crmrte ~ unem, data = crime2, vcov = "hetero")
regc_pooled


# --- Effetti fissi temporali (tre modi equivalenti) ---
regc_d87       <- feols(crmrte ~ unem + d87,   data = crime2, vcov = "hetero")
regc_fe_year   <- feols(crmrte ~ unem | year,  data = crime2, vcov = "hetero")

crime2_dm_year <- crime2 %>%
  group_by(year) %>%
  mutate(crmrte_dm = crmrte - mean(crmrte),
         unem_dm   = unem   - mean(unem)) %>%
  ungroup()
regc_within_year <- feols(crmrte_dm ~ -1 + unem_dm, data = crime2_dm_year, vcov = "hetero")

modelsummary(
  list("Dummy d87" = regc_d87, "|year" = regc_fe_year, "Within anno" = regc_within_year),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- Effetti fissi individuali (per città) ---
# ccrmrte, cunem sono già le first difference 1987-1982 per ogni città
regc_fd      <- feols(ccrmrte ~ -1 + cunem, data = crime2, vcov = "hetero")
regc_fe_city <- feols(crmrte ~ unem | city,  data = crime2, vcov = "hetero")

modelsummary(
  list("FD (no intercetta)" = regc_fd, "|city" = regc_fe_city),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


# --- FE individuali + temporali: FD con intercetta = |city + year ---
regc_fd_int  <- feols(ccrmrte ~ cunem,           data = crime2, vcov = "hetero")
regc_fe_full <- feols(crmrte ~ unem | city + year, data = crime2, vcov = "hetero")

modelsummary(
  list("FD con intercetta" = regc_fd_int, "|city + year" = regc_fe_full),
  gof_omit = "AIC|BIC|RMSE|R2 Adj."
)
