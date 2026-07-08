#Esercitazione 10 - Script


library(tidyverse)
library(wooldridge)
library(modelsummary)
data("labsup", package = "wooldridge")

library(fixest)
ols_labsup <- feols(hours ~ kids + educ + age + black + hispan, data = labsup, vcov = "hetero")


modelsummary(list("Hours" = ols_labsup), gof_omit = "AIC|BIC|RMSE|R2 Adj.|R2")

iv_labsup <- feols(hours ~ educ + age + black + hispan | kids ~ samesex, data = labsup, vcov = "hetero")

modelsummary(list("Hours" = iv_labsup), gof_omit = "AIC|BIC|RMSE|R2 Adj.|R2")

summary(iv_labsup , stage = 1)

fs_labsup <- feols(kids ~ samesex + educ + age + black + hispan, data = labsup, vcov = "hetero")
fs_labsup


labsup$kids_hat <- predict(fs_labsup)
feols(hours ~ kids_hat + educ + age + black + hispan, data = labsup, vcov = "hetero")

library(car)
linearHypothesis(fs_labsup , "samesex=0")

linearHypothesis(fs_labsup , "samesex=0", test="F")

iv_labsup <- feols(hours ~ educ + age + black + hispan | kids ~ multi2nd, data = labsup, vcov = "hetero")
iv_labsup


fs_labsup_overid <- feols(kids ~ samesex + multi2nd + educ + age + black + hispan, data = labsup, vcov = "hetero")

linearHypothesis(fs_labsup_overid, c("samesex=0", "multi2nd=0"))


iv_labsup_overid <- feols(hours ~ educ + age + black + hispan | kids ~ samesex + multi2nd, data = labsup, vcov = "hetero")

labsup <- labsup |> mutate(uhat = hours - iv_labsup_overid$fitted.values)
Jlm <- feols(uhat~ samesex + multi2nd + educ +age + black + hispan, data = labsup, vcov = "iid")

linearHypothesis(Jlm, c("samesex=0", "multi2nd=0"))


modelsummary(list("Hours" = iv_labsup_overid), gof_omit = "AIC|BIC|RMSE|R2 Adj.|R2|Num\\.Obs", stars =TRUE)