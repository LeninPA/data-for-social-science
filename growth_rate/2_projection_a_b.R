# Proyección de los parámetros a, b
# Método escogido: regresión lineal

# Importación de bibliotecas
library(tidyverse)
library(readxl)
# Proyección de a
obs_bilogito <- read_csv("data/bilogito_params_obs.csv")

ols_a <- lm(a ~ year, data = obs_bilogito)
ols_b <- lm(b ~ year, data = obs_bilogito)

new_data <- data.frame(
  year = 2020:2070
)

pred_a <- predict(ols_a, newdata = new_data)
pred_b <- predict(ols_b, newdata = new_data)

pred_bilogito <- data.frame(
  year = pred_years,
  a    = pred_a,
  b    = pred_b
)

write.csv(pred_bilogito, "data/bilogito_params_pred.csv")
