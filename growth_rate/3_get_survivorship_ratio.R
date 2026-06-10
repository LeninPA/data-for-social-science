# Tasas de supervivencia a CSV

# Cargamos bibliotecas
library(tidyverse)
library(dplyr)

# Leemos tabla de vida
lifetable <- read_csv("data/lifetable-1950-2070.csv")
# Dataframe de hombres
h <- lifetable %>%
  select(año, edad, sexo, Sx) %>%
  filter(sexo == "Hombres") %>%
  select(año, edad, Sx) %>%
  pivot_wider( # Convertimos a formato largo para Excel
    names_from = año,
    values_from = Sx
  ) %>%
  drop_na()
# Dataframe de mujeres, proceso análogo
m <- lifetable %>%
  select(año, edad, sexo, Sx) %>%
  filter(sexo == "Mujeres") %>%
  select(año, edad, Sx) %>%
  pivot_wider(
    names_from = año,
    values_from = Sx
  ) %>%
  drop_na()

write.csv(h, "data/survivorship_rates_male.csv")
write.csv(m, "data/survivorship_rates_female.csv")