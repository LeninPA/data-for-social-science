# Pirámides de población

library(tidyverse)
library(ggplot2)

f_total <- read_csv("data/population_female_1950_2070.csv")
m_total <- read_csv("data/population_male_1950_2070.csv")

years <- c("1950", "1970", "2000", "2020", "2050", "2070")

f <- f_total %>%
  select(Edad, all_of(years)) %>%
  filter(Edad != "Nacimientos_mujeres") %>% 
  mutate(Edad = as.numeric(Edad)) %>%
  mutate(Quinquenio = cut_interval(Edad, length = 5)) %>%
  select(Quinquenio, all_of(years)) %>%
  group_by(Quinquenio) %>%
  summarise(
    `1950` = sum(`1950`),
    `1970` = sum(`1970`),
    `2000` = sum(`2000`),
    `2020` = sum(`2020`),
    `2050` = sum(`2050`),
    `2070` = sum(`2070`)
  ) %>%
  mutate(Género = "F")%>%
  select(Quinquenio, Género, all_of(years))

m <- m_total %>%
  select(Edad, all_of(years)) %>%
  filter(Edad != "Nacimientos_hombres") %>% 
  mutate(Edad = as.numeric(Edad)) %>%
  mutate(Quinquenio = cut_interval(Edad, length = 5)) %>%
  select(Quinquenio, all_of(years)) %>%
  group_by(Quinquenio) %>%
  summarise(
    `1950` = sum(`1950`),
    `1970` = sum(`1970`),
    `2000` = sum(`2000`),
    `2020` = sum(`2020`),
    `2050` = sum(`2050`),
    `2070` = sum(`2070`)
  ) %>%
  mutate(Género = "M")%>%
  select(Quinquenio, Género, all_of(years))

pop <- bind_rows(f,m) %>%
  pivot_longer(all_of(years), 
               names_to = "Año", 
               values_to = "Población" ) %>%
  select(Año, Quinquenio, Género, Población) %>%
  group_by(Año) %>%
  mutate(
    Porcentaje = 100 * Población / sum(Población, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    porcentaje_plot = if_else(
      Género == "M",
      -Porcentaje,
      Porcentaje
    )
  )
max_pct <- max(pop$Porcentaje, na.rm = TRUE)

ggplot(
  pop,
  aes(
    x = porcentaje_plot,
    y = Quinquenio,
    fill = Género
  )
) +
  geom_col(width = 0.9) +
  facet_wrap(~ Año, ncol = 3) +
  coord_cartesian(xlim = c(-max_pct, max_pct)) +
  scale_x_continuous(
    labels = \(x) paste0(abs(x), "%")
  ) +
  xlab("Porcentaje de la población total")+
  theme_minimal()
