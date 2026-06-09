# PROYECCION TASA GLOBAL DE FECUNDIDAD 
# Modificado de la carpeta de apoyo DA 9282 2026-2

# Ya había intentado pasar esta materia una vez y fallé miserablemente
# Sé que si paso la materia habrá sido en gran medida por el apoyo que
#  usted ha brindado durante el curso
# Muchas gracias, prometo no ejercer de demógrafo jaja

# Limpieza del entorno de R
rm(list=ls(all=TRUE))
options(scipen = 999)
options(digits=4)

# Mandamos a llamar a las librerías necesarias
library(readxl)
library(dplyr)
library(tidyverse)
library(forecast)
library(ggplot2)

# Se cargan la bases de datos
TGF <- read_excel("1_bilogito.xlsx", 
                  sheet = "Proyección TGF")

# Nivel de fecundidad 
# Transformación logística de la TGF

# Dadas tasas específicas de fecundidad, 
# estimamos y proyectamos el nivel de fecundidad (TGF)


#Primero utilizaremos la siguiente transformación logística de la TGF:
# Gt = ln((TGFt-L)/(U-TGFt))

# Donde L es la cota inferior 
#       U la cota superior de la TGF para todo t. 
# Para todos los casos, se tomo L = 1.5 y U = 4.5 hijos e hijas por mujer, 
# que se acercan a los máximos y mínimos históricos 
# de la TGF de las entidades federativas

L <- 1.5 # cota inferior
U <- 4.5 # cota 4.5
# Transformación logística de la TGF
Gt <- log( ( TGF$TGF - L ) / ( U - TGF$TGF ) ) 

# Proyectaremos Gt por medio de un modelo ARIMA para los 51 años de proyección

# Serie temporal: proyectar Gt
Gt <- ts(Gt, start = 1990)

# --------
# Explorando diferentes modelos temporales
# --------
# Guardamos los valores AIC 
# entre más bajo mejor
aic_vals <- numeric(15)
# Probamos diferentes configuraciones de arima
for (i in 1:15) {
  res <- arima(
    Gt,
    order = c(i, 1, 1)
  )
  aic_vals[i] <- AIC(res)
}
# Guardamos de forma tabular
tabla_aic <- data.frame(
  idx   = 1:15,
  param = round(aic_vals, 4) 
)
print(tabla_aic)
# Encontramos el que minimiza
best <- which.min(aic_vals)
cat(
  "Mejor modelo según AIC:\nGt: AR(", 
  best,
  ")\n",
  sep = ""
)
# Proyectamos con el parámetro que minimiza AIC
modelo_Gt <- Arima(Gt, order = c(best,1,1), include.drift = TRUE)
Gt_proy <- forecast(modelo_Gt, h = 51)
Gt_proy$mean
# Mi modelo es más pesimista :(, pero ajusta mejor

# Obtenemos las TGF proyectadas:
años_proy = 2020:2070
TGF_proy = ( L + U * exp( Gt_proy$mean ) ) / ( 1 + exp( Gt_proy$mean ) )
TGF_proy <- data.frame(
  AÑO = años_proy,
  TGF = TGF_proy
)

TGF$AÑO <- as.numeric(TGF$AÑO)
TGF_proy$AÑO <- as.numeric(TGF_proy$AÑO)

# Juntamos en una base las TGF de la conciliación y la proyección
TGF$Tipo      = "Conciliación"
TGF_proy$Tipo = "Proyección"
TGF_total     =  rbind(TGF, TGF_proy)

# Visualización de datos
ggplot(TGF_total, aes(x = AÑO, y = TGF, color = Tipo)) +
  geom_line(size = 1.2) +
  geom_point(data = TGF_total %>% filter(AÑO %% 5 == 0), size = 2, alpha = 0.6) +  # puntos cada 5 años
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray40", linewidth = 0.8) +  # marca de corte
  scale_color_manual(values = c("Conciliación" = "#1b9e77", "Proyección" = "#d95f02")) +
  scale_x_continuous(breaks = seq(1950, 2070, by = 10), expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(limits = c(1.2, 5), expand = expansion(mult = c(0.01, 0.01))) +
  labs(
    title = "Evolución y proyección de la Tasa Global de Fecundidad (TGF)",
    subtitle = "República Mexicana, 1950–2070",
    x = "Año",
    y = "TGF",
    color = "Serie"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


write.csv(TGF_proy, "data/tgf_proy.csv")

