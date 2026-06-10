## ----setup, include=FALSE--------------------------------
# install.packages(c("readxl", "forecast", "dplyr", "ggrepel"))
rm(list=ls(all=TRUE))
library(readxl)
library(dplyr)
library(tidyverse)
library(forecast)
library(ggplot2)
library(ggrepel)

Pob_mitad   <- read_excel("data/0_Pob_Mitad_1950_2070.xlsx")
Defunciones <- read_excel("data/1_Defunciones_1950_2070.xlsx")
TEM <- Defunciones

TEM$POBLACION <- Pob_mitad$POBLACION
TEM$TEM       <- TEM$DEFUNCIONES/TEM$POBLACION

TEM <- TEM %>% filter(ENTIDAD == "República Mexicana")
TEM <- TEM %>% select(-ENTIDAD)

Base_CD_TEM <- matrix(NA, 220, 70)
colnames(Base_CD_TEM) = seq(1950,2019)

for (i in 1950:2019) 
{
  for (j in 1:220) 
  {
    if(j <= 110) 
    { # Hombres
      valor <- TEM %>%
        filter(SEXO == "Hombres", AÑO == i, EDAD == (j-1)) %>%
        pull(TEM) # Extraer el valor
    } else { # Mujeres
      valor <- TEM %>%
        filter(SEXO == "Mujeres", AÑO == i, EDAD == (j-111)) %>%
        pull(TEM) # Extraer el valor
    }
    # Asignamos el valor a la matriz
    Base_CD_TEM[j, i-1949] = ifelse(length(valor) > 0, valor, NA)
  }
}

Base_CD_TEM <- as.data.frame.matrix(Base_CD_TEM) 

age   <-    0:109
years <- 1950:2019
sex   <- rep(c("Hombre, Mujer"), each = 110)

df <- as.data.frame(Base_CD_TEM)
colnames(df) <- as.character(years)
df$Edad <- rep(age, 2)
df$Sexo <- sex

df_largo <- df %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"),
               names_to = "Año",
               values_to = "Tasa") %>%
  mutate(Año = as.integer(Año))

ev           <- read_excel("data/EV_1950_2019.xlsx")
tabla_modelo <- read_excel("data/MLT_UN2011_130_1y_complete.xlsx")

imputar_mx_edades_extremas <- function(tabla_modelo, esperanza_vida, mxt, años = 1950:2019) {
  for (año in años) {
    for (sexo in c("Male", "Female")) {
      
      # Extraer esperanza de vida como entero
      sexo_spa <- if( sexo == "Male" ) "Hombres" else "Mujeres"
      e0 <- as.numeric(ev[which(ev$año == año & ev$sexo == sexo_spa), ]$EV)
      
      # Filtrar tabla modelo por patrón Latin, sexo y E0 exacto
      tabla_filtrada <- tabla_modelo[
        tabla_modelo$Family == "Latin" &
          tabla_modelo$Sex == sexo &
          tabla_modelo$E0 == e0, ]
      
      # Extraer mx1 para edades 90-109
      edades_extremas <- 85:109
      mx_vals <- tabla_filtrada[tabla_filtrada$age %in% edades_extremas, "mx1"]
      
      # Determinar filas en mxt
      filas_mxt <- if (sexo == "Male") edades_extremas + 1 else edades_extremas + 111
      
      # 6. Determinamos columna en mxt
      col_mxt <- which(colnames(mxt) == año)
      
      # 7. Imputar valores
      mxt[filas_mxt, col_mxt] <- mx_vals
    }
  }
  return(mxt)
}

ev[,3] = round(ev[,3])
mxt = imputar_mx_edades_extremas(tabla_modelo, ev, Base_CD_TEM, años = 1950:2019)
mxt = as.data.frame.matrix(mxt)

sex_eng <- rep(c("Male", "Female"), each = 110)
# Convertir mxt a data frame largo
df <- as.data.frame(mxt)
colnames(df) <- as.character(years)
df$Edad <- rep(age, 2)
df$Sexo <- sex_eng

# Reorganizar en formato largo
df_largo <- df %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"),
               names_to = "Año",
               values_to = "Tasa") %>%
  mutate(Año = as.integer(Año))

# Filtrar para hombres y todas las edades
df_hombres <- df_largo %>%
  filter(Sexo == "Male", Año >= 1950, Año <= 2019)
df_mujeres <- df_largo %>%
  filter(Sexo == "Female", Año >= 1950, Año <= 2019)

ln_x_t = log(mxt) 
ax = as.data.frame(rowMeans(ln_x_t[, c("2018", "2019")], na.rm = TRUE))  
colnames(ax) = "ax"

ax$Edad <- rep(0:109, 2)
ax$Sexo <- rep(c("Hombre", "Mujer"), each = 110)

ln_x_t_centrada <- ln_x_t %>%
  mutate(across(everything(), ~ .x - ax[,1]))

svd_resultado <- svd(ln_x_t_centrada, nu =220, nv = 70)
U <- svd_resultado$u   # Edad
D <-  svd_resultado$d  # Valores singulares
V <- (svd_resultado$v) # Tiempo   

var_explicada <- svd_resultado$d^2 / sum(svd_resultado$d^2)
var_explicada_acum <- cumsum(var_explicada)
df <- data.frame(
  Componente = 1:length(var_explicada_acum),
  Varianza = var_explicada_acum)

# Valor del punto 2
punto2 <- df[df$Componente == 2, ]
punto2$label <- paste0(round(punto2$Varianza*100, 2), "%")

# Primer componente
bx1 <- U[, 1]          # patrón de edad 1
kt1 <- D[1] * V[, 1]  # patrón temporal 1
# Segundo componente
bx2 <- U[, 2]          # patrón de edad 2
kt2 <- D[2] *  V[, 2]  # patrón temporal 2
# Realizamos un cambio de signo
bx1 <- -bx1
kt1 <- -kt1
bx2 <- -bx2
kt2 <- -kt2

df_bx <- data.frame(
  Edad = rep(0:109, 2),
  Sexo = rep(c("Hombre", "Mujer"), each = 110),
  bx1 = bx1,
  bx2 = bx2
)
df_kt1 <- data.frame(
  Año = 1950:2019,
  kt1 = kt1
)
df_kt2 <- data.frame(
  Año = 1950:2019,
  kt2 = kt2
)

modelo_kt1 <- Arima(kt1, order = c(2,1,1), include.drift  = TRUE) 
kt1_proy <- forecast(modelo_kt1, h = 51)  # por ejemplo, hasta 2070

modelo_kt2 <- Arima(kt2, order = c(1,1,0)) 
kt2_proy <- forecast(modelo_kt2, h = 51)  # por ejemplo, hasta 2070

comp1 <- outer(bx1, kt1_proy$mean, "*")
comp2 <- outer(bx2, kt2_proy$mean, "*")

a = as.data.frame.matrix(comp1 + comp2 + ax$ax)
mxt_proy = as.data.frame.matrix(exp(comp1 + comp2 + ax$ax))

colnames(mxt_proy) = seq(2020,2070)
rownames(mxt_proy) = seq(1,220)

años_proy <- 2020:(2019 + length(kt1_proy$mean))
kt1_proy_df <- data.frame(
  año = 2020:(2019 + length(kt1_proy$mean)),
  valor = kt1_proy$mean,
  tipo = "Proyeccion"
)
kt1_conc_df <- data.frame(
  año = 1950:2019,
  valor = kt1,
  tipo = "Conciliacion"
)
kt1_df <- rbind(kt1_conc_df, kt1_proy_df)

renglon_2019 <- data.frame(año = tail(kt1_conc_df[[1]], 1),valor = tail(kt1_conc_df[[2]], 1), tipo = "Proyección")
kt1_proy_df <- rbind(renglon_2019, kt1_proy_df)
kt1_df <- rbind(kt1_conc_df, kt1_proy_df)

años_proy <- 2020:(2019 + length(kt2_proy$mean))
kt2_proy_df <- data.frame(
  año = 2020:(2019 + length(kt2_proy$mean)),
  valor = kt2_proy$mean,
  tipo = "Proyección"
)

kt2_conc_df <- data.frame(
  año = 1950:2019,
  valor = kt2,
  tipo = "Conciliación"
)

kt2_df <- rbind(kt2_conc_df, kt2_proy_df)

renglon_2019 <- data.frame(año = tail(kt2_conc_df[[1]], 1),valor = tail(kt2_conc_df[[2]], 1), tipo = "Proyección")
kt2_proy_df <- rbind(renglon_2019, kt2_proy_df)
kt2_df <- rbind(kt2_conc_df, kt2_proy_df)

bx1_df <- data.frame(
  edad = rep(0:109, 2),
  sexo = rep(c("Hombres", "Mujeres"), each = 110),
  sensibilidad = bx1
)

edad <- rep(0:109, 2)
sexo <- rep(c("Hombres", "Mujeres"), each = 110)
mxt = as.data.frame.matrix(mxt)

mxt_conc_largo <- mxt %>%
  mutate(edad = edad, sexo = sexo) %>%
  pivot_longer(cols = -c(edad, sexo), names_to = "año", values_to = "mxt") %>%
  mutate(año = as.numeric(sub("X", "", año)), tipo = "Conciliación")

mxt_proy_largo <- mxt_proy %>%
  mutate(edad = edad, sexo = sexo) %>%
  pivot_longer(cols = -c(edad, sexo), names_to = "año", values_to = "mxt") %>%
  mutate(año = as.numeric(gsub("[^0-9]", "", año)), tipo = "Proyección")

mxt_total <- bind_rows(mxt_conc_largo, mxt_proy_largo)

df_largo = mxt_total

df_hombres <- df_largo %>%
  filter(sexo == "Hombres", año >= 1950, año <= 2019)

df_mujeres <- df_largo %>%
  filter(sexo == "Mujeres", año >= 1950, año <= 2019)

tabla_vida <- function(df, radix = 100000, n = 1) 
{
  calc_tabla <- function(subdf) {
    subdf <- subdf %>% arrange(edad)
    mx <- subdf$mxt
    w <- length(mx)
    # n_ax
    nax <- rep(n/2, w)
    # qx
    qx <- (n * mx) / (1 + (n - nax) * mx)
    qx[w] <- 1
    # px
    px <- 1 - qx
    # lx
    lx <- numeric(w)
    lx[1] <- radix
    for (i in 1:(w - 1)) 
    {
      lx[i + 1] <- lx[i] * px[i]
    }
    # dx
    dx <- lx * qx
    # Lx
    Lx <- numeric(w)
    for (i in 1:(w - 1)) 
    {
      Lx[i] <- n * lx[i + 1] + nax[i] * dx[i]
    }
    Lx[w] <- nax[w] * dx[w]
    # Tx
    Tx <- rev(cumsum(rev(Lx)))
    # ex
    ex <- Tx / lx
    subdf %>%
      mutate(
        qx = qx,
        px = px,
        lx = lx,
        dx = dx,
        Lx = Lx,
        Tx = Tx,
        ex = ex
      )
  }
  df %>%
    group_by(sexo, año, tipo) %>%
    group_modify(~ calc_tabla(.x)) %>%
    ungroup()
}

resultado <- tabla_vida(mxt_total)

df_e0 <- resultado %>%
  filter(edad == 0)

relaciones_sobrevivencia <- function(df_tidy, n = 1) {
  df_tidy %>%
    arrange(sexo, año, edad) %>%
    group_by(sexo, año) %>%
    mutate(
      # S_x = l_{x+n} / l_x
      Sx = lead(lx, n) / lx,
      # Relación desde el nacimiento
      S0x = lx / first(lx)
    ) %>%
    ungroup()
}

df_surv <- relaciones_sobrevivencia(resultado)

write.csv(df_surv, "data/lifetable-1950-2070.csv")

