# =============================================================================
# TEMA 2: REGRESIÓN DE POISSON
# =============================================================================
# en este este script cubrimos los siguientes puntos:
#   1. Distribución de Poisson (fundamentos teóricos)
#   2. Modelo de Poisson simple (incendios en Centroamérica - datos VIIRS)
#   3. Modelo de Poisson completo (por país, fecha e intensidad del fuego)
# =============================================================================
# ¿Cuándo usar Poisson?
# Cuando la variable respuesta son CONTEOS, como los números enteros = 0
# ej. número de incendios, avistamientos de especies, casos de enfermedad
# =============================================================================


# PARTE 1: FUNDAMENTOS DE LA DISTRIBUCIÓN DE POISSON
# -----------------------------------------------------------------------------

library(tidyverse)

# La distribución de Poisson modela la probabilidad de que ocurra
# un número k de eventos en un intervalo, dado que la tasa promedio es lambda (λ)

# Ejemplo: En una reserva se avistan en promedio 2 jaguares por mes (lambda = 2) 
# 1. Probabilidad de ver EXACTAMENTE 4 jaguares en un mes
# dpois() = función de densidad (probabilidad puntual)

dpois(x = 4, lambda = 2)
# 9% de probabilidad de ver exactamente 4 jaguares

# 2. probabilidad de ver MÁS DE 3 jaguares: P(X > 3)
# ppois() = función de distribución acumulada

# lower.tail = FALSE calcula la cola superior (P > 3)

ppois(q = 3, lambda = 2, lower.tail = FALSE)

# 14% de probabilidad de ver más de 3 jaguares

# 3. simulación: serie de tiempo aleatoria de avistamientos en 12 meses
# rpois() genera números aleatorios con distribución Poisson
avistamientos_mes <- rpois(n = 12, lambda = 2)

# Graficamos los avistamientos mes a mes
plot(avistamientos_mes,
     type = "b",                                    # "b" = puntos + líneas
     main = "Avistamientos Mensuales Estimados",
     xlab = "Mes", ylab = "Número de avistamientos")


# Visualización comparativa de diferentes lambdas

# Creamos datos para comparar distribuciones con lambda = 1, 4 y 10
x  <- 0:20
df <- data.frame(
  x      = rep(x, 3),
  prob   = c(dpois(x, 1), dpois(x, 4), dpois(x, 10)),
  lambda = factor(rep(c(1, 4, 10), each = 21))
)

# gráfica donde a mayor lambda, la distribución se desplaza a la derecha y se aplana
ggplot(df, aes(x = x, y = prob, fill = lambda)) +
  geom_col(position = "dodge") +
  labs(title  = "Distribución de Poisson con diferentes tasas (lambda)",
       x      = "Número de eventos",
       y      = "Probabilidad") +
  theme_minimal()

# Interpretación: lambda pequeño (1): casi siempre 0 o 1 eventosv y lambda grande, los eventos se distribuyen más ampliamente


# -----------------------------------------------------------------------------
# PARTE 2: MODELO DE POISSON SIMPLE
#  ¿El número de incendios detectados en Centroamérica depende
# del momento del día (día/noche) y la intensidad del fuego (FRP)?

# Cargamos datos de detección de incendios activos 
# FRP = Fire Radiative Power (potencia radiativa del fuego, en MW)
fires <- read_csv("SUOMI_VIIRS_C2_Central_America_7d.csv")

#Limpiamos y obtenemos la preparación de datos ---

# Filtramos solo detecciones confiables como nominal y high
# Eliminamos detecciones de baja confianza que pueden ser falsas alarmas
fires <- fires %>%
  filter(confidence %in% c("nominal", "high"))

# Convertimos la fecha y extraemos mes y año
fires <- fires %>%
  mutate(acq_date = as.Date(acq_date),
         mes      = month(acq_date),
         año      = year(acq_date))

# Contamos incendios por día y por momento (de día a noche)
# Esta será nuestra variable respuesta Y (conteo de eventos)
conteo_diario <- fires %>%
  filter(confidence %in% c("nominal", "high")) %>%
  mutate(acq_date = as.Date(acq_date),
         dia      = as.numeric(acq_date - min(acq_date)) + 1) %>%
  group_by(dia, daynight) %>%
  summarise(
    n_incendios = n(),          # Conteo de incendios
    frp_media   = mean(frp),    # Intensidad promedio del fuego
    .groups     = "drop"
  )

#Modelo de Poisson 

# glm() con family = poisson ajusta un modelo lineal generalizado de poisson
# link = log significa que modelamos el logaritmo del conteo esperado
modelo <- glm(n_incendios ~ daynight + frp_media,
              family = poisson(link = "log"),
              data   = conteo_diario)

summary(modelo)

# Exponenciamos los coeficientes para interpretarlos como razones de tasas (IRR)
exp(coef(modelo))

# Interpretación:
# - (Intercept): tasa base de incendios diurnos con FRP = 0
# - daynightN: de noche hay 62% menos incendios que de día (IRR = 0.377)
# - frp_media: a mayor intensidad del fuego, se detectan ligeramente menos
#   incendios por día (puede reflejar que fuegos muy intensos son menos frecuentes)


# PARTE 3: MODELO DE POISSON COMPLETO (por país)
# Cuántos incendios se detectan por país, controlando por fecha,
# intensidad del fuego y proporción de detecciones nocturnas?
# -----------------------------------------------------------------------------

library(sp)

install.packages("rworldmap") 
library(rworldmap)

# Recargamos y filtramos datos
fires <- read_csv("SUOMI_VIIRS_C2_Central_America_7d.csv")

fires <- fires %>%
  filter(confidence %in% c("nominal", "high")) %>%
  mutate(acq_date = as.Date(acq_date))

# -Asignamos país a cada punto de fuego usando coordenadas ---

# Creamos objeto espacial con las coordenadas de cada incendio
coords  <- fires %>% select(longitude, latitude)
puntos  <- SpatialPoints(coords,
                         proj4string = CRS("+proj=longlat +datum=WGS84"))

# Cargamos mapa mundial y hacemos intersección espacial
mapa_mundo    <- getMap(resolution = "low")
pais_asignado <- over(puntos, mapa_mundo)

# Asignamos el nombre del país a cada registro
fires$pais <- pais_asignado$ADMIN

# Eliminamos puntos en el océano o sin país asignado
fires <- fires %>% filter(!is.na(pais))

# Agrupamos por país y fecha

conteo <- fires %>%
  group_by(pais, acq_date) %>%
  summarise(
    n_incendios = n(),                    # Variable respuesta: conteo de incendios
    frp_media   = mean(frp),             # Intensidad promedio del fuego
    brillo_med  = mean(bright_ti4),      # Brillo térmico promedio
    prop_noche  = mean(daynight == "N"), # Proporción de detecciones nocturnas
    .groups     = "drop"
  )

# Convertimos fecha a numérico para incluirla como predictor continuo
conteo <- conteo %>%
  mutate(dia = as.numeric(acq_date - min(acq_date)) + 1)

# Modelo de Poisson completo

# El país de referencia es "Belize" (primer nivel del factor)
# Los coeficientes de cada país indican cuánto más o menos incendios tienen
# en comparación con Belize
modelo_completo <- glm(n_incendios ~ pais + acq_date + frp_media + prop_noche,
                       family = poisson(link = "log"),
                       data   = conteo)

summary(modelo_completo)

# Interpretamos los coeficientes como incidence rate ratios (IRR)
exp(coef(modelo_completo))

# Interpretación de resultados clave:
# - México tiene 62 veces más incendios que belize en el mismo período, Venezuela es el país con más incendios relativos, prop_noche donde a mayor proporción de detecciones nocturnas,
# menos incendios totales detectados pues los incendios activos son más frecuentes de día


