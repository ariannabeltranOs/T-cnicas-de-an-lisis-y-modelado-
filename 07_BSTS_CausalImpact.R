#SERIES DE TIEMPO ESTRUCTURALES CON BAYES (BSTS)

# Las Series de Tiempo Estructurales Bayesianas (BSTS) modelan una serie
# de tiempo descomponiéndola en tendencia, estacionalidad y regresión.
# causalimpact BSTS para estimar qué hubiera pasado sin la intervención
# contrafactual y compararlo con lo que sí ocurrió


#instalamos los paquetes necesarios 
install.packages(c("CausalImpact", "zoo"))  
library(CausalImpact)  # Modelo BSTS para análisis de impacto causal
library(zoo)           # Manejo de series de tiempo irregulares
library(tidyverse)


# 1. cargamos y preparamos datos


# Datos de calidad del agua de la bhía de chsapeake, leemos los datos

datos <- read.csv("BKB_WaterQualityData_2020084.csv", stringsAsFactors = FALSE)

# Convertimos la fecha al formato correcto
datos$Read_Date <- as.Date(datos$Read_Date, format = "%m/%d/%Y")

# Filtramos el sitio "Bay" y seleccion lo que queremos analizar
# temp_agua, pH, salinidad, secchi = variables predictoras
bay <- datos %>%
  filter(Site_Id == "Bay") %>%
  select(Read_Date,
         oxigeno   = "Dissolved.Oxygen..mg.L.",
         temp_agua = "Water.Temp...C.",
         pH        = "pH..standard.units.",
         salinidad = "Salinity..ppt.",
         secchi    = "Secchi.Depth..m.")

# Convertimos a numérico y ordenamos por fecha
bay <- bay %>%
  mutate(across(-Read_Date, as.numeric)) %>%
  arrange(Read_Date)

# Eliminamos filas con datos faltantes en variables clave
bay <- bay %>% drop_na(oxigeno, temp_agua, pH, salinidad, secchi)


# 2. definimos los periodos pre y post intervención


# Intervención: 1 de enero de 2010
# El Chesapeake Bay TMDL fue el mayor plan de control de contaminación de la EPA
# Buscaba reducir nutrientes y sedimentos para mejorar la calidad del agua
fecha_intervencion <- as.Date("2010-01-01")

# Periodo PRE:  antes de la intervención 
# Periodo POST: después de la intervención, que es donde medimos el impacto 
pre_period  <- c(min(bay$Read_Date), fecha_intervencion - 1)
post_period <- c(fecha_intervencion, max(bay$Read_Date))


# 3. Ccreamos series de tiempo en formato zoo

# zoo permite manejar series de tiempo con fechas irregulares
# y = variable respuesta: oxígeno disuelto
y <- zoo(bay$oxigeno, bay$Read_Date)

# X = predictores: temperatura, pH, salinidad, profundidad Secchi
# Estas covariables ayudan al modelo a construir el contrafactual
X <- zoo(bay[, c("temp_agua", "pH", "salinidad", "secchi")], bay$Read_Date)

# Combinamos en un solo objeto zoo
datos_zoo <- cbind(y, X)


# 4.ejecutamos el modelo bsts

# CausalImpact ajusta un modelo BSTS en el periodo PRE
# y lo usa para predecir qué hubiera pasado sin la intervención
# Luego compara esa predicción con los datos reales del periodo post
impacto <- CausalImpact(datos_zoo,
                        pre.period  = pre_period,
                        post.period = post_period)


# 5. resultados e interpretacion 


# Resumen numérico del impacto estimado
summary(impacto)

# Reporte narrativo automático en inglés
summary(impacto, "report")

# Gráfica con tres paneles:
# Panel 1: serie original vs contrafactual predicho
# Panel 2: diferencia puntual entre real y predicho
# Panel 3: efecto acumulado de la intervención
plot(impacto)

# INTERPRETACIÓN:
# El modelo estima un efecto promedio de -0.15 mg/L en oxígeno disuelto, itervalo de confianza [-2.6, 2.0] cruza el cero , efecto no signicativo
# Probabilidad de efecto causal = 50%  no hay evidencia clara de impacto
# Esto no sigfica que la intervención no funcionó: el oxígeno disuelto
# puede no ser la variable más sensible a corto plazo para detectar
# reducciones de nutrientes. Variables como clorofila-a o nitrógeno total
# podrían reflejar mejor la respuesta del ecosistema.

