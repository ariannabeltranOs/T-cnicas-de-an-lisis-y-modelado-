# =============================================================================
# TEMA 1: MODELO DE REGRESIÓN LINEAL

# Een este este script cubrimos los siguientes puntos:
#   1- Regresión lineal simple (datos simulados)
#   2- Matrices de correlación (datos de pingüinos)
#   3- Regresión lineal simple con datos reales (pingüinos Adelie)
#   4- Regresión lineal múltiple (pingüinos Adelie)
#   5- Regresión lineal con datos de incendios forestales
# =============================================================================


# -----------------------------------------------------------------------------
# PARTE 1: REGRESIÓN LINEAL SIMPLE CON DATOS SIMULADOS

# Fijamos una semilla para que los resultados sean reproducibles
set.seed(123)

# Simulamos una variable independiente x con distribución normal (100 datos)
x <- rnorm(100)

# Simulamos el error aleatorio del modelo (ruido)
e <- rnorm(100, mean = 0, sd = 1)

# Creamos la variable dependiente y con una relación lineal: y = 2 + 1.5x + error
# Intercepto = 2, Pendiente real = 1.5
y <- 2 + 1.5 * x + e

# Visualizamos la dispersión de los datos
plot(x, y, main = "Dispersión de datos simulados", xlab = "x", ylab = "y")

# Creamos un data frame con los datos
datos <- data.frame(x, y)

# Ajustamos el modelo de regresión lineal simple: y ~ x
# lm() = "linear model"
reg.simple <- lm(y ~ x, data = datos)

# Vemos el resumen del modelo:
# - Coeficientes (intercepto y pendiente)
# - R² (qué tan bien explica el modelo la variación de y)
# - p-valores (si los coeficientes son significativos)
summary(reg.simple)


# --- Cálculo manual de los valores del summary ---

# Pendiente: covarianza(x,y) / varianza(x)
cov(x, y) / var(x)

# Coeficiente de determinación R² (manual)
SCE <- sum((mean(datos$y) - reg.simple$fitted)^2)  # Suma de cuadrados explicada
SCT <- sum((datos$y - mean(datos$y))^2)             # Suma de cuadrados total
R2  <- SCE / SCT
R2  # Debe coincidir con el R² del summary

# Verificación con función de R
summary(reg.simple)$r.squared

# Graficamos los datos con la recta de regresión ajustada
plot(x, y, main = "Regresión lineal simple")
abline(reg.simple, lwd = 2, col = "blue")  # abline() agrega la recta del modelo

# R-ajustada: estandariza la relación entre x e y
# Es equivalente a la correlación de Pearson en regresión simple
beta <- summary(reg.simple)$coeff[2, 1]
beta * sd(x) / sd(y)    # Calculada manualmente
sqrt(summary(reg.simple)$r.squared)  # Raíz del R²
cor(x, y)               # Correlación de Pearson

# -----------------------------------------------------------------------------
# PARTE 2: matrices de correlación (Pingüinos Palmer)

# Cargamos el paquete con datos de pingüinos del Archipiélago Palmer, Antártida
install.packages("palmerpenguins")
library(palmerpenguins)

# Exploramos la estructura del dataset
# 344 pingüinos de 3 especies, con medidas morfológicas
str(penguins)

# Filtramos solo los pingüinos de la especie adelie
Ad <- subset(penguins, species == "Adelie")

# Matriz de dispersión para pingüinos Adelie (columnas 3 a 6: medidas corporales)
# lower.panel = NULL elimina el panel inferior para no duplicar gráficos
pairs(Ad[, 3:6], pch = 19, col = "blue", lower.panel = NULL,
      main = "Dispersión - Pingüinos Adelie")

# Matriz de dispersión para TODAS las especies con colores distintos para cada especie
pairs(penguins[, 3:6],
      pch = 19,
      col = c("red", "orange", "blue")[penguins$species],
      lower.panel = NULL,
      main = "Dispersión - Todas las especies")

# Matriz de correlación con coeficientes de Pearson, solo Adelie

install.packages("psych")  
library(psych)

pairs.panels(Ad[, 3:6],
             method   = "pearson",  # Tipo de correlación
             density  = FALSE,      # Sin curvas de densidad
             ellipses = FALSE,      # Sin elipses de confianza
             lm       = TRUE)       # Agrega recta de regresión



# PARTE 3: regresion lineal simple con datos reales
# -----------------------------------------------------------------------------

# Pregunta: ¿La masa corporal predice la longitud del pico en pingüinos Adelie?

# Ajustamos el modelo: longitud del pico ~ masa corporal

modelo1 <- lm(Ad$bill_length_mm ~ Ad$body_mass_g)
summary(modelo1)

# Interpretación clave:
# - Intercepto (~26.99): longitud del pico cuando masa = 0 (no tiene sentido biológico)
# - Pendiente (0.003188): por cada gramo extra de masa, el pico crece ~0.003 mm
# - R² = 0.30: la masa corporal explica el 30% de la variación en longitud del pico
# - p-valor < 0.001: la relación es estadísticamente significativa


# PARTE 4: REGRESIÓN LINEAL MÚLTIPLE (Pingüinos Adelie)
# -----------------------------------------------------------------------------

# Ahora incluimos DOS predictores: masa corporal + profundidad del pico 

# Convertimos la profundidad del pico en una variable categórica, con 3 niveles
Ad$bill_depth_cat <- cut(Ad$bill_depth_mm, 3,
                         labels = c("Pequeño", "Mediano", "Grande"))

# Modelo múltiple: longitud del pico ~ masa corporal + categoría de profundidad
model.Ad_cat <- lm(bill_length_mm ~ body_mass_g + bill_depth_cat, data = Ad)

# Visualizamos el efecto de cada predictor por separado
install.packages("visreg")
library(visreg)

# gráfica: efecto de la masa corporal según la categoría de profundidad del pico

visreg(model.Ad_cat, "body_mass_g", "bill_depth_cat", gg = TRUE)

# Interpretación: En los tres grupos, a mayor masa corporal, mayor longitud del pico, y la pendiente es similar en las tres categorías pues las líneas son casi paralelas


# PARTE 5: REGRESIÓN LINEAL CON DATOS DE INCENDIOS FORESTALES
# -----------------------------------------------------------------------------

# Leemos el archivo de incendios forestales del Parque Natural de Montesinho, Portugal
# Variables: temperatura, humedad, viento, índices FWI, área quemada
fires <- read.csv("forestfires.csv")

# --- Exploración visual ---

library(ggplot2)

# El área quemada tiene distribución muy sesgada pues hay muchos ceros
ggplot(fires, aes(x = area)) +
  geom_histogram(bins = 40, fill = "firebrick", color = "white") +
  labs(title = "Distribución del área quemada",
       x = "Área (ha)", y = "Frecuencia") +
  theme_minimal()

# Aplicamos transformación logarítmica para normalizar la distribución
# log(área + 1) evita log(0) que es indefinido
fires$log_area <- log(fires$area + 1)

ggplot(fires, aes(x = log_area)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribución log(área + 1)",
       x = "log(Área)", y = "Frecuencia") +
  theme_minimal()

# modelo: ¿La temperatura predice el área quemada? ---

modelo_simple <- lm(log_area ~ temp, data = fires)
summary(modelo_simple)

# Interpretación:
# - R² = 0.003: la temperatura sola explica menos del 1% de la variación
# - p-valor = 0.225: la relación no es estadísticamente significativa
# - conclusión: la temperatura por sí sola no es un buen predictor del área quemada

# Gráfica de dispersión con recta de regresión
ggplot(fires, aes(x = temp, y = log_area)) +
  geom_point(alpha = 0.4, color = "firebrick") +
  geom_smooth(method = "lm", color = "navy") +
  labs(title = "Temperatura vs. Área quemada",
       x = "Temperatura (°C)",
       y = "log(Área + 1)") +
  theme_minimal()


# =============================================================================
# REFERENCIAS
# - Palmer Penguins: Horst AM, Hill AP, Gorman KB (2020)
# - Forest Fires: Cortez & Morais (2007), UCI Machine Learning Repository
# - https://bookdown.org/fxpalacio/bookdown_curso/
# =============================================================================
