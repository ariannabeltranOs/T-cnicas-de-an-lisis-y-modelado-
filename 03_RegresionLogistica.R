# =============================================================================
# TEMA 3: REGRESIÓN LOGÍSTICA

# en este script cubrimos:
#   1-Fundamentos de la regresión logística binaria
#   2-Modelo logístico: presencia/ausencia de Cladonia fimbriata en función del pH
#   3-Evaluación del modelo (matriz de confusión, pseudo R²)
#   4-Visualización de la curva logística


# ¿Cuándo usar regresión logística?
# Cuando la variable respuesta es BINARIA (0/1, sí/no, presencia/ausencia)
# NO se puede usar regresión lineal porque las probabilidades deben estar entre 0 y 1
# La regresión logística modela: P(Y=1) = 1 / (1 + e^-(b0 + b1*x))
# =============================================================================



# PARTE 1: cargado de datos y preparación 


# Cargamos el paquete vegan, que contiene datos de comunidades vegetales
# install.packages("vegan")  # Descomentar si no está instalado
library(vegan)

# Cargamos dos datasets del paquete vegan:
# - varespec: cobertura de especies de plantas en 24 sitios de tundra
# - varechem: variables químicas del suelo en esos mismos 24 sitios
data(varespec)
data(varechem)

# Usamos varechem como nuestro dataframe principal (variables predictoras)
df <- varechem

# Exploramos la estructura de los datos
str(df)
head(df)

# Variables disponibles en varechem:
# N, P, K, Ca, Mg, S, Al, Fe, Mn, Zn, Mo, Baresoil, Humdepth, pH

# PARTE 2: creamos la variable respuesta binaria
# -----------------------------------------------------------------------------

# Pregunta ecológica: ¿El pH del suelo determina si Cladonia fimbriata
# está presente o ausente en un sitio?

# Cladonia fimbriata es un liquen sensible a la acidez del suelo
# Creamos variable binaria: en donde
# 1 = presencia (cobertura > 0 en varespec)
# 0 = ausencia  (cobertura = 0)
df$presencia <- ifelse(varespec$Cladfimb > 0, 1, 0)

# Revisamos cuántos sitios tienen presencia vs ausencia
table(df$presencia)
# La mayoría de sitios tienen presencia del liquen


# PARTE 3: modelo de regresion logistica

# Ajustamos el modelo logístico con glm()
# family = binomial indica que la variable respuesta es binaria (0/1)
# link = "logit" es el enlace por defecto para distribución binomial
modelo1 <- glm(presencia ~ pH,
               data   = df,
               family = binomial)

# Resumen del modelo
summary(modelo1)

# Interpretación de los coeficientes:
# - (Intercept) = 24.089: log-odds de presencia cuando pH = 0
# - pH = -7.080: por cada unidad que aumenta el pH, el log-odds de presencia
#   disminuye en 7.08 (a mayor pH, menor probabilidad de presencia del liquen)
# - p-valor pH = 0.068: marginalmente significativo pues está cerca del umbral 0.05


# ddds ratios ya que es más fácil de interpretar

# Exponenciamos los coeficientes para obtener odds ratios
exp(coef(modelo1))

# Interpretación del OR:
# - OR del ph 0.00084: por cada unidad que aumenta el pH,
#   las probabilidades de presencia se reducen a menos del 0.1% de lo que eran
# y esto indica que cladonia fimbriata prefiere suelos más acidos, o sea un ph mas bajo



# PARTE 4: evaluación del modelo 


# Matriz de confusión 

# Calculamos las probabilidades predichas por el modelo
# Si P > 0.5 → predecimos presencia (1), si P <= 0.5 → ausencia (0)
predichos <- ifelse(fitted(modelo1) > 0.5, 1, 0)

# Comparamos valores reales vs predichos
table(Real = df$presencia, Predicho = predichos)

# Interpretación:
# - Verdaderos positivos: sitios con presencia real que el modelo predijo como presencia
# - Falsos negativos: sitios con presencia real que el modelo predijo como ausencia
# - El modelo tiene alta sensibilidad  o sea que detecta bien la presencia
# - Pero tiene baja especificidad pues confunde algunas ausencias como presencias


#Pseudo R2 de mcfadden

# En regresión logística no existe el R2 tradicional
# el pseudo R2 de mcfaadden es el equivalente: valores entre 0.2 y 0.4 son buenos
pseudo_r2 <- 1 - (modelo1$deviance / modelo1$null.deviance)
pseudo_r2

# Interpretación:
# - Pseudo R2 0.364: el pH explica aproximadamente el 36% de la variación
#   en la presencia/ausencia de cladonia fimbriata
# Es un ajuste razonable considerando que solo usamos un predictor



# PARTE 5: visualizacion de la curva logistica 


library(ggplot2)

# Creamos una secuencia de valores de pH para graficar la curva predicha
pH_seq <- seq(min(df$pH), max(df$pH), length.out = 200)

# Calculamos las probabilidades predichas para cada valor de pH
prob_pred <- predict(modelo1,
                     newdata = data.frame(pH = pH_seq),
                     type    = "response")  # "response" devuelve probabilidades (0-1)

# Creamos dataframe para graficar
curva_df <- data.frame(pH = pH_seq, probabilidad = prob_pred)

# Graficamos la curva logística con los datos reales
ggplot() +
  # Puntos reales (0 = ausencia, 1 = presencia)
  geom_jitter(data = df,
              aes(x = pH, y = presencia),
              height = 0.02, alpha = 0.6, color = "steelblue", size = 2) +
  # Curva logística predicha
  geom_line(data = curva_df,
            aes(x = pH, y = probabilidad),
            color = "firebrick", linewidth = 1.2) +
  # Línea de umbral de decisión (P = 0.5)
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  labs(title    = "Regresión logística: Presencia de cladonia fimbriata",
       subtitle = "Variable predictora: ph del suelo",
       x        = "ph del suelo",
       y        = "Probabilidad de presencia") +
  theme_minimal()

# Interpretación de la gráfica:
# - La curva en forma de S  muestra cómo cambia la probabilidad con el ph
# - A ph bajo hay alta probabilidad de presencia del liquen
# y a ph alto  hay mas baja probabilidad de presencia
# - la línea punteada en 0.5 es el umbral de clasificación 

