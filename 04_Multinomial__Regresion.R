
# REGRESIÓN LOGÍSTICA MULTINOMIAL 

#cargamos liberias
# nnet: contiene la función multinom() para regresión logística multinomial
library(nnet)

# tidyverse: para manejo y visualización de datos (ggplot2, dplyr, etc.)
library(tidyverse)

# car: para la prueba de Wald (Wald Test)
library(car)


# 1. cargamos y preparamos los datos 

# Cargar el dataset MAR 

datos <- read.csv("M.A.R_Cleaned.csv", stringsAsFactors = FALSE)

# Revisar la estructura general del dataset
str(datos)
head(datos)

# Variable dependiente: culgrieve, son quejas culturales 
# Sus categorías son:
#   0 = Sin quejas culturales 
#   1 = Quejas por restricciones religiosas
#   2 = Quejas por restricciones lingüísticas
#   3 = Quejas por restricciones tanto religiosas como lingüísticas

# Convertir la variable dependiente a factor para que r la trate como categórica
# El nivel de referencia será 0, o sea sin quejas culturales 
datos$culgrieve <- factor(datos$culgrieve)

# Verificamos las categorías
table(datos$culgrieve)

# Variables independientes que usaremos:
#   - poldisc: discriminación política (0-4, donde mayor = más discriminación)
#   - ecdisc:  discriminación económica (0-4, donde mayor = más discriminación)
#   - grplang: el grupo tiene idioma distinto al oficial (0=no, 1=sí)

# Eliminar filas con valores faltantes en las variables de interés
datos_limpio <- datos %>%
  filter(!is.na(culgrieve) & !is.na(poldisc) & !is.na(ecdisc) & !is.na(grplang))

cat("Observaciones después de limpiar NAs:", nrow(datos_limpio), "\n")

# PASO 1: visualizar la variable independiente y dependiente

# Gráfica de barras: distribución de los tipos de quejas culturales
ggplot(datos_limpio, aes(x = culgrieve, fill = culgrieve)) +
  geom_bar() +
  scale_x_discrete(
    labels = c("0" = "Sin quejas",
               "1" = "Religiosas",
               "2" = "Lingüísticas",
               "3" = "Religiosas y\nLingüísticas")
  ) +
  labs(
    title = "Distribución de Tipos de Quejas Culturales",
    subtitle = "Dataset Minorities at Risk (MAR)",
    x = "Tipo de Queja Cultural",
    y = "Frecuencia",
    fill = "Categoría"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Gráfica de relación entre discriminación política y tipo de queja cultural
ggplot(datos_limpio, aes(x = culgrieve, y = poldisc, fill = culgrieve)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = c("0" = "Sin quejas",
               "1" = "Religiosas",
               "2" = "Lingüísticas",
               "3" = "Relig. y Ling.")
  ) +
  labs(
    title = "Discriminación Política por Tipo de Queja Cultural",
    x = "Tipo de Queja Cultural",
    y = "Nivel de Discriminación Política",
    fill = "Categoría"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# PASO 2: ajustamos los modelos de regresion logistica multinomial

# La función multinom() del paquete nnet ajusta la regresión logística multinomial
# La categoría de referencia es el primer nivel del factor que en este caso es 0

#  Modelo 1: Solo con discriminación política , o sea un modelo reducido
modelo1 <- multinom(culgrieve ~ poldisc, data = datos_limpio)

# Modelo 2: Modelo completo con discriminación política, económica e idioma
modelo2 <- multinom(culgrieve ~ poldisc + ecdisc + grplang, data = datos_limpio)

# Vemos un resumen del modelo 2, con coeficientes en log-odds
summary(modelo2)

# PASO 3: interpretaci{on del modelo

# 3a. Calcular valores p manualmente ---
# multinom() no devuelve valores p directamente; se calculan a partir de los zscores

# Obtener coeficientes y errores estándar
coef_mod2   <- summary(modelo2)$coefficients
errores_mod2 <- summary(modelo2)$standard.errors

# Calcular estadísticos z
z_scores <- coef_mod2 / errores_mod2

# Calcular valores p, prueba bilateral
p_values <- (1 - pnorm(abs(z_scores), 0, 1)) * 2

cat("\n=== VALORES P DEL MODELO 2 ===\n")
print(round(p_values, 4))

# 3b. Convertir coeficientes a Razones de Probabilidad (odds ratios) ---
# Los odds ratios facilitan la interpretación:
# OR > 1 aumenta la probabilidad de esa categoría vs. la referencia (cat. 0)
# OR < 1  disminuye la probabilidad de esa categoría vs. la referencia (cat. 0)

odds_ratios <- exp(coef_mod2)

cat("\n=== ODDS RATIOS DEL MODELO 2 ===\n")
print(round(odds_ratios, 3))

# 3c. Interpretamos un coeficiente clave, como ejemplo: grplang
# Para la categoría "quejas lingüísticas" (2) vs. "sin quejas" (0):
OR_grplang_cat2 <- exp(coef_mod2["2", "grplang"])
cat("\nOR de grplang para categoría 2 (Lingüísticas vs. Sin quejas):",
    round(OR_grplang_cat2, 3), "\n")
cat("Interpretación: Los grupos con idioma diferente tienen",
    round(OR_grplang_cat2, 1), "veces más probabilidades de tener\n",
    "quejas lingüísticas en comparación con no tener quejas culturales.\n")

# 3d. Probabilidades predichas
# Calculamos la probabilidad promedio predicha para cada categoría de queja

prob_predichas <- predict(modelo2, type = "probs")

# Promediamos las probabilidades predichas de cada categoría
probs_promedio <- colMeans(prob_predichas, na.rm = TRUE)
cat("\n=== PROBABILIDADES PROMEDIO PREDICHAS ===\n")
print(round(probs_promedio, 4))

# Visualizamos probabilidades predichas según nivel de discriminación política
# y creamos un dataset de predicción variando poldisc
datos_pred <- data.frame(
  poldisc = rep(0:4, 3),
  ecdisc  = rep(mean(datos_limpio$ecdisc, na.rm = TRUE), 15),
  grplang = rep(c(0, 1, 0), each = 5)  # comparar con y sin idioma distinto
)

prob_pred_df <- cbind(datos_pred, predict(modelo2, newdata = datos_pred, type = "probs"))

# Reshape para ggplot
prob_long <- prob_pred_df %>%
  pivot_longer(cols = c("0", "1", "2", "3"),
               names_to = "Categoria",
               values_to = "Probabilidad") %>%
  mutate(Categoria = recode(Categoria,
    "0" = "Sin quejas",
    "1" = "Religiosas",
    "2" = "Lingüísticas",
    "3" = "Religiosas y Lingüísticas"
  ))

# Graficar solo para grplang = 0, el grupo sin idioma diferente
prob_long %>%
  filter(grplang == 0) %>%
  ggplot(aes(x = poldisc, y = Probabilidad, color = Categoria)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Probabilidades Predichas por Tipo de Queja Cultural",
    subtitle = "Según nivel de discriminación política (grupos sin idioma distinto)",
    x = "Discriminación Política",
    y = "Probabilidad Predicha",
    color = "Tipo de Queja"
  ) +
  theme_minimal()

# PASO 4: verificamos supuestos: independencia de alternativas irrelevantes (IIA)

# El supuesto principal de la regresión logística multinomial es la (IIA)
# La razón de probabilidades entre dos categorías no debe cambiar si se
# agrega o elimina una tercera categoría.

# Una forma de evaluar esto informalmente es ajustar modelos excluyendo
# una categoría y comparar si los coeficientes cambian significativamente.

# Modelo excluyendo la categoría 1 conquejas religiosas
datos_sin_cat1 <- datos_limpio %>% filter(culgrieve != "1")
datos_sin_cat1$culgrieve <- droplevels(datos_sin_cat1$culgrieve)

modelo_sin_cat1 <- multinom(culgrieve ~ poldisc + ecdisc + grplang,
                             data = datos_sin_cat1)

cat("\n=== Coeficientes del modelo completo (categorías 0, 1, 2, 3) ===\n")
print(round(coef(modelo2), 4))

cat("\n=== Coeficientes del modelo sin categoría 1 (0, 2, 3) ===\n")
print(round(coef(modelo_sin_cat1), 4))

cat("\nSi los coeficientes son similares, el supuesto IIA se sostiene.\n")


# PASO 5: comparamos modelos - AIC, BIC y prueba de razón de verosimilitud


# AIC y BIC 
# Menor valor = mejor ajuste penalizando la complejidad del modelo

cat("\n=== COMPARACIÓN DE MODELOS ===\n")
cat("Modelo 1 (solo poldisc) - AIC:", AIC(modelo1), "| BIC:", BIC(modelo1), "\n")
cat("Modelo 2 (poldisc + ecdisc + grplang) - AIC:", AIC(modelo2), "| BIC:", BIC(modelo2), "\n")

# Diferencia en AIC (regla práctica: diferencia > 10 indica modelo claramente mejor)
dif_AIC <- AIC(modelo1) - AIC(modelo2)
cat("Diferencia en AIC (Modelo1 - Modelo2):", round(dif_AIC, 2), "\n")
cat("Si la diferencia es positiva y grande, el Modelo 2 tiene mejor ajuste.\n")

# Prueba de razón de verosimilitudt
# Compara si el modelo completo mejora significativamente sobre el reducido
# H0: los modelos tienen el mismo ajuste
# Si p < 0.05, el modelo completo es significativamente mejor

lrt <- anova(modelo1, modelo2, test = "Chisq")
cat("\n=== PRUEBA DE RAZÓN DE VEROSIMILITUD ===\n")
print(lrt)

# PASO 6: PRUEBA DE WALD para el modelo 2


# La prueba de Wald evalúa si una variable es significativa en su conjunto
# considerando todas las categorías de la variable dependiente a la vez
# Útil para variables con múltiples categorías como "región"

# Prueba de Wald para cada variable en el modelo
cat("\n=== PRUEBA DE WALD - Variable poldisc ===\n")
print(linearHypothesis(modelo2, c("1:poldisc = 0", "2:poldisc = 0", "3:poldisc = 0")))

cat("\n=== PRUEBA DE WALD - Variable grplang ===\n")
print(linearHypothesis(modelo2, c("1:grplang = 0", "2:grplang = 0", "3:grplang = 0")))

