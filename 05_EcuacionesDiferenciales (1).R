
#  ECUACIONES DIFERENCIALES
# Modelo presa-depredador: algas y dafnias en un lago controlado

# Las ecuaciones diferenciales describen cómo cambia una población en el tiempo.
# as{i que usamos el modelo lotka volterra con capacidad de carga:
#
#   dx/dt = r*x*(1 - x/K) - a*x*y   : Algas siendo la presa 
#   dy/dt = e*a*x*y - m*y            → Dafnias el Depredador
#
# parámetros:
#   r = tasa de crecimiento de las algas
#   K = capacidad de carga del lago
#   a = tasa de ataque, esto es cuánto comen las dafnias
#   e = eficiencia de conversión 
#   m = mortalidad natural de las dafnias

# instalamos paquetes 
install.packages("deSolve")  
library(deSolve)
library(ggplot2)


# 1. definimos el modelo 

# Función con las dos ecuaciones diferenciales
# desolve requiere los argumentos: t (tiempo), state (variables), parameters
modelo_lago <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {

    # Ecuación de las algas: crecimiento logístico menos consumo por dafnias
    dx <- r * x * (1 - x / K) - a * x * y

    # Ecuación de las dafnias: ganancias por alimentación menos mortalidad
    dy <- e * a * x * y - m * y

    return(list(c(dx, dy)))
  })
}

# 2. parametros y condiciones iniciales 

params <- c(r = 1.2, K = 50, a = 0.1, e = 0.4, m = 0.4)

# Poblaciones al inicio de la simulación
estado_inicial <- c(x = 20, y = 5)  # 20 algas, 5 dafnias

# Simulamos 100 días con pasos de 0.1
tiempo <- seq(0, 100, by = 0.1)


# 3. resoluci{on del sistema }


# ode() resuelve numéricamente las ecuaciones usando el método Runge-Kutta
out <- as.data.frame(ode(y = estado_inicial, times = tiempo,
                          func = modelo_lago, parms = params))


# 4. obtenemos una visualización, donde variamos distintos parametros visuales, con geomline o linewidth


ggplot(out, aes(x = time)) +
  geom_line(aes(y = x, color = "Algas (Presa)"),      linewidth = 1) +
  geom_line(aes(y = y, color = "Dafnias (Depredador)"), linewidth = 1) +
  labs(title    = "Dinámica Algas-Dafnias en lago controlado",
       subtitle = "Modelo presa-depredador con capacidad de carga",
       x = "Días", y = "Biomasa", color = "Especie") +
  theme_minimal()


# 5. a{alisis

# Población máxima alcanzada por cada especie
cat("Máximo de algas:  ", round(max(out$x), 2), "\n")
cat("Máximo de dafnias:", round(max(out$y), 2), "\n")

# Día en que ocurrió el pico de dafnias
cat("Pico de dafnias en el día:", out$time[which.max(out$y)], "\n")

# y con summary obtenemos esumen estadístico de la simulación
summary(out)

# =============================================================================
# INTERPRETACIÓN:
# Las poblaciones oscilan al inicio pero llegan a un equilibrio estable, 
# el pico de dafnias ocurre después del pico de algas, que significa un desfase tipico
# luego vemos que la capacidad de carga K=50 limita el crecimiento infinito de las algas
# =============================================================================
