# T-cnicas-de-an-lisis-y-modelado-
Portafolio digital del trabajo en clase 

EL curso de la materia de técnicas de análisis y modelado, busca que el alumno disstinga las técnicas de análisis para el tratamiento y análisis de datos multivariados y no lineales que se emplean para el estudio de las Ciencias Ambientales, a tráves de los siguientes temas, con su respectivo objetivo específico, as{i cómo támbien el ejercicio/aprendizaje obtenido: 

-Modelo de regresión lineal
*Objetivos:Identificar los fundamentos del modelo de regresión lineal simple y múltiple para explicar relaciones entre variables ambientales continuas.
Regresión de Poisson
*Ejercicio/aprendizaje: Se exploraron relaciones entre variables continuas mediante regresión simple y múltiple. Se analizaron datos de pingüinos (Palmer Penguins) e incendios forestales, calculando coeficientes, R², matrices de correlación e interpretación de resultados.

-Regresión de Poisson
*Objetivo: Reconocer el modelo de regresión de Poisson como herramienta estadística para el análisis de datos de conteo en fenómenos ambientales, como abundancia de especies o eventos de contaminación.
*Ejercicio/aprendizaje:Se modelaron datos de conteo de eventos discretos. Se aplicó a avistamientos de jaguares e incendios activos en Centroamérica (datos VIIRS-SUOMI), evaluando el efecto del momento del día, la intensidad del fuego y el país sobre el número de incendios detectados.

-Regresión logística
*Objetivo: Comprender el modelo de regresión logística binaria para clasificar y predecir variables de respuesta dicotómica en contextos ambientales, como presencia/ausencia de una especie.
*Ejercicio/aprendizaje:se utilizó para predecir variables de respuesta binaria. Se modeló la presencia/ausencia de Cladonia fimbriata en función del pH del suelo, usando datos de vegetación de la librería vegan.

-Regresión multinomial
*Objetivo: Aplicar el modelo de regresión logística multinomial para el análisis de variables de respuesta con múltiples categorías en estudios ambientales, como tipos de cobertura vegetal o uso de suelo.
*Ejercicio/aprendizaje: Se modeló el tipo de queja cultural de grupos etnoculturales en riesgo (dataset MAR) usando discriminación política, económica y concentración geográfica como predictores. Al tener una variable respuesta con más de dos categorías, se aplicó regresión multinomial con multinom(), calculando Odds Ratios y probabilidades predichas para cada tipo de queja.


-Ecuaciones diferenciales
*objetivo: Comprender el uso de ecuaciones diferenciales ordinarias y en derivadas parciales para modelar dinámicas ambientales, como crecimiento poblacional, dispersión de contaminantes y flujos de energía.
*Ejercicio/aprendizaje: se implementaron modelos presa-depredador (Lotka-Volterra con capacidad de carga) usando deSolve. Se simuló la dinámica algas-dafnias en un lago y el impacto de pesticidas, y se modeló el ecosistema del Río Esmeralda bajo condiciones sanas e impactadas por una termoeléctrica.

-Modelos de redes
*Objetivo: Analizar la estructura y aplicación de los modelos de redes para representar interacciones ecológicas, flujos de materia y conectividad en sistemas ambientales complejos.
*Ejercicio/aprendizaje:Se analizó la red social de 62 delfines nariz de botella (Tursiops) de Doubtful Sound, Nueva Zelanda. Se calcularon métricas de centralidad (grado, betweenness, closeness, eigenvector) y se detectaron comunidades con los algoritmos Louvain y Girvan-Newman.


-Series de tiempo estructurales con Bayes: 
*Objetivo: Aplicar modelos de series de tiempo estructurales bayesianas (BSTS) para evaluar el impacto causal de una intervención ambiental sobre la calidad del agua.
*Ejercicio/aprendizaje: Se analizó el efecto del plan de control de contaminación Chesapeake Bay TMDL (2010) sobre el oxígeno disuelto en la Bahía de Chesapeake, usando datos históricos de calidad del agua. Mediante el paquete CausalImpact, se ajustó un modelo BSTS que construye un contrafactual (qué hubiera pasado sin la intervención) y lo compara con los datos reales del periodo post-intervención, incorporando temperatura, pH, salinidad y profundidad Secchi como covariables.
