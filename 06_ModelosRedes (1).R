#MODELOS DE REDES
# Red social de delfines nariz de botella en Nueva Zelanda

# Los modelos de redes representan sistemas como grafos, en este caso, nuestros nodos y aristas son representados como: 
# NODOS: individuos (delfines)
# ARISTAS: asociaciones frecuentes entre ellos
#
# Analizamos métricas de centralidad, ¿quién es más importante
# también detectamos comunidades, o sea, que subgrupos existen? 


#instalamos paquetes que nos ayudar a formar nuestras redes
install.packages(c("igraph", "ggraph", "tidygraph", "ggrepel"))
library(igraph)
library(ggraph)
library(tidygraph)
library(ggrepel)


# 1.cargamos la red


# Descargamos la red de 62 delfines con 159 asociaciones 
url  "http://www-personal.umich.edu/~mejn/netdata/dolphins.zip"
temp <- tempfile()
download.file(url, temp)
unzip(temp, exdir = tempdir())

gml_file      <- list.files(tempdir(), pattern = "\\.gml$", full.names = TRUE)
dolphin_graph <- read_graph(gml_file, format = "gml")

# Información básica de la red
print(dolphin_graph)
# U = red No Dirigida | 62 nodos | 159 aristas


# 2. metricas de centralidad}

# Grado: número de conexiones directas de cada delfín
deg <- degree(dolphin_graph)

# Betweenness: cuántas veces actúa como puente entre otros delfines
# (los "brokers" o intermediarios de la red)
bet <- betweenness(dolphin_graph, normalized = TRUE)

# Closeness: qué tan cerca está de todos los demás nodos
clo <- closeness(dolphin_graph, normalized = TRUE)

# obtenemos una tabla resumen de centralidad
centralidad <- data.frame(
  Delfin      = V(dolphin_graph)$label,
  Grado       = deg,
  Betweenness = round(bet, 3),
  Closeness   = round(clo, 3)
)

print(centralidad)

# queremos saber el topm de 5 delfines más importantes 
top5 <- centralidad %>%
  dplyr::arrange(desc(Betweenness)) %>%
  head(5)

cat("Top 5 delfines brokers (intermediadores):\n")
print(top5)

# 3.aplicamos la detecci{on de comunidades con el algoritmo de Louvain


# Louvain detecta subgrupos con más conexiones internas que externas
louvain <- cluster_louvain(dolphin_graph)

cat("Comunidades detectadas y su tamaño:\n")
print(table(membership(louvain)))

# Asignamos comunidad a cada nodo
V(dolphin_graph)$comunidad <- membership(louvain)

# 4. visualizamos


# Convertimos a tidygraph para usar ggraph
dolphin_tbl <- as_tbl_graph(dolphin_graph) %>%
  activate(nodes) %>%
  mutate(comunidad = as.factor(group_louvain()))

# Gráfica con nodos coloreados por comunidad
ggraph(dolphin_tbl, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(color = comunidad), size = 4) +
  geom_node_text(aes(label = label), repel = TRUE, size = 3) +
  scale_color_brewer(palette = "Set2", name = "Comunidad") +
  theme_graph() +
  labs(title    = "Red social de delfines (Doubtful Sound)",
       subtitle = "Colores = comunidades detectadas por algoritmo Louvain")

# INTERPRETACIÓN:
# Cada color representa un subgrupo social de delfines
# los nodos centrales, los que tienen mas conexiones,  son individuos más sociables
# SN100 y Beescratch son los principales brokers de la red ya que conectan diferentes comunidades y son críticos para la cohesión del grupo

