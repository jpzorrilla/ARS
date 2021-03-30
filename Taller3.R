install.packages("data.table")
install.packages("geosphere")

library(igraph)
library("dplyr")
library(data.table)
library("geosphere")

load("cities_t3.RData")

# Análisis para una ventana

# Ventana de 2 años (1980-1981)

years <- sort(unique(cities_t3$application_year))

ass_city_db <- cities_t3

ass_city_db <- ass_city_db[which(ass_city_db$application_year >= years[1] & ass_city_db$application_year <= years[2]),]

# Se crea un vector llamado "componentes", cuyos nodos son los propietarios (ass_city_id) y los vínculos los generan las patentes (patent_id)

componentes <- c("ass_city_id", "patent_id")

temporal <- unique(subset.data.frame(ass_city_db, select = componentes))

colnames(temporal) <- c("nodo", "link")

edList <- temporal %>% 
  group_by(link) %>% 
  filter(n()>=2) %>% 
  group_by(link) %>% 
  do(data.frame(t(combn(.$nodo, 2)), stringsAsFactors=FALSE)) %>% 
  as.data.table

edList <- setNames(subset.data.frame(edList, select = c("X1", "X2", "link")), c(paste(componentes[1], "out", sep = "_"), paste(componentes[1], "in", sep = "_"), componentes[2]))

# Antes de crear la red de propietarios. Se genera una base con los atributos para cada nodo
# de manera de poder cargarlos todos al momento de crear la red.

att_ass <- ass_city_db %>% 
  group_by(ass_city_id) %>% 
  summarise(country = last(country), 
            city_id = last(city_id), 
            lat = last(lat), 
            lon = last(lon), 
            ass_is_latam = last(ass_is_latam)) %>% 
  as.data.table

# Ahora se crea la red con los atributos de los nodos.
# Simplificada = porque aparecerá un vínculo por cada patente, sin ponderar el vínculo.

net_ass_link_pat <- graph_from_data_frame(edList, directed = FALSE, vertices = att_ass) %>% simplify()
summary(net_ass_link_pat)

# Ahora se genera una base con atributos de las ciudades
# Se agrega una métrica de red que surja del análisis de la red de propietarios de la ciudad

att_cities <- ass_city_db %>% 
  group_by(city_id) %>% 
  summarise(n_ass = n_distinct(ass_city_id), 
            n_pat = n_distinct(patent_id), 
            n_inv = n_distinct(inventor_id)) %>% 
  as.data.table

cities <- att_cities$city_id

att_cities$meanDass <- 0

for (j in 1:length(cities)) {
  att_cities[city_id == cities[j], "meanDass"] <- mean(igraph::degree(net_ass_link_pat, v = V(net_ass_link_pat)[city_id == cities[j]]))
}

# Ahora se crea la red de ciudades utilizando el mismo procedimiento que para la de propietarios

componentes <- c("city_id", "patent_id")

temporal <- unique(subset.data.frame(ass_city_db, select = componentes))

colnames(temporal) <- c("nodo", "link")

edList <- temporal %>% 
  group_by(link) %>% 
  filter(n()>=2) %>% 
  group_by(link) %>% 
  do(data.frame(t(combn(.$nodo, 2)), stringsAsFactors=FALSE)) %>% 
  as.data.table

edList <- setNames(subset.data.frame(edList, select = c("X1", "X2", "link")), c(paste(componentes[1], "out", sep = "_"), paste(componentes[1], "in", sep = "_"), componentes[2]))

att_ass <- unique(att_ass[,-1])

att_cities <- merge(att_cities, att_ass, by = "city_id")

net_city_link_pat <- graph_from_data_frame(edList, directed = FALSE, vertices = att_cities) %>% simplify()

summary(net_city_link_pat)

# En gral. se generan atributos para los nodos (obj. de inv. en ARS)
# Pero en este caso vamos a generar un atributo del vínculo que mida la dist. geográfica (geodésica) entre ciudades (nodos) 
# Esto nos permite considerar esta dim. para caracterizar el tipo de relaciones que establece la ciudad

distancias <- as.data.frame(ends(net_city_link_pat, E(net_city_link_pat)))

colnames(distancias) <- c("salida", "llegada")

distancias$id_edge <- 1:length(distancias$salida)

geo_data <- as.data.table(igraph::as_data_frame(net_city_link_pat, what = "vertices")[, c("name", "lon", "lat")])

distancias <- merge(distancias, geo_data, by.x = "llegada", by.y = "name")
distancias <- merge(distancias, geo_data, by.x = "salida", by.y = "name")

distancias$distance <- round(distGeo(distancias[, c("lon.y", "lat.y")], distancias[, c("lon.x", "lat.x")])/1000, 2)

distancias <- arrange(distancias, id_edge)

E(net_city_link_pat)$distance <- distancias$distance
summary(net_city_link_pat)

# Ahora utilizaremos este atributo para construir 2 vars. que nos brinden información de la dist. a la que se encuentran las ciudades con las que se vincula cada ciufdad.
# Para ello consideramos la media y el desvío estándar de la dist. de los vínculos

for (j in 1:vcount(net_city_link_pat)) {
  
  V(net_city_link_pat)$meanDist[j] <- if_else(is.na(mean(incident(net_city_link_pat, v = V(net_city_link_pat)$name[j])$distance)) == TRUE, 0, mean(incident(net_city_link_pat, v = V(net_city_link_pat)$name[j])$distance))
  
  V(net_city_link_pat)$sdDist[j] <- if_else(is.na(sd(incident(net_city_link_pat, v = V(net_city_link_pat)$name[j])$distance)) == TRUE, 0, sd(incident(net_city_link_pat, v = V(net_city_link_pat)$name[j])$distance))
  
}

# Eventualmente también nos podría interesar tener alguna otra medida de resumen sobre las características de las ciudades con las que se vinvula cada ciudad
# En este caso contaremos con cuantas ciudades de LatAm se víncula c/ciudad
# Para hacerlo, dentro del "for" se crea un grafo con los de las ciudades que se vinculan a esta ciudad.
# A esto se le llama "egonetwork" donde el "ego" es la ciudad que estoy analizando
# En este enfoque se considera el vecindario de grado 1 o mayor del "ego" y se construye una red sin él.

for (j in 1:vcount(net_city_link_pat)) {
  g <- delete_vertices(make_ego_graph(net_city_link_pat, order = 1, nodes = V(net_city_link_pat)$name[j])[[1]], v = V(net_city_link_pat)$name[j])
  
  V(net_city_link_pat)$propLA[j] <- if_else(vcount(g) == 0, 0, sum(V(g)$ass_is_latam)/vcount(g))
}

# Por últ. se agrega un atributo común para todas las ciudades, pero variable en el tiempo.
# Si queremos analizar los cambios en la antropología de la red afectan el desempeño de los nodos.
# En concreto vamos a calcular el grado de homofilia según la pertenencia de las ciudades a LatAm o no

V(net_city_link_pat)$assort_is_latam <- assortativity.nominal(net_city_link_pat, types = (V(net_city_link_pat)$ass_is_latam + 1), directed = F)

# De este proceso se obtiene una base de datos de corte transversal, ya que aún no incluimos los períodos, que tienen las sig. características

att_cities <- igraph::as_data_frame(net_city_link_pat, what = "vertices")

# Análisis para todas las ventanas
# En esta sección se repiten las etapas anteriores, pero se incluye un "for" que las repita para cada ventana
# IMPORTANTE: borrar los objetos y se vuelve a cargar la base

rm(list=ls())
load("cities_t3.RData")

# Creamos un vector con los años
years <- sort(unique(cities_t3$application_year))
# Un objeto con el tamaño de las ventanas
y_in_w <- 2
# y otro vector que indica la posición de los extremos de la ventana en el vector de años
ventanas <- seq(from = 1, to = length(years), by = y_in_w)

net_ass_list <- list()
net_cities_list <- list()
panel_cities <- list()

# A continuación se corre todo el código para todas las ventanas
# Donde lo único que cambia respecto de lo hecho en el apartado anterior es la forma en la que se seleccionan los años con los que nos quedamos de la base

for (i in ventanas) {
  
  ass_city_db <- cities_t3
  
  ass_city_db <- ass_city_db[which(ass_city_db$application_year >= years[i] & ass_city_db$application_year <= years[i+y_in_w-1]),] ## Vean la foram en que se indica la ventana
  
  periodos <- paste(sep = "_", years[i], years[i+y_in_w-1]) ## Solo para usar como nombre de los objetos de las listas
  
  ##
  
  componentes <- c("ass_city_id", "patent_id")
  
  temporal <- unique(subset.data.frame(ass_city_db, select = componentes))
  
  colnames(temporal) <- c("nodo", "link")
  
  edList <- temporal %>% 
    group_by(link) %>% 
    filter(n()>=2) %>% 
    group_by(link) %>% 
    do(data.frame(t(combn(.$nodo, 2)), stringsAsFactors=FALSE)) %>% 
    as.data.table
  
  edList <- setNames(subset.data.frame(edList, select = c("X1", "X2", "link")), c(paste(componentes[1], "out", sep = "_"), paste(componentes[1], "in", sep = "_"), componentes[2]))
  
  ##
  
  att_ass <- ass_city_db %>% 
    group_by(ass_city_id) %>% 
    summarise(country = last(country), 
              city_id = last(city_id), 
              lat = last(lat), 
              lon = last(lon), 
              ass_is_latam = last(ass_is_latam)) %>% 
    as.data.table
  
  net_ass_link_pat <- graph_from_data_frame(edList, directed = FALSE, vertices = att_ass) %>% simplify()
  
  ##
  
  att_cities <- ass_city_db %>% 
    group_by(city_id) %>% 
    summarise(n_ass = n_distinct(ass_city_id), 
              n_pat = n_distinct(patent_id), 
              n_inv = n_distinct(inventor_id)) %>% 
    as.data.table
  
  cities <- att_cities$city_id
  
  att_cities$meanDass <- 0
  
  for (j in 1:length(cities)) {
    att_cities[city_id == cities[j], "meanDass"] <- mean(igraph::degree(net_ass_link_pat, v = V(net_ass_link_pat)[city_id == cities[j]]))
  }
  
  ##
  
  componentes <- c("city_id", "patent_id")
  
  temporal <- unique(subset.data.frame(ass_city_db, select = componentes))
  
  colnames(temporal) <- c("nodo", "link")
  
  edList <- temporal %>% 
    group_by(link) %>% 
    filter(n()>=2) %>% 
    group_by(link) %>% 
    do(data.frame(t(combn(.$nodo, 2)), stringsAsFactors=FALSE)) %>% 
    as.data.table
  
  edList <- setNames(subset.data.frame(edList, select = c("X1", "X2", "link")), c(paste(componentes[1], "out", sep = "_"), paste(componentes[1], "in", sep = "_"), componentes[2]))
  
  ##
  
  att_ass <- unique(att_ass[,-1])
  
  att_cities <- merge(att_cities, att_ass, by = "city_id")
  
  net_city_link_pat <- graph_from_data_frame(edList, directed = FALSE, vertices = att_cities) %>% simplify()
  
  ##
  
  distancias <- as.data.frame(ends(net_city_link_pat, E(net_city_link_pat)))
  colnames(distancias) <- c("salida", "llegada")
  
  distancias$id_edge <- 1:length(distancias$salida)
  
  geo_data <- as.data.table(igraph::as_data_frame(net_city_link_pat, what = "vertices")[, c("name", "lon", "lat")])
  
  ##
  
  distancias <- merge(distancias, geo_data, by.x = "llegada", by.y = "name")
  distancias <- merge(distancias, geo_data, by.x = "salida", by.y = "name")
  
  distancias$distance <- round(distGeo(distancias[, c("lon.y", "lat.y")], distancias[, c("lon.x", "lat.x")])/1000, 2)
  
  distancias <- arrange(distancias, id_edge)
  
  E(net_city_link_pat)$distance <- distancias$distance
  
  for (j in 1:vcount(net_city_link_pat)) {
    
    V(net_city_link_pat)$meanDist[j] <- if_else(is.na(mean(incident(net_city_link_pat, v = V(net_city_link_pat)$name[j])$distance)) == TRUE, 0, mean(incident(net_city_link_pat, v = V(net_city_link_pat)$name[j])$distance))
    
    V(net_city_link_pat)$sdDist[j] <- if_else(is.na(sd(incident(net_city_link_pat, v = V(net_city_link_pat)$name[j])$distance)) == TRUE, 0, sd(incident(net_city_link_pat, v = V(net_city_link_pat)$name[j])$distance))
    
  }
  
  ##
  
  for (j in 1:vcount(net_city_link_pat)) {
    g <- delete_vertices(make_ego_graph(net_city_link_pat, order = 1, nodes = V(net_city_link_pat)$name[j])[[1]], v = V(net_city_link_pat)$name[j])
    
    V(net_city_link_pat)$propLA[j] <- if_else(vcount(g) == 0, 0, sum(V(g)$ass_is_latam)/vcount(g))
  }
  
  ##
  
  V(net_city_link_pat)$assort_is_latam <- assortativity.nominal(net_city_link_pat, types = (V(net_city_link_pat)$ass_is_latam + 1), directed = F)
  
  att_cities <- igraph::as_data_frame(net_city_link_pat, what = "vertices")
  
  ## Agrego columna con las ventanas
  
  att_cities$periodo <- periodos
  
  ## Guardo la base de cada ventana
  
  panel_cities[[periodos]] <- att_cities
  
  ## Guardo las redes para cada ventana
  
  net_ass_list[[periodos]] <- net_ass_link_pat
  net_cities_list[[periodos]] <- net_city_link_pat
  
}

# Ahora limpiamos los objetos que no son necesarios
rm(list=ls()[! ls() %in% c("net_ass_list", "net_cities_list", "panel_cities")])

# Finalmente unimos las bases de cada ventana para formar el panel
panel_cities <- rbindlist(panel_cities)

#Con esa base podemos pasar a trabajar con herramientas de análisis de otro tipo, pero en las variables estaremos considerando efectos que provienen de las interacciones de las ciudades

# BONUS TRACK
# Cálculo de dist. entre ciudades

# En este apartado les dejamos una forma de construir una matriz de distancias
# Esto es útil se les preocupa que exista correlación serial a nivel geográfico
# o sea que haya una dependencia geográfica que afecte las estimaciones de los modelos econométricos

load("cities_t3.RData")

ass_city_db <- cities_t3

componentes <- c("city_id", "patent_id")

temporal <- unique(subset.data.frame(ass_city_db, select = componentes))

colnames(temporal) <- c("nodo", "link")

edList <- temporal %>% 
  group_by(link) %>% 
  filter(n()>=2) %>% 
  group_by(link) %>% 
  do(data.frame(t(combn(.$nodo, 2)), stringsAsFactors=FALSE)) %>% 
  as.data.table

edList <- setNames(subset.data.frame(edList, select = c("X1", "X2", "link")), c(paste(componentes[1], "out", sep = "_"), paste(componentes[1], "in", sep = "_"), componentes[2]))

##

att_cities <- ass_city_db %>% 
  group_by(city_id) %>% 
  summarise(lat = last(lat), 
            lon = last(lon)) %>% 
  as.data.table

net_city_link_pat <- graph_from_data_frame(edList, directed = FALSE, vertices = att_cities) %>% simplify()

##

geo_data <- as.data.table(igraph::as_data_frame(net_city_link_pat, what = "vertices")[, c("name", "lon", "lat")])

# PROBLEMA: falta cargar alguna librería
W <- distm(geo_data_matrix, fun=distGeo)
W <- round(W/1000, 2)
dimnames(W) <- list(geo_data$name, geo_data$name)

rm(list=ls()[! ls() %in% c("W")])









