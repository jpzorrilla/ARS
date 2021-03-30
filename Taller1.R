install.packages("igraph")
library(igraph) #library for networks

rm(list=ls()) #Limpia todo el entorno (buenas practicas)

ej_1 <- make_graph(c(1, 2, 2, 3, 3, 4), directed=FALSE) #una red, no dirigida, con 4 nodos y tres vinculos

#see the object
ej_1
#or
summary(ej_1)

#visualize
plot(ej_1)

#Otra forma de crear la misma red, pero en este caso los numeros son tomados como nombres de los nodos.
ej_1_1 <- graph.formula(1-2, 2-3, 3-4)
plot(ej_1_1) #La disposicion de los nodos cambia cada vez que realizamos el plot

#Directed network
nodos <- c(1, 2, 2, 3, 3, 4)
ej_2 <- make_graph(nodos, directed=TRUE)
plot(ej_2)

#Esto facilita la modificacion del objeto, volviendo a usar la funcion concatenar.
ej_3 <- make_graph( c(nodos, 4,1), directed=TRUE)
plot(ej_3)

#Por ultimo, podemos usar el formato de formula para enunciar nodos y vinculos.
ej_4 <- make_graph( ~ A -- B, B -- C:D -- E)
ej_5 <- make_graph( ~ A --+ B, B +-- C, A --+ D:E, B --+ A )
plot(ej_4)
plot(ej_5)

#Algunas funciones para conocer caracteristicas de los objetos igraph
summary(ej_4)
ecount(ej_4) #number of edges
vcount(ej_4) #number of vertices
is.directed(ej_4)

summary(ej_5)
ecount(ej_5)
vcount(ej_5)
is.directed(ej_5)

#La red de estudiantes de ARS

estudiantes_incidencia <- read.csv("estudiantes_incidencia.csv", row.names=1, sep=";") #Cargo la base de datos
estudiantes_incidencia

#Las bases de datos de este tipo son conocidas como Matrices de Incidencia y permiten generar redes bipartitas (o de dos modos), mediante la funcion graph_from_incidence_matrix.

bipartita <- graph_from_incidence_matrix(estudiantes_incidencia)
plot(bipartita, 
     layout = layout_as_bipartite(bipartita)[,2:1],
     vertex.size = 8,
     vertex.label.dist = 5,
     vertex.color = c("green","blue")[V(bipartita)$type+1],
     vertex.label.degree = pi*V(bipartita)$type)

#A partir de una matriz de incidencia podemos obtener una proyeccion de un solo modo, a la cual llamamos Matriz de Adyacencia
estudiantes_incidencia <- as.matrix(estudiantes_incidencia) # Convierto la base en una matriz
estudiantes_adjacencia <- estudiantes_incidencia %*% t(estudiantes_incidencia) # Transformo la base de dos modos a un modo
estudiantes_adjacencia

#Usando la funcion graph_from_adjacency_matrix obtenemos la red de un modo que establece un vinculo entre c/u de los estudiantes que comparte, al menos, una materia
#En nuestro caso obtenemos un grafo conectado porque todos participan en ARS, por lo tanto todos comparten una materia.

un_modo <- graph_from_adjacency_matrix(estudiantes_adjacencia)
un_modo
plot(un_modo)

#ver explicacion tutorial
#verificacion
is.directed(un_modo) #Devuelve TRUE si el grafo es dirigido
length(which(which_loop(un_modo) == TRUE)) # Contamos los loops (porque hay mas loop que nodos?)
is_weighted(un_modo) #Devuelve TRUE si la red tiene ponderaciones
isSymmetric(estudiantes_adjacencia) #Devuelve TRUE si la matriz es simetrica
diag(estudiantes_adjacencia)

#Solucion
un_modo_simple <- graph_from_adjacency_matrix(estudiantes_adjacencia, mode = "undirected", diag = FALSE, weighted = TRUE)
un_modo_simple
#verificacion & graph
is.directed(un_modo_simple)
length(which(which_loop(un_modo_simple) == TRUE))
is_weighted(un_modo_simple)
plot(un_modo_simple)

#ver explicación tutorial
estudiantes_adjacencia_noARS <- estudiantes_adjacencia - 1 # La flexibilidad de R a la hora de operar con matrices permite hacer cosas como esta, por ello hay que tener mucho cuidado al aplicar algebra matricial en R
un_modo_simple_noARS <- graph_from_adjacency_matrix(estudiantes_adjacencia_noARS, mode = "undirected", diag = FALSE, weighted = TRUE)
un_modo_simple_noARS
plot(un_modo_simple_noARS)

##Borramos los objetos que tenemos en nuestro entorno de trabajo.
rm(list=ls())

#REDES DE COMERCIO

load("itn_tech_2010.RData")

dim(MA_itn_HT_2010)

#Podemos verificar si esta matriz puede dar lugar a una red dirigida, para lo cual no debería ser simétrica.
isSymmetric.matrix(MA_itn_HT_2010)

#Esta característica hace que podamos construir una red dirigida, mostrando el comercio binacional para cada par de países, o una red no dirigida al considerar el balance comercial entre cada par de países
#Sabemos que no deberían existir loops, ya que un país no debería tener comercio internacional consigo mismo.
#Esto lo podemos ver sumando los valores de la diagonal principal.

sum(diag(MA_itn_HT_2010))

#Las importaciones por país son:
rowSums(MA_itn_HT_2010)

#Las exportaciones por país:
colSums(MA_itn_HT_2010)

#Volumen de comercio mundial:
sum(rowSums(MA_itn_HT_2010))

latam_names

#A partir de esta matriz vamos a generar, en primer lugar, una red ponderada y dirigida para el comercio mundial
#Además se obtiene el número de nodos y de vínculos, así como la densidad de la red construyendo el cálculo o utilizando la función edge_density.

g_dir_global <- graph_from_adjacency_matrix(MA_itn_HT_2010, mode = "directed", weighted = T)

summary(g_dir_global)
ecount(g_dir_global)
vcount(g_dir_global)
ecount(g_dir_global)/(vcount(g_dir_global)*(vcount(g_dir_global)-1))
edge_density(g_dir_global)

#Ahora creamos la red de comercio, dirigida y ponderada, entre los países de Latinoamérica y analizamos las mismas propiedades que en el caso anterior.

g_dir_latam <- induced_subgraph(g_dir_global, vids = which(V(g_dir_global)$name %in% latam_names))

summary(g_dir_latam)
ecount(g_dir_latam)
vcount(g_dir_latam)
ecount(g_dir_latam)/(vcount(g_dir_latam)*(vcount(g_dir_latam)-1))
edge_density(g_dir_latam)

#Lo que hicimos fue obtener un sub grafo, mediante la función induced_subgraph, indicando que conserve únicamente los nodos cuyo nombre esté en el vector latam_names
#El comando which(V(g_dir_global)$name %in% latam_names) pregunta cuáles de los objetos del vector de nombres (V(g_dir_global)$name) cumplen la condición de estar en (%in%) el vector de nombre de países latinoamericanos (latam_names)
#Concretamente:
which(V(g_dir_global)$name %in% latam_names)

#Nuestra red de comercio latinoamericano se ve de esta forma:
plot(g_dir_latam, vertex.size = 10, edge.arrow.size = 0.5)

#Una alternativa podría ser que solo se consideren los vínculos que representen más de un determinado porcentaje de las importaciones o exportaciones de cada país
#En el siguiente ejemplo consideramos un porcentaje del 5%, para lo cual expresamos el grafo g_dir_latam como una matriz de adyacencia y luego dejamos en cero los intercambios que quedan por debajo del límite establecido.
MA_latam <- as.matrix(as_adjacency_matrix(g_dir_latam, type = "both", attr = "weight"))
MA_principales_latam <- MA_latam
minimo <- 0.05
for(i in 1:length(latam_names)) {
        export <- sum(MA_latam[, i]) # Estas son las exportaciones del pais i
        import <- sum(MA_latam[i, ]) # idem para import
        for(j in 1:length(latam_names)) {
                if(export*minimo >= MA_latam[j, i]) {
                        MA_principales_latam[j, i] <- 0
                }
                if(import*minimo >= MA_latam[i, j]) {
                        MA_principales_latam[i, j] <- 0
                }
        }
}

#Con la matriz MA_principales_latam podemos generar el nuevo grafo, que representa las principales relaciones comerciales, según el criterio establecido.

principales_latam_2010_ht <- graph_from_adjacency_matrix(MA_principales_latam, mode = "directed", weighted = T)
summary(principales_latam_2010_ht)
degree(principales_latam_2010_ht)

#Vemos que al hacer esto tenemos un nodo (Dominican Rep.) con grado cero, por lo cual lo eliminamos para analizar únicamente a los países que mantienen algún vínculo en base al criterio definido
#Luego se plantean dos visualizaciones distintas del grafo.
principales_latam_2010_ht <- delete_vertices(principales_latam_2010_ht, V(principales_latam_2010_ht)[name = "Dominican Rep."])
plot(principales_latam_2010_ht, layout = layout_in_circle(principales_latam_2010_ht))

plot(principales_latam_2010_ht, layout = layout_with_dh(principales_latam_2010_ht))

#El grafo resultante presenta las siguientes características:
is.simple(principales_latam_2010_ht) # Busca la presencia del loops y múltiples vínculos entre nodos.
degree(principales_latam_2010_ht, mode = "total") # Nos da un vector con el grado de cada nodo, sin considerar la direccion del vinculo.
degree(principales_latam_2010_ht, mode = "in") # Nos da un vector con el grado de cada nodo, considerando solo los vínculos de entrada.
degree(principales_latam_2010_ht, mode = "out") # Nos da un vector con el grado de cada nodo, considerando solo los vínculos de salida.
degree_distribution(principales_latam_2010_ht) # Nos da la frecuencia relativa de los grados que se encuentran en la red, yendo desde el cero al máximo grado encontrado.
is.connected(principales_latam_2010_ht) # Decimos que un grafo esta concetado si cualquier nodo se puede alcanzar desde cualquier otro.

#Si quisiéramos calcular indicadores para algún país en particular, como por ejemplo Uruguay, podríamos hacer lo siguiente:
degree(principales_latam_2010_ht, v = V(principales_latam_2010_ht)[name = "Uruguay"], mode = "out") #calcula el grado del nodo, de entrada o salida según lo que indiquemos en el parámetro mode
degree(principales_latam_2010_ht, v = V(principales_latam_2010_ht)[name = "Uruguay"], mode = "in")
alpha_centrality(principales_latam_2010_ht, nodes = V(principales_latam_2010_ht)[name = "Uruguay"]) #calcula un indicador de centralidad propuesto por Bonacich & Lloyd (2001), en el cual la relevancia de un nodo crece con la relevancia de los nodos que se vinculan a él
#Esta medida permite, a diferencia del eigen_centrality (Bonacich, 1987) controlar por características del nodo que no dependen de sus vínculos.

#Ahora creamos un grafo no dirigido, para poder utilizar el algoritmo fastgreedy.community de detección de comunidad
#Para ello existen distintas alternativas para definir lo que se hace con las ponderaciones de los vínculos
#En este caso se sumarán los vínculos para obtener una medida del comercio binacional entre los países.

undir_prin_latam_2010_ht <- as.undirected(principales_latam_2010_ht, mode = "collapse", edge.attr.comb = list(weight="sum"))
is.simple(undir_prin_latam_2010_ht) 
degree(undir_prin_latam_2010_ht)
degree_distribution(undir_prin_latam_2010_ht)
is.connected(undir_prin_latam_2010_ht)

#Ahora detectamos las comunidades mediante la función fastgreedy.community
#Esta aplica el algoritmo propuesto por Clauset, Newman, & Moore (2004), el cual busca la distribución de comunidades que optimiza la modularidad de la red, considerando el número de vínculos internos externos de cada sub grafo.

latam_itn_comm <- fastgreedy.community(undir_prin_latam_2010_ht)
length(latam_itn_comm)
sizes(latam_itn_comm)
membership(latam_itn_comm)
plot(latam_itn_comm, undir_prin_latam_2010_ht, layout = layout_with_lgl)

#Otra forma de visualizar el análisis de cluster que realiza este algoritmo es mediante este gráfico:
install.packages("ape")
dendPlot(latam_itn_comm, mode="phylo")