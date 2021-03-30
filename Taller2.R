#Instalacion de paquetes
install.packages("dplyr")
install.packages("backbone")

#Cargo paquetes
library(igraph)
library(dplyr)
library(backbone)

#Cargo base de datos y se revisa
load("taller2.RData")
summary(df_inv_pat_90_99)
latam

#Red general
#A continuación comenzamos por crear una red donde cada patente representa un vínculo entre dos países
#Por lo cual tendremos un grafo con múltiples links entre cada par de nodos

g_total <- graph_from_data_frame(df_inv_pat_90_99, directed = F)
summary(g_total)

#Ponderamos la red
E(g_total)$weight <- 1
g_total <- simplify(g_total, edge.attr.comb = list(weight = "sum", "ignore"))
summary(g_total)

#Como podemos apreciar, la cantidad de vínculos disminuye considerablemente, aunque ahora solo tenemos un atributo para los vínculos
#A continuación vamos a agregar un atributo que nos permita identificar si el país es de la región o no
#Luego de esto generamos una visualización que identifica, en verde, a estos países en la región y permite apreciar que la mayoría se encuentran en lo que podría ser el "core" de una red de tipo "Centro-Periferia"

V(g_total)$is_latam <- if_else(V(g_total)$name %in% latam, 1, 0)

plot(g_total,
     vertex.size = 5,
     vertex.frame.color = "black",
     vertex.label.color = "black",
     vertex.label.cex = NA,
     vertex.label.dist = 1,
     vertex.label = if_else((V(g_total)$is_latam) == 1, V(g_total)$name, ""),
     vertex.color = c("green","grey")[if_else((V(g_total)$is_latam) == 1, 1, 2)],
     edge.width = 0.8
)

#Backbone extraction

#1º hay que generar una lista de vinculos de una red bipartita
el <- rbind(as.matrix(df_inv_pat_90_99[, c(1,3)]), as.matrix(df_inv_pat_90_99[, c(2,3)]))
el <- unique(el)
dim(el)

#Luego utilizamos la función hyperg para calcular los p-valores que surgen de nuestros datos.
hyperg_B <- hyperg(el)
hyperg_B$summary
n_distinct(el[,1])
n_distinct(df_inv_pat_90_99$patent_id)

#Utilizando estas probabilidades podemos construir una lista de vínculos, de la red unimodal, que conserve solamente las interacciones estadísticamente significativas
hyperg_el_holm <- backbone.extract(hyperg_B, signed = F, alpha = .05, fwer = "holm", narrative = T)
hyperg_g_holm <- graph_from_data_frame(hyperg_el_holm[,1:2], directed = F) %>% simplify()

#El parámetro signed es útil si estamos analizando una red, por ejemplo, de amistades y enemistades
#Ya que permite crear vínculos positivos y negativos
#En nuestro caso esto no es necesario
#Además, la red debe ser simplificada (simplify) ya que por defecto backbone.extract nos da una matriz dirigida (que en este caso es simétrica)
#El parámetro narrative es el que genera la explicación que vemos en la consola.

#Ahora agregamos el atributo que identifica a los países de la región y realizamos una representación gráfica de la red.
V(hyperg_g_holm)$is_latam <- if_else(V(hyperg_g_holm)$name %in% latam, 1, 0)


plot(hyperg_g_holm,
     vertex.size = 5,
     vertex.frame.color = "black",
     vertex.label.color = "black",
     vertex.label.cex = NA,
     vertex.label.dist = 1,
     vertex.label = if_else((V(hyperg_g_holm)$is_latam) == 1, V(hyperg_g_holm)$name, ""),
     vertex.color = c("green","grey")[if_else((V(hyperg_g_holm)$is_latam) == 1, 1, 2)],
     edge.width = 0.8
)

#Evolución dinámica de la red general

#En esta sección trabajaremos con la red total, pero el procedimiento sería análogo si quisiéramos trabajar con la red obtenida mediante backbone.extract
#Para esto empezaremos por crear una lista que contenga a las redes por año

years <- sort(unique(df_inv_pat_90_99$application_year))

inv_pat <- list()

for (i in 1:length(years)) {
  this_year <- df_inv_pat_90_99[which(df_inv_pat_90_99$application_year == years[i]),]
  g <- graph_from_data_frame(this_year, directed = F)
  E(g)$weight <- 1
  g <- simplify(g, edge.attr.comb = list(weight = "sum", "ignore"))
  V(g)$is_latam <- if_else(V(g)$name %in% latam, 1, 0)
  inv_pat[[paste("y", years[i], sep = "_")]] <- g
}

#De esta manera podremos trabajar con la red de cada año de una manera sencilla
#Para verlo exploremos el objeto que hemos creado

inv_pat
summary(inv_pat)
inv_pat[[1]]
inv_pat[["y_1990"]]
inv_pat$y_1990
vcount(inv_pat[[1]])

#También tenemos la posibilidad de trabajar con cada objeto igrpah de forma recursiva.

for (i in 1:length(inv_pat)) {
  print(years[i])
  print(vcount(inv_pat[[i]]))
  print(ecount(inv_pat[[i]]))
}

#Esta última característica es especialmente útil para analizar la evolución de la estructura de la red, a lo largo del tiempo. Para hacerlo crearemos una matriz donde guardaremos cada indicador para cada año.
years <- as.character(years)
indicadores <- c("Countries", "Colaborations", "Mean_Degree", "SD_Degree", "Components", "Transitivity", "Assortativity", "Edge_Density")
BD_indicadores <- matrix(0, nrow = length(years), ncol = length(indicadores), dimnames = list(years, indicadores))

#El objeto BD_indicadores es una matriz cuyas filas tienen como nombre los años que estamos analizando
#Es importante convertir "character" al objeto years, ya que esto me permite usarlo para referenciar las filas
#Los nombres de las columnas vienen dados por el vector indicadores, en el cual podemos agregar las nuevas variables que queramos calcular, o sustituir las existentes
#El valor inicial de todos los campos de esta matriz es cero, los cuales son modificados en el siguiente código.

for (i in 1:length(years)) {
  BD_indicadores[years[i], "Countries"] <- vcount(inv_pat[[i]])
  BD_indicadores[years[i], "Colaborations"] <- ecount(inv_pat[[i]])
  BD_indicadores[years[i], "Mean_Degree"] <- mean(degree(inv_pat[[i]]))
  BD_indicadores[years[i], "SD_Degree"] <- sd(degree(inv_pat[[i]]))
  BD_indicadores[years[i], "Components"] <- count_components(inv_pat[[i]])
  BD_indicadores[years[i], "Transitivity"] <- transitivity(inv_pat[[i]], type = c("global"))
  BD_indicadores[years[i], "Assortativity"] <- assortativity(inv_pat[[i]], types1 = V(inv_pat[[i]])$is_latam, directed = F)
  BD_indicadores[years[i], "Edge_Density"] <- edge_density(inv_pat[[i]])
}

#La evolución de estos indicadores la podemos analizar visualmente en RStudio observando los valores, o de forma gráfica utilizando una estructura recursiva.

for (i in 1:length(indicadores)) {
  plot(years, BD_indicadores[,i], type = "l", ylab = colnames(BD_indicadores)[i])
}

#Ley de Potencia
resultados <- c("alpha", "p_v_KS")

PL_resultados <- matrix(0, nrow = length(years), ncol = length(resultados), dimnames = list(years, resultados))

for (i in 1:length(inv_pat)) {
  PL_resultados[years[i], "alpha"] <- fit_power_law(degree_distribution(inv_pat[[i]]), implementation = "plfit")$alpha
  PL_resultados[years[i], "p_v_KS"] <- fit_power_law(degree_distribution(inv_pat[[i]]), implementation = "plfit")$KS.p
}

plot(years, PL_resultados[,1], type = "l", ylab = colnames(PL_resultados)[1])

plot(years, PL_resultados[,2], type = "l", ylab = colnames(PL_resultados)[2])

#Nuestros resultados muestran evidencia a favor de la existencia de una "Ley de Potencia" en la distribución de grado de los nodos de nuestra red
#ya que la hipótesis nula es que los datos siguen una distribución de este tipo y los valores del p-valor de Kolmogorov-Smirnov no permiten rechazar dicha hipótesis