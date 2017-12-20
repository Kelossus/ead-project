#########################################################################
#
# 'k-means', 'pam' ('Partitioning Around Medoids') y 'clara' (CLustering
# LARge Applications) son algoritmos de clustering que definen particiones
# sobre un número predefinido 'k' de clases.
#
# #########################################################################
#
# Introducción de los datos: 'iris'
#
?iris
#
# Para una clasificación no supervisada no se toma en cuenta
# la variable 'Species' de la planta (5a. columna)
#
datos0 <- iris[,1:4]
#
# Características generales del conjunto de datos
#
dim(datos0); nrow(datos0); ncol(datos0)
#
head.matrix(datos0)
#
summary(datos0)
#
# Preprocesamiento
#
# ¿Estandarizar las variables?
# Son homogéneas (cm.); es razonable tratarlas tal cual.
# Sin embargo, algunas medidas son más grandes ('*.Length') que otras,
# y tratarlas tal cual equivaldría, en cierto sentido,
# a analizarlas por su apariencia visual.
#
# Estandarizar las variables equivale, en principio, a
# poner todas las variables a un mismo nivel en el análisis.
# Esta es la opción que se hace en el siguiente análisis
#
# Para estandarizar las variables (media 0, varianza 1)
#
datos <- scale(datos0)
#
# si no fueran estandarizadas:
#
# datos <- datos0
#
# Verificación:
#
for(j in 1:ncol(datos)) print(c(mean(datos[,j]), var(datos[,j])))
#
# En lenguaje R es más propio verificarlo así, sin bucles explícitos:
#
apply(datos,2,mean); apply(datos,2,var)
#
########################################################################
#
# Estudio de la función 'kmeans()':
#
# http://en.wikipedia.org/wiki/K-means
# http://es.wikipedia.org/wiki/K-means
# http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/
  #   K-Means
# Ejecutar el siguiente comando:
  #
  #   > example(kmeans)
#
?kmeans
#
#
# ########################################################################
#
# Primer ensayo con 'kmeans()'
#
# Fijación de las semillas iniciales para obtener los mismos resultados
# en distintas ejecuciones del siguiente código
#
set.seed(2011)
#
# 'k' arbitrario
#
k <- 5
#
km <- kmeans(datos, k)
#
km
#
# Características generales del objeto 'km'
#
class(km)
mode(km)
attributes(km)
#
# Tamaño de las 'k' clases resultantes
#
km$size
#
# Pertenencia de los objetos a las clases
#
km$cluster
#
# Composición de las clases
#
compocl <- vector("list", k)
for(kcl in 1:k) compocl[[kcl]] <- which(km$cluster == kcl)
compocl
#
# #########################################################################
#
# Índice de la bondad de la partición resultante: 'IB'
#
# 'km$tot.withinss': Suma de los cuadrados ('squared sum')
#
# de las diferencias de todos ('tot') los objetos
# respecto al centro de su respectiva clase
# Expresa la variabilidad dentro ('within') de las clases
#
#
# 'km$betweenss': Suma de los cuadrados ('squared sum')
# de las diferencias de los centros de cada clase
# respecto al centro del conjunto de datos
# Expresa la variabilidad entre ('between') las clases
#
# 'km$totss': Suma de los cuadrados ('squared sum')
# de las diferencias de todos ('tot') los objetos
# respecto al centro del conjunto de datos
# Expresa la variabilidad total del conjunto de datos
#
# Se cumple la siguiente igualdad:
  # km$totss == km$tot.withinss + km$betweenss
#
# 'IB' expresa la proporción de variabilidad debida a
# la formación de las clases
#
IB <- km$betweenss/km$totss # IB <- 1-km$tot.withinss/km$totss
IB
#
# 0 <= IB <= 1
#
# Si IB=0, entonces las clases formadas no tienen sentido
# pues los centros de clases coinciden
# (km$betweenss=0)
# Si IB=1, entonces las clases formadas son homogéneas,
# pues los elementos de la misma clase coinciden
#
(km$tot.withinss=0)
#
# Un valor de 'IB' cercano a 1
# indica que las clases son homogéneas y separadas
#
########################################################################
#
# Contraste de la adecuación de las clases:
#
# * Representación gráfica de las clases por pares de variables
#
pairs(datos0, col=km$cluster+1, las=1,
      main=paste("Iris data set\nk-means, k=", k, ", IB=", round(IB,4)),
      font.main=4)
#
# Se observan dos clases bien diferenciadas,
# una de las cuales es alargada
#
#
# * Imagen coloreada del conjunto de datos
# en función de las clases formadas y de las distancias
#
# Reordenamiento de los elementos en función de las clases resultantes
#
sortdatos <- sort(km$cluster, index.return=TRUE)$ix
datosort <- datos[sortdatos, ]
#
# Distancia euclidiana:
  #   http://es.wikipedia.org/wiki/Distancia_euclidiana
#
distancias <- dist(datosort, method="euclidean")
#
# Las distancias euclidianas cuadráticas 'distancias^2'
# están en consonancia con la noción de varianza generalizada:
  #
  #
  sum(distancias^2)/nrow(datosort)^2 == km$totss/nrow(datosort)
#
# Los valores de 'distancias^2' están implícitamente en
# la función objetivo de 'kmeans()':
  #
  #km$totss == km$tot.withinss + km$betweenss
#
#
IB <- km$betweenss/km$totss # IB <- 1-km$tot.withinss/km$totss
#
# Construcción de la imagen: 'image()'
#
# Propuesta: ejecutar los siguientes comandos
#
# > example(image)
# > ?image
#
# Cuanto más cerca están los objetos entre sí
# mayor es la intensidad del color rojo (mapa de calor)
#
image(as.matrix(distancias^2), col=heat.colors(12), axes=FALSE)
#
# Conviene completar la imagen para su mejor interpretación
#
# Marcas de los bordes por clases
#
atpoints <- c(0,cumsum(km$size))/max(cumsum(km$size))
for(side in 1:4) axis(side, at=atpoints, col.axis="transparent")
atpoints2 <- c(atpoints[-1],0)
atpoints2 <- (atpoints+atpoints2)/2
atpoints2 <- atpoints2[-length(atpoints2)]
#
etclases <- sort(unique(km$cluster))
for(side in 1:4)
  for(etcl in 1:length(etclases))
    axis(side,
         at=atpoints2[etcl], labels=etclases[etcl], col.axis=etclases[etcl],
         las=1, tick=FALSE, cex.axis=0.7)
#
# Tamaño de las clases
#
text(atpoints2, atpoints2, km$size, col="white")
#
#########################################################################
#
# Se observan dos clases, en una de las cuales
# se podrían considerar algunas subclases
#
##########################################################################
# 
# Contraste de la adecuación de las clases
#
selec <- c(1,2) # cualquier par de variables
#
plot(datos0[,selec], col=km$cluster, las=1,
     main=paste("Iris data set\nk-means, k=", k, ", IB=", round(IB,4)),
     font.main=4, xlab=names(datos0)[selec[1]], ylab=names(datos0)[selec[2]])
#
# Ubicación de los centros de clase
#
kmcenters <- matrix(rep(0, k*ncol(datos0)), ncol=ncol(datos0))
for(j in 1:ncol(datos0))
  kmcenters[,j] <- tapply(datos0[,j], km$cluster, mean)
rownames(kmcenters) <- 1:k; colnames(kmcenters) <- colnames(kmcenters)
#
points(kmcenters[etclases,selec],
       col=etclases, pch=19, cex=2)
#
legend("topleft", bty="n",
       x.intersp=1, y.intersp=1,
       legend=km$size[etclases], text.col=etclases,
       pch=19, col=etclases, ncol=1, cex=1)
#
##########################################################################



#########################################################################
#
# Verificación de los resultados de 'km()'
#
# #########################################################################
#
# Recuperación del ejemplo tratado anteriormente
#
# Introducción de los datos: 'iris'
#
datos0 <- iris[,1:4]
#
# Estandarización de las variables (media 0, varianza 1)
#
datos <- scale(datos0)
#
# Fijación de las semillas iniciales para obtener los mismos resultados
# en distintas ejecuciones del siguiente código
#
set.seed(2011)
#
# 'k' arbitrario
#
k <- 5
km <- kmeans(datos, k)
#
# #########################################################################
#
attributes(km)
#
# Verificación de los cálculos:
#
km$size
#
km$centers
#
km$withinss
#
km$tot.withinss
#
km$betweenss
#
km$totss
#
# ##########
#
# km$size
#
km$size
sum(km$size) == nrow(datos)
#
# ##########
#
# km$centers
#
# Cálculo de las medias de las variables por cada clase
#
medias.cl <- matrix(rep(0, k*ncol(datos)), ncol=ncol(datos))
for(j in 1:ncol(datos))
  medias.cl[,j] <- tapply(datos[,j], km$cluster, mean)
#
rownames(medias.cl) <- 1:k; colnames(medias.cl) <- colnames(datos)
medias.cl
km$centers
medias.cl == km$centers # FALSE!!!
#
# Las medias (ponderadas) de las medias parciales de cada clase
# coinciden con las medias totales (0, si las variables son estandarizadas)
#
medias.pond <- vector()
for(j in 1:ncol(datos))
  medias.pond[j] <- sum(km$size*km$centers[,j])/nrow(datos)
names(medias.pond) <- colnames(datos)
medias.pond
#
medias.tot <- apply(datos, 2, mean) # colMeans(datos)
medias.tot
medias.pond == medias.tot # FALSE!!!
#
# ##########
#
# km$withinss
#
# Cálculo de las varianzas de las variables
# dentro de cada clase
#
varcl <- matrix(rep(0, k*ncol(datos)), ncol=ncol(datos))
for(j in 1:ncol(datos))
  varcl[,j] <- tapply(datos[,j], km$cluster, var)
rownames(varcl) <- 1:k; colnames(varcl) <- colnames(datos)
varcl
#
# Cálculo de las sumas de las diferencias cuadráticas
# de todas las variables dentro de cada clase
#
SSWcl <- vector()
for(kcl in 1:k)
  SSWcl[kcl] <- sum((km$size[kcl]-1)*varcl[kcl,])
SSWcl
km$withinss
km$withinss == SSWcl # FALSE!!!
#
# ##########
#
# km$tot.withinss
#
# Cálculo de las sumas de las diferencias cuadráticas
# de todas las variables dentro de todas las clases
#
SSW <- sum(SSWcl)
SSW
km$tot.withinss
km$tot.withinss == SSW # FALSE!!!
km$tot.withinss == sum(km$withinss)
###########
#
# km$betweenss
#
# Cálculo de varianzas de las variables
# siendo cada clase representada por su centro (media)
#
varB.pond <- vector() # varB.pond[j], varianza ponderada de cada variable
for(j in 1:ncol(datos))
  varB.pond[j] <- sum(km$size*(km$centers[,j]-medias.tot[j])^2)/nrow(datos)
names(varB.pond) <- colnames(datos)
varB.pond
#
# Cálculo de sumas de las diferencias cuadráticas de las variables
# siendo cada clase representada por su centro (media)
#
SSBj <- nrow(datos)*varB.pond
SSBj
#
# Cálculo de las sumas de las diferencias cuadráticas
# de todas las variables y de todas las clases,
# siendo cada clase representada por su centro (media)
#
SSB <- sum(SSBj)
SSB
km$betweenss
km$betweenss == SSB # FALSE !!!
#
# ##########
#
# km$totss
#
# Cálculo de las sumas de las diferencias cuadráticas
# de todas las variables
#
SST <- 0
for(j in 1:ncol(datos))
  SST <- SST + sum((nrow(datos)-1)*var(datos[,j]))
SST
#
SST == SSW + SSB
#
km$totss
km$totss == SST
km$totss == km$tot.withinss + km$betweenss
#
# #########################################################################


#########################################################################
#
# Interpretación de una clasificación
#
# #########################################################################
#
# Recuperación del ejemplo tratado anteriormente
#
# Introducción de los datos: 'iris'
#
datos0 <- iris[,1:4]
#
# Cálculo de los estadísticos principales por variables
#
medias.tot <- apply(datos0, 2, mean); medias.tot
var.tot <- apply(datos0, 2, var);
var.tot
desv.st.tot <- apply(datos0, 2, sd); desv.st.tot
#
# Estandarización de las variables (media 0, varianza 1)
#
datos <- scale(datos0)
#
# Fijar las semillas iniciales para obtener los mismos resultados
#
set.seed(2011)
#
# 'k' arbitrario
#
k <- 5
km <- kmeans(datos, k)
attributes(km)
#
# #########################################################################
#
# Interpretación de la clasificación:
# - Índice de bondad
# - Análisis de estadísticos
# - Representación gráfica
#
#
# Índice de la bondad de las clases formadas: 'IB'
#
IB <- km$betweenss/km$totss # IB <- 1-km$tot.withinss/km$totss
IB
#
# 0 <= IB <= 1
# Si IB=0, entonces las clases formadas no tienen sentido
# pues los centros de clases coinciden
# (km$betweenss=0)
# Si IB=1, entonces las clases formadas son homogéneas,
# pues los elementos de la misma clase coinciden
# (km$tot.withinss=0)
#
#
# Análisis de estadísticos de cada variable por clases
#
# Media de cada variable por clases
km$centers
# Se observa que los centros de las clases:
  #   '1' y '2' se separan de los de '3', '4' y '5'
# '1'<'2' y '5'<'3'<'4'
#
# Varianza de cada variable por clases
#
varcl <- matrix(rep(0, k*ncol(datos)), ncol=ncol(datos))
for(j in 1:ncol(datos))
  varcl[,j] <- tapply(datos[,j], km$cluster, var)
rownames(varcl) <- 1:k; colnames(varcl) <- colnames(datos)
#
varcl
# Se observa que las variables
# 'Petal.Length' y 'Petal.Width'
# son las de menor varianza en todas las clases
# por tanto, las más características de cada clase
#
# Varianza de cada variable entre clases
#
medias.pond <- vector()
for(j in 1:ncol(datos))
  medias.pond[j] <- sum(km$size*km$centers[,j])/sum(km$size)
names(medias.pond) <- colnames(datos)
medias.pond # si ha habido estandarización, todas nulas
#
varB.pond <- vector() # varB.pond[j], varianza ponderada por clases
for(j in 1:ncol(datos))
  varB.pond[j] <-
  sum(km$size*(km$centers[,j]-medias.pond[j])^2)/sum(km$size)
names(varB.pond) <- colnames(datos)
#
varB.pond
#
sum(varB.pond)*nrow(datos); km$betweenss # Verificación de la descomposición de B
#
# Se observa que las variables
# 'Petal.Length' y 'Petal.Width' son las de mayor
# varianza, por tanto, las más características de
# la constitución de las clases
#
#
# Representación gráfica de las clases:
#
# Por cada par de variables
#
pairs(datos0, col=km$cluster, las=1,
      main=paste("Iris data set\nk-means, k=", k, ", IB=", round(IB,4)),
      font.main=4)
#
# Por un par de variables características
#
elecx <- 3; elecy <- 4
# 'Petal.Length' y 'Petal.Width'
plot(datos0[,c(elecx,elecy)], col=km$cluster, las=1,
    main=paste("Iris data set\nk-means, k=", k, ", IB=", round(IB,4)),
font.main=4, xlab=names(datos0)[elecx], ylab=names(datos0)[elecy])
#
# Ubicación de los centros de clase
#
kmcenters <- matrix(rep(0, k*ncol(datos0)), ncol=ncol(datos0))
for(j in 1:ncol(datos0))
kmcenters[,j] <- tapply(datos0[,j], km$cluster, mean)
rownames(kmcenters) <- 1:k; colnames(kmcenters) <- colnames(kmcenters)
#
kmcenters
#
etclases <- unique(km$cluster) # etiqueta de clases
#
points(kmcenters[etclases,c(elecx,elecy)],
col=etclases, pch=19, cex=2)
#
legend("bottomright", bty="n",
x.intersp=1, y.intersp=1,
legend=km$size[etclases], text.col=etclases,
pch=19, col=etclases, ncol=1, cex=1)
#
# #########################################################################

#########################################################################
#
# El resultado depende de las semillas iniciales
#
# #########################################################################
#
# Recuperación del ejemplo tratado anteriormente
#
# Introducción de los datos: 'iris'
#
datos0 <- iris[,1:4]
#
# Estandarización de las variables (media 0, varianza 1)
#
datos <- scale(datos0)
#
# Fijar las semillas iniciales para obtener los mismos resultados
#
set.seed(2011)
#
# 'k' arbitrario
#
k <- 5
km <- kmeans(datos, k)
attributes(km)
#
km$size
#
# Índice de la bondad de las clases formadas: 'IB'
#
IB <- km$betweenss/km$totss # IB <- 1-km$tot.withinss/km$totss
IB
#
# #########################################################################
#
# El resultado depende de las semillas iniciales
#
# Si no se fija la semilla 'set.seed(xxxx)'
# los resultados son diversos
#
k <- 5
tabla <- NULL
for(i in 1:20) {
  km <- kmeans(datos, k)
  tabla <- rbind(tabla,
                 c(km$size, round(km$betweenss/km$totss, digits=6)))
}
nombrecl <- vector()
for(kcl in 1:k) nombrecl[kcl] <- paste("cl", kcl, sep="")
# nombrecl
colnames(tabla) <- c(nombrecl, "IB")
#
tabla
#
# Se observa que los resultados son diversos
#
# Para evitar esta diversidad y aproximarse a un máximo
# se ejecutar el algoritmo 'kmeans()' 'nstart' veces.
# 'kmeans()' selecciona automáticamente el mejor resultado
#
# Dependiendo del tamaño de 'datos'
# 'nstart' no asegura la consecución de un máximo total.
#
# También hay que asegurar que el algoritmo se detenga,
# pues la convergencia a un máximo local no está asegurada
# en un predeterminado número de iteraciones:
# Fijar 'iter.max' pasos
#
k <- 5
tabla <- NULL
for(i in 1:20) {
  km <- kmeans(datos, k, nstart=100, iter.max=1000)
  tabla <- rbind(tabla,
                 c(km$size, round(km$betweenss/km$totss, digits=6)))
}
nombrecl <- vector()
for(kcl in 1:k) nombrecl[kcl] <- paste("cl", kcl, sep="")
colnames(tabla) <- c(nombrecl, "IB")
#
tabla
#
# Se observa que los resultados son los mismos,
# salvo la etiquetación u orden de las clases
#  (se podría comprobar que
  #   no sólo los tamaños de las clases coinciden,
  #   sino también la composición de las mismas)
#
# #########################################################################

#########################################################################
#
# Siluetas
#
# #########################################################################
#
# Recuperación del ejemplo tratado anteriormente
#
# Introducción de los datos: 'iris'
#
datos0 <- iris[,1:4]
#
# Estandarización de las variables (media 0, varianza 1)
#
datos <- scale(datos0)
#
# Construcción de una partición
#
k <- 3
km <- kmeans(datos, k, nstart=100, iter.max=10000)
#
# Composición de las clases
#
compocl <- vector("list", k)
for(kcl in 1:k) compocl[[kcl]] <- which(km$cluster == kcl)
compocl
#
# #########################################################################
#
# Construcción de siluetas mediante la función 's(i)'
# asignado a cada objeto 'i'.
#
# http://en.wikipedia.org/wiki/Silhouette_%28clustering%29
#
# s(i) = (b(i)-a(i))/max(b(i),a(i))
#
# a(i) = disimilitud media del objeto 'i' con los de su clase
# b(i) = mínima disimilitud media del objeto 'i' con los de otras clases
#
# Hay que definir una distancia (disimilitud) entre los objetos 'i'
#
# La distancia debe estar en consonancia
# con la función objetivo de 'kmeans()'
#
distancias <- dist(datos, method="euclidean", upper=TRUE, diag=TRUE)^2
distancias.m <- as.matrix(distancias)
#
# Cálculo de los valores 'a(i)', 'b(i)', y 's(i)'
#
dmean <- matrix(rep(NA, nrow(datos)*k), ncol=k)
a <- rep(NA, nrow(datos))
b <- rep(NA, nrow(datos))
s <- rep(NA, nrow(datos))
#
for(i in 1:nrow(datos)){
  for(kcl in 1:k) dmean[i,kcl] <- mean(distancias.m[i, compocl[[kcl]]])
  kcli <- km$cluster[i]
  a[i] <- dmean[i,kcli]*km$size[kcli]/(km$size[kcli]-1)
  b[i] <- min(dmean[i,-kcli])
  s[i] <- (b[i]-a[i])/max(b[i],a[i])
}
#
# Cálculo de los valores 's(i)' ('width'),
# mediante la función 'silhouette()'
#
library(cluster)
#
silk <- silhouette(km$cluster, distancias)
#
silk
#
# Características generales del objeto 'silk'
#
class(silk)
mode(silk)
attributes(silk)
#
cluster <- silk[,1] # clase del objeto 'i'
table(cluster)
#
neighbor <- silk[,2] # clase vecina o más próxima al objeto 'i'
table(neighbor)
#
width <- silk[,3] # valor del índice de silueta 's(i)'
summary(width)
hist(width)
#
summary(silk)
attributes(summary(silk))
#
summary(silk)$clus.avg.widths
mediascl <- tapply(width, cluster, mean)
#
summary(silk)$avg.width
mean(s)
#
# Verificación de que
# los valores 'sil_width' (3a. columna) son los 's(i)'
#
cbind(s, silk[,3])
s == width # FALSE !!!
#
# El valor medio del índice 's(i)' ('avg.width'),
# se usa para evaluar la bondad de la partición
#
# Una evaluación más detallada de la bondad de la partición
# se puede hacer mediante las siluetas de las clases
#
# El análisis de los valores medios por clase
# y la negatividad de algunos valores
# se puede usar para determinar el número apropiado de clases 'k'
#
etclases <- sort(unique(km$cluster))
plot(silk, col=etclases)
#
#
# Las siluetas sólo dependen de la partición,
# no dependen del algoritmo para obtenerlas
#
# Así, las siluetas valen para comparar los resultados de diferentes clustering
# aplicados a los mismo conjunto de datos
#
# Las siluetas valen para interpretar y validar los resultados
# 'k' pequeño, clases naturales mezcladas:
# Si 'SSW' grande, 'a(i)' grande, s(i) pequeños.
# 'k' grande, clases naturales separadas:
# Si 'SSB' pequeño, 'b(i)' pequeño, s(i) pequeños.
#
# Valor medio global puede valer para seleccionar el valor de 'k'
#
# Las siluetas pueden servir para mejorar los resultados del análisis,
# por ejemplo, moviendo un objeto con valor de s(i)<0 a su clase vecina
#
mean(silk[,3])
# [1] 0.6507398
#
which(silk[,3] < 0)
# [1] 112 128
silk[which(silk[,3] < 0), 2]
# [1] 3 3 las calses vecinas
km$cluster.new <- km$cluster
km$cluster.new[which(silk[,3] < 0)] <- silk[which(silk[,3] < 0), 2]
#
silk <- silhouette(km$cluster.new, distancias)
etclases <- sort(unique(km$cluster))
plot(silk, col=etclases+1)
mean(silk[,3])
# [1] 0.6529184 # se ha mejorado el valor del indice
#
which(silk[,3] < 0)
# [1] 109
silk[which(silk[,3] < 0), 2]
km$cluster.new2 <- km$cluster.new
km$cluster.new2[which(silk[,3] < 0)] <- silk[which(silk[,3] < 0), 2]
kmsize.new2 <- as.vector(table(km$cluster.new2))
#
silk <- silhouette(km$cluster.new2, distancias)
etclases <- sort(unique(km$cluster))
plot(silk, col=etclases+1)
mean(silk[,3])
# [1] 0.6544969 # se ha mejorado el valor del indice
#
#########################################################################





#########################################################################
#
# Selección del valor de 'k' (el número de clases)
#
# #########################################################################
#
# Recuperación del ejemplo tratado anteriormente
#
# Introducción de los datos: 'iris'
#
datos0 <- iris[,1:4]
#
# Estandarización de las variables (media 0, varianza 1)
#
datos <- scale(datos0)
#
# 'k' arbitrario
#
k <- 5
km <- kmeans(datos, k, nstart=100, iter.max=10000)
#
attributes(km)
#
#########################################################################
#
# Selección del valor de 'k':
# Índice de bondad, 'IB'
# Índice Davies-Bouldin
# Valor medio de índice de siluetas
#
# ##########
#
# Índice de la bondad de la partición: 'IB'
#
IB <- km$betweenss/km$totss # IB <- 1-km$tot.withinss/km$totss
IB
#
# Cálculo del valor de 'IB' para cada valor de 'k'
# buscando el máximo para cada uno de ellos (con 'nstart'=100)
#
IB <- vector()
for(k in 1:20)
{
  km <- kmeans(datos, k, nstart=100, iter.max=10000)
  IB[k] <- km$betweenss/km$totss
}
#
cbind(k=1:length(IB), IB=round(IB, digits=6))
#
# Representación gráfica de la variación de 'IB' en función de 'k'
#
plot(IB, type="b", las=1, xlab="k")
#
# Selección del valor de 'k'
# tras el análisis de los valores de 'IB'
#
# El valor de 'IB' crece, lógicamente, con 'k',
# pues a mayor 'k' mayor 'km$betweenss'
#
# Así, analizar los valores de 'IB'
# consiste en analizar su crecimiento respecto a 'k':
# Si hay un salto grande entre dos consecutivos,
# entonces es señal de que hay dos clases ciertamente separadas
#
selectk <- NULL
for(k in 3:length(IB))
  selectk <- rbind(selectk, c(k, IB[k]-IB[k-1], IB[k]/IB[k-1]))
colnames(selectk) <- c("k", "IB[k]-IB[k-1]", "IB[k]/IB[k-1]")
selectk
#
# Observados los saltos ("IB[k]-IB[k-1]", "IB[k]/IB[k-1]"),
# k=2 parece una buena elección,
# y k=3 podría serla también
#
# ##########
#
# Índice de Davies-Bouldin
# http://en.wikipedia.org/wiki/Davies-Bouldin_index
#
# La distancia debe estar en consonancia
# con la función objetivo de 'kmeans()'
#
distancias <- dist(datos, method="euclidean")^2 # distancia cuadrática
#
# Adecuación del objeto 'distancias' a un tipo de objeto manejable para lo que sigue
#
distancias.m <- as.matrix(distancias, ncol=nrow(datos)) # formato necesario
#
k <- 3
km <- kmeans(datos, k, nstart=100, iter.max=10000)
#
# Composición de las clases
#
compocl <- vector("list", k)
for(kcl in 1:k) compocl[[kcl]] <- which(km$cluster == kcl)
#
# Definición del índice 'DB' mediante una función
#
DB <- function(distancias.m, compocl, k){
  #
  #   Distancias medias 'dentro de' y 'entre' las clases: 'Mcl'
  #   'dentro de': diagonal de 'Mcl'
  #   'entre'  : fuera de la diagonal de 'Mcl'
  Mcl <- matrix(rep(0,k*k), ncol=k) # inicialización
  for(kcli in 1:k){
    for(kclj in 1:k){
      Mcl[kcli,kclj] <- mean(distancias.m[compocl[[kcli]], compocl[[kclj]]])
    }
  }
  # Cálculo de ratios
  #
  R <- matrix(rep(0,k*k), ncol=k)
  for(kcli in 1:k){
    for(kclj in 1:k){
      R[kcli,kclj] <- (Mcl[kcli,kcli]+Mcl[kclj,kclj])/Mcl[kcli,kclj]
    }
  }
  #
  # Cuanto menor sea R, mayor será la separación entre clases
  #
  # Se adopta el peor de los casos para definir el índice
  #
  D <- rep(0,k)
  for(kcl in 1:k){
    D[kcl] <- max(R[kcl,-kcl])
  }
  #
  # Cálculo del valor del índice de Davies-Bouldin
  #
  DB <- mean(D)
  #
  # Cuanto menor es el índice, mejor es la partición
  #
  return(DB)
  #
}
#
# Cálculo del valor de 'DB' para cada valor de 'k'
# buscando el mínimo
#
db <- NA # db[1] no tiene sentido
for(k in 2:20)
{
  km <- kmeans(datos, k, nstart=100, iter.max=10000)
  compocl <- vector("list", k)
  for(kcl in 1:k) compocl[[kcl]] <- which(km$cluster == kcl)
  db[k] <- DB(distancias.m, compocl,k)
}
#
cbind(k=1:length(db), DB=round(db, digits=6))
#
# Representación gráfica de la variación de 'db' en función de 'k'
#
plot(db, type="b", las=1, xlab="k")
#
# Observados los saltos, k=2 parece una buena elección
#
# ##########
#
# Valor medio del índice de siluetas 's(i)'
#
k <- 3
km <- kmeans(datos, k, nstart=100, iter.max=10000)
#
# La distancia debe estar en consonancia
# con la función objetivo de 'kmeans()'
#
distancias <- dist(datos, method="euclidean")^2
#
# Cálculo del valor del índice 's(i)' ('widths'),
# asignado a cada objeto 'i',
# mediante la función 'silhouette()'
# http://en.wikipedia.org/wiki/Silhouette_%28clustering%29
#
library(cluster)
#
silk <- silhouette(km$cluster, distancias)
#
class(silk)
mode(silk)
attributes(silk)
#
summary(silk)
attributes(summary(silk))
#
# El valor medio del índice 's(i)' ('avg.width'),
# es también un índice de la bondad de la partición
#
summary(silk)$avg.width
#
# Se analiza este valor para distintos valores de 'k'
#
avg.width <- NA # avg.width[1] no tiene sentido
for(k in 2:20)
{
  km <- kmeans(datos, k, nstart=100, iter.max=10000)
  silk <- silhouette(km$cluster, distancias)
  avg.width[k] <- summary(silk)$avg.width
}
#
cbind(k=1:length(avg.width), avg.width=round(avg.width, digits=6))
#
#
# Representación gráfica de la variación de 'avg.width' en función de 'k'
#
plot(avg.width, type="b", las=1, xlab="k")
#
selectk <- NULL
for(k in 3:length(IB))
  selectk <- rbind(selectk, c(k, avg.width[k]-avg.width[k-1],
                              avg.width[k]/avg.width[k-1]))
colnames(selectk) <- c("k", "avg.width[k]-avg.width[k-1]",
                       "avg.width[k]/avg.width[k-1]")
selectk
#
# Observados los saltos "avg.width[k]-avg.width[k-1]",
# "avg.width[k]/avg.width[k-1]",
# k=2 parece una buena elección,
#
#########################################################################

#########################################################################
#
# Introducción de los datos: 'iris'
#
datos0 <- iris[,1:4]
#
# Estandarización de las variables (media 0, varianza 1)
#
datos <- scale(datos0)
#
#########################################################################
#
# Selección del valor del parámetro 'k': k=2
#
k <- 2
km <- kmeans(datos, k, nstart=100, iter.max=10000)
#
# La distancia debe estar en consonancia
# con la función objetivo de 'kmeans()'
#
distancias <- dist(datos, method="euclidean")^2
#
# Siluetas
#
library(cluster)
#
silk <- silhouette(km$cluster, distancias)
#
etclases <- sort(unique(km$cluster))
plot(silk, col=etclases+1)
#
# Cálculo de medias por clase
#
medias.cl <- matrix(rep(0, k*ncol(datos)), ncol=ncol(datos))
for(j in 1:ncol(datos))
  medias.cl[,j] <- tapply(datos[,j], km$cluster, mean)
#
# Cálculo de medias total. Si variables son estandarizadas, 0
#
medias.tot <- apply(datos, 2, mean) # colMeans(datos)
medias.tot
#
# Cálculo de varianzas entre clases por variable
#
varB.pond <- vector() # varB.pond[j], varianza ponderada de cada variable
for(j in 1:ncol(datos))
  varB.pond[j] <- sum(km$size*(medias.cl[,j]-medias.tot[j])^2)/nrow(datos)
names(varB.pond) <- colnames(datos)
varB.pond
#
# Sepal.Length Sepal.Width Petal.Length Petal.Width
#
0.5112540
0.3616017
0.8458193
0.7821296
#
# Las dos variables del Sépalo son las que más explican
# la consideración de las dos clases o conglomerados
#
# Cálculo de sumas de errores cuadráticos entre clases
#
SSBj <- nrow(datos)*varB.pond; SSBj
#
SSB <- sum(SSBj) # km$betweenss
IB <- km$betweenss/km$totss
IB
#
# La consideración de estas dos clases o conglomerados (clusters)
# explica casi el 63% de la varianza inicial (bastante alto)
# y se debe, principalmente, a los variables del sépalo
#
pairs(datos, pch=21, col="black", bg=km$cluster+1)
#
#########################################################################