#########################################################################
#
# 'hclust', 'agnes' ('AGglomerative NESting') y 'diana' (DIvisive ANAlysis)
#  son algoritmos de clustering que definen jerarquías
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
nombredatos <- "Iris data set"
#
##########################################################################
#
# ¿Estandarizar las variables?
#    Son homogéneas (cm.); es razonable tratarlas tal cual.
#    Sin embargo, algunas medidas son más grandes ('*.Length') que otras,
#    y tratarlas tal cual equivaldría, en cierto sentido,
#    a analizarlas por su apariencia visual.
#
#    Estandarizar las variables equivale, en principio, a
#    poner todas las variables a un mismo nivel en el análisis.
#    Esta es la opción que se hace en el siguiente análisis
#
# Para estandarizar las variables (media 0, varianza 1)
#
datos <- scale(datos0, center=TRUE, scale=TRUE)
#
##########################################################################
#
# Estudio de la función 'hclust()':
#
#   http://en.wikipedia.org/wiki/Hierarchical_clustering
#   http://es.wikipedia.org/wiki/Agrupamiento_jer%C3%A1rquico
#   http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/
#                                           Clustering/Hierarchical_Clustering
#   https://sites.google.com/site/dataclusteringalgorithms/
#                                           hierarchical-clustering-algorithm
#   http://en.wikipedia.org/wiki/Dendrogram
#
# El algoritmo se basa en una matriz de distancias o desemejanzas
#
distancias <- dist(datos, method="euclidean", upper=TRUE, diag=TRUE)
#
complete <- hclust(distancias)
#
# El método 'por defecto' es 'complete'
#
# Trazado del dendrograma correspondiente
#
plot(complete, hang=-1, main=nombredatos, sub="", las=1,
     xlab="Distance='Euclidean'\nMethod='Complete'")
#
#######
#
single <- hclust(distancias, method="single")
#
plot(single, hang=-1, main=nombredatos, sub="", las=1,
     xlab="Distance='Euclidean'\nMethod='Single'")
#
#
#######
#
average <- hclust(distancias, method="average")
#
plot(average, hang=-1, main=nombredatos, sub="", las=1,
     xlab="Distance='Euclidean'\nMethod='Average'")
#
#######
#
ward <- hclust(distancias, method="ward.D")
#
plot(ward, hang=-1, main=nombredatos, sub="", las=1,
     xlab="Distance='Euclidean'\nMethod='Ward'")
#
#######
#
ward2 <- hclust(distancias, method="ward.D2")
#
plot(ward2, hang=-1, main=nombredatos, sub="", las=1,
     xlab="Distance='Euclidean^2'\nMethod='Ward'")
#
#######
#
ward22 <- hclust(distancias^2, method="ward.D")
#
plot(ward22, hang=-1, main=nombredatos, sub="", las=1,
     xlab="Distance='Euclidean^2'\nMethod='Ward'")
#
#
#######
#
# Las dos últimas no dan la misma jerarquía indexada.
# La jerarquia en sí es la misma, pero las ramas (distancias), no.
# La relacion entre los valores de los indices
# correspondientes a ambas jerarquias es cuadratica.
#
#
cbind(ward2$merge, ward22$merge)
#
cbind(ward2$height, ward22$height)
plot(ward2$height, ward22$height)
cbind(ward2$height^2, ward22$height)
plot(ward2$height^2, ward22$height)
#
sum(ward22$height)
#[1] 1192  # = (4*(150-1))*2
#
2*sum((nrow(datos)-1)*diag(var(datos))) # variables estandarizadas
#[1] 1192
#
##########################################################################





###################################################################
#
# Para seleccionar una partición:
# - se analizan los árboles correspondientes
# - se determinan las podas convenientes
# - se analizan las bondades de las particiones correspondientes
# - y se elige una partición
#
# Se da por supuesto que se ha ejecutado el capitulo 'Inicio'
#
# La función 'cutree()' es la que efectua la poda
#
? cutree
example(cutree)
#
###################################################################
#
# Trazado del dendrograma correspondiente
#
plot(complete, hang=-1, main=nombredatos, sub="", las=1,
     xlab="Distance='Euclidean'\nMethod='Complete'")
#
# Se observa un posible corte de 3 o 5 clases
#
(cutcomplete <- cutree(complete, k=c(3,5)))
#
# Validación interna
#
library(cluster)
plot(silhouette(cutcomplete[,1], distancias),
     col=sort(unique(cutcomplete[,1]))+1, main="Complete")
plot(silhouette(cutcomplete[,2], distancias),
     col=sort(unique(cutcomplete[,2]))+1, main="Complete")
#
# k=3 (0.45) no es una mala clasificación
#
# Validación externa con iris$Species
#
table(cutcomplete[,1], iris$Species)
table(cutcomplete[,2], iris$Species)
#
# Los resultados no son buenos
# Parte de los 'versicolor' se agrupa con los 'virginica'
#
#######
#
plot(single, hang=-1, main=nombredatos, sub="", las=1,
     xlab="Distance='Euclidean'\nMethod='Single'")
#
# Se observa un posible corte de 3 o 5 clases
# Una de las clases es singular
#
(cutsingle <- cutree(single, k=c(3,5)))
#
# Validación interna
#
plot(silhouette(cutsingle[,1], distancias),
     col=sort(unique(cutsingle[,1]))+1, main="Single")
plot(silhouette(cutsingle[,2], distancias),
     col=sort(unique(cutsingle[,2]))+1, main="Single")
#
# Son malas clasificaciones
#
table(cutsingle[,1], cutcomplete[,1])
#
# Validación externa con 'iris$Species'
#
table(cutsingle[,1], iris$Species)
#
# Mal resultado
#
#######
#
plot(average, hang=-1, main=nombredatos, las=1,
     xlab="Distance='Euclidean'\nMethod='Average'")
#
# Se observa un posible corte de 2 o 4 clases
# Una de las clases es singular
# Para 4 clases, hay ademas otra clase pequeña
#
(cutaverage <- cutree(average, k=c(2,4)))
#
# Validación interna
#
plot(silhouette(cutaverage[,1], distancias),
     col=sort(unique(cutaverage[,1]))+1, main="Average")
plot(silhouette(cutaverage[,2], distancias),
     col=sort(unique(cutaverage[,2]))+1, main="Average")
#
# k=2 (0.58) es una buena clasificación
#
table(cutaverage[,1], cutcomplete[,1])
#
# Validación externa con iris$Species
#
table(cutaverage[,1], iris$Species)
#
# No diferencia las 'versicolor' de las 'virginica'
#
#######
#
plot(ward, hang=-1, main=nombredatos, las=1,
     xlab="Distance='Euclidean'\nMethod='Ward'")
#
# Se observa un posible corte de 2 clases
#
(cutward <- cutree(ward, k=2))
#
# Validación interna
#
plot(silhouette(cutward, distancias),
     col=sort(unique(cutward))+1, main="Ward")
#
# k=2 (0.58) es una buena clasificación
#
table(cutward, cutaverage[,1])
#
# 'cutward' es muy parecido a 'cutaverage[,1]'
# pues sólo difiere en un objeto
#
# Validación externa con 'iris$Species'
#
table(cutward, iris$Species)
#
# No diferencia las 'versicolor' de las 'virginica'
#
#######
#
plot(ward2, hang=-1, main=nombredatos, sub="", las=1,
     xlab="Distance='Euclidean^2'\nMethod='Ward.2D'")
#
# Se observa un posible corte de 2 o 3 clases
#
(cutward2 <- cutree(ward2, k=c(2,3)))
#
# Validación interna
#
#
plot(silhouette(cutward2[,1], distancias),
     col=sort(unique(cutward2[,1]))+1, main="Ward.2D")
plot(silhouette(cutward2[,2], distancias),
     col=sort(unique(cutward2[,2]))+1, main="Ward.2D")
#
#
# k=2 (0.58) es una buena clasificación
#
table(cutward2[,1], cutward)
#
# 'cutward2[,1]' es el mismo que 'cutward'
#
#######
#
plot(ward22, hang=-1, main=nombredatos, sub="", las=1,
     xlab="Distance='Euclidean^2'\nMethod='Ward'")
#
# Se observa un posible corte de 2 o 3 clases
#
(cutward22 <- cutree(ward22, k=c(2,3)))
#
# Validación interna
#
plot(silhouette(cutward22[,1], distancias),
     col=sort(unique(cutward22[,1]))+1, main="Ward.D^2")
plot(silhouette(cutward22[,2], distancias),
     col=sort(unique(cutward22[,2]))+1, main="Ward.D^2")
#
# k=2 (0.58) es una buena clasificación
#
table(cutward22[,1], cutward)
#
# 'cutward22[,1]' es el mismo que 'cutward'
#
#######
#
# La partición con k=2 correspondiente a los cortes equivalentes 
# 'cutward', 'cutward2[,1]' y 'cutward22[,1]')
# es la que mayor valor de índice obtiene
#
solucion <- cutward22[,1]
#
plot(ward22, hang=-1, main=nombredatos, las=1,
     xlab="Distance='Euclidean'\nMethod='Ward.D^2'")
rect.hclust(ward22, k=2, which=c(1,2), x=NULL, h=NULL,
            border=c("green3", "red"), cluster=solucion)
#
pairs(datos, pch=21, col="black", bg=solucion+1)
#
#######
#
# Interpretacion del resultado segun 'ward22'
#
sum(ward22$height)
#[1] 1192 
# = ((150-1)*4)*2, suma de varianzas de las 4 variables estandarizadas *2
#
# k=2 y cutward22 tienen propiedades
# que ayudan a la interpretacion de la solucion
#
k <- 2
solucion <- cutward22[,1]
#
# Cálculo de las medias de las variables por cada clase
#
medias.cl <- matrix(rep(0, k*ncol(datos)), ncol=ncol(datos))
for(j in 1:ncol(datos))
  medias.cl[,j] <- tapply(datos[,j], solucion, mean)
#
rownames(medias.cl) <- 1:k; colnames(medias.cl) <- colnames(datos)
medias.cl
#
# Las medias (ponderadas) de las medias parciales de cada clase
# coinciden con las medias totales (0, si las variables son estandarizadas)
#
medias.pond <- vector()
for(j in 1:ncol(datos))
  medias.pond[j] <- sum(as.numeric(table(solucion))*medias.cl[,j])/nrow(datos)
names(medias.pond) <- colnames(datos)
medias.pond # = 0 en las 4 variables estandarizadas
#
# Cálculo de varianzas de las variables entre las clases (B, between)
# siendo cada clase representada por su centro (media)
#
varB.pond <- vector() # varB.pond[j], varianza ponderada de cada variable
for(j in 1:ncol(datos))
  varB.pond[j] <- sum(as.numeric(table(solucion))*(medias.cl[,j]-medias.pond[j])^2)/nrow(datos)
names(varB.pond) <- colnames(datos)
varB.pond
#
# Cálculo de la suma de las diferencias cuadráticas de cada variable
# siendo cada clase representada por su centro (media)
#
( SSBj <- nrow(datos)*varB.pond )
#
# Cálculo de la suma de las diferencias cuadráticas
# de todas las variables y de todas las clases,
# siendo cada clase representada por su centro (media)
#
( SSB <- sum(SSBj) )
#
# Porcentaje de la variabilidad inicial,
# suma de las distancias entre cada par de objetos,
# explicada por la partición de k=2
#
SSB/((150-1)*4)
# [1]  0.6187981 # 62%
#
#######
#
# Silueta correspondiente a 'iris'
#
plot(silhouette(as.numeric(iris[,5]), distancias),
     col=sort(unique(as.numeric(iris[,5])))+1,
     main="Iris")
#
##########################################################################