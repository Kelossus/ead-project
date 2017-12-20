load("yosu/ejercicios1.RData")

nombredatos <- "una variable"

datos <- scale(data[,1], center=TRUE, scale=TRUE)
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

(cutcomplete <- cutree(average, k=c(3,5)))

library(cluster)
plot(silhouette(cutcomplete[,1], distancias),
     col=sort(unique(cutcomplete[,1]))+1, main="Complete")
plot(silhouette(cutcomplete[,2], distancias),
     col=sort(unique(cutcomplete[,2]))+1, main="Complete")











# other data 
load("yosu/ejercicios2.RData")

nombredatos <- "dos variables"


datos <- scale(data[,1:2], center=TRUE, scale=TRUE)
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

(cutcomplete <- cutree(single, k=c(12, 13, 14, 15, 16)))

library(cluster)

plot(silhouette(cutcomplete[,1], distancias),
     col=sort(unique(cutcomplete[,1]))+1, main="Single")
plot(silhouette(cutcomplete[,2], distancias),
     col=sort(unique(cutcomplete[,2]))+1, main="Single")
plot(silhouette(cutcomplete[,3], distancias),
     col=sort(unique(cutcomplete[,3]))+1, main="Single")
plot(silhouette(cutcomplete[,4], distancias),
     col=sort(unique(cutcomplete[,4]))+1, main="Single")
plot(silhouette(cutcomplete[,5], distancias),
     col=sort(unique(cutcomplete[,5]))+1, main="Single")

##########################################################################



