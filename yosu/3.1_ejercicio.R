
load(file = "yosu/ocr.RData")

datos0 <- ocr[,-41]
#
# Estandarización de las variables (media 0, varianza 1)
#
datos <- scale(datos0)
#
# 'k' arbitrario
#
#
?kmeans


IB <- km$betweenss/km$totss # IB <- 1-km$tot.withinss/km$totss
IB
for(k in 1:20)
{
  km <- kmeans(datos, k, nstart=100, iter.max=10000)
  IB[k] <- km$betweenss/km$totss
}
cbind(k=1:length(IB), IB=round(IB, digits=6))
IB
plot(IB, type="b", las=1, xlab="k")
selectk <- NULL
for(k in 3:length(IB))
  selectk <- rbind(selectk, c(k, IB[k]-IB[k-1], IB[k]/IB[k-1]))
colnames(selectk) <- c("k", "IB[k]-IB[k-1]", "IB[k]/IB[k-1]")
selectk


k <- 4
km <- kmeans(datos, k, nstart=100, iter.max=10000)

compocl <- vector("list", k)
for(kcl in 1:k) compocl[[kcl]] <- which(km$cluster == kcl)

distancias <- dist(datos, method="euclidean", upper=TRUE, diag=TRUE)^2
distancias.m <- as.matrix(distancias)

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

library(cluster)
#
silk <- silhouette(km$cluster, distancias)
#
silk



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

km$cluster

etclases <- sort(unique(km$cluster))
plot(silk, col=etclases)
mean(silk[,3])


km$cluster.new <- km$cluster
km$cluster.new[which(silk[,3] < 0)] <- silk[which(silk[,3] < 0), 2]

silk <- silhouette(km$cluster.new, distancias)
etclases <- sort(unique(km$cluster))
plot(silk, col=etclases+1)
mean(silk[,3])


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
db <- NA # db[1] no tiene sentido
for(k in 2:20)
{
  km <- kmeans(datos, k, nstart=100, iter.max=10000)
  compocl <- vector("list", k)
  for(kcl in 1:k) compocl[[kcl]] <- which(km$cluster == kcl)
  db[k] <- DB(distancias.m, compocl,k)
}