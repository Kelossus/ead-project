load("yosu/column0.RData")

col <- column

length(col[,1])
##########################################################################
#
# 1.er enfoque: Problema de sobreajuste
#
# Metodo de retencion (holdout)
#
# Training set 66% Test set 33%
#
library(class)
#
set.seed(2015)
training <- sample(1:310, 200)
test <- setdiff(1:310, training)
#
colknn <- knn(train=col[training,1:6], test=col[test,1:6],
               cl=col[training,7], k=11)
#
( tablcol <- table(col[test,7], colknn) )
#
# Calculo del error
#
( err <- 1-(tablcol[1,1]+tablcol[2,2]+tablcol[3,3])/sum(tablcol) )
#
##########################################################################
#
# Calculo del error para valores diferentes de k
#
kmax <- 20
imax <- 100
#
err <- matrix(nrow=kmax, ncol=imax)
#
set.seed(2015)
for (i in 1:imax){
  training <- sample(1:310, 200)
  test <- setdiff(1:310, training)
  for (k in 1:kmax){
    colknn <- knn(train=col[training,1:6], test=col[test,1:6],
                  cl=col[training,7], k=k)
    err[k, i] <- 1-(tablcol[1,1]+tablcol[2,2]+tablcol[3,3])/sum(tablcol)
  }
  meanErr <- rowMeans(err)
}

#
plot(1:kmax, meanErr, xlab="k", ylab="", main="Tasa de error", las=1,
     type="b", pch=19, col="red")

#
# El minimo se alcanza en varios k, k=11 entre ellos
#
##########################################################################




#########################################################################
#
library(class)
#
# Estimacion del error: Validacion cruzada
# https://es.wikipedia.org/wiki/Validaci%C3%B3n_cruzada
#
# Construccion de bloques
#
# numero de observaciones
#
n <- nrow(col)
#
# numero de bloques: B (B-validacion cruzada)
#
B <- 6
#
# determinar el tamaño de cada bloque
#
tamanno <- n%/%B
#
# para obtener la misma secuencia todo el tiempo
#
set.seed(2014)
#
# generar una columna de valores aleatorios
#
alea <- runif(n)
#
# asociar a cada objeto un rango
#
rang <- rank(alea)
#
# asociar à cada objeto un numero de bloque
#
bloque <- (rang-1)%/%tamanno +1
#
# transformarlo en factor
#
bloque <- as.factor(bloque)
#
# impresion de control
#
summary(bloque)
#
###############################
#
# Comienzo de la validacion cruzada
#
err.t <- vector()
for(b in 1:B){
  #
  # Aprender el modelo sobre todos los objetos salvo los del bloque b
  #
  training <- which(bloque!=b)
  test <- which(bloque==b)
  #
  colknn <- knn(train=col[training,1:40], test=col[test,1:40],
                cl=col[training,41], k=11)
  tablcol <- table(col[test,41], colknn)
  err <- 1-(tablcol[1,1]+tablcol[2,2]+tablcol[3,3])/sum(tablcol)
  #
  # recoger el valor del error
  #
  err.t <- rbind(err.t, err)
}
#
# vector de los errores recogidos
#
err.t
#
# error en la validacion cruzada:
# puede bastar una media no ponderada
# ya que los bloques son de tamaño identico
#
( err.cv <- mean(err.t) )
#
##########################################################################
#
# KNN para diferentes valores de k
#
kmax <- 30
err.cv <- rep(NA, kmax)
#
for(k in 1:kmax){
  #
  err.t <- vector()
  #
  for(b in 1:B){
    #
    training <- which(bloque != b)
    test <- which(bloque == b)
    #
    colknn <- knn(train=col[training,1:40], test=col[test,1:40],
                  cl=col[training,41], k=k)
    tablcol <- table(col[test,41], colknn)
    err <- 1-(tablcol[1,1]+tablcol[2,2]+tablcol[3,3])/sum(tablcol)
    #
    err.t <- rbind(err.t, err)
  }
  #
  err.cv[k] <- mean(err.t)
  #
}
#
plot(1:kmax, err.cv, xlab="k", ylab="", main="Tasa de error", las=1,
     type="b", pch=19, col="red")
#
# El minimo se alcanza en k=11
#
##########################################################################

#########################################################################
#
library(class)
#
# Estimacion del error: Validacion cruzada
#
# Caso particular: 'Leave one out'
#
# B=n, el numero de objetos
# Computacionalmente muy costoso
#
n <- nrow(col)
#
# numero de bloques: B (B-validacion cruzada)
#
B <- n
#
###############################
#
# Comienzo de la validacion cruzada 'Leave one out'
#
err.t <- vector()
for(b in 1:B){
  #
  # Aprender el modelo sobre todos los objetos salvo el objeto b
  #
  colknn <- knn(train=col[-b,1:40], test=col[b,1:40],
                cl=col[-b,41], k=11) # k=11, arbitrario
  err <- ifelse(colknn == col[b, 41], 1, 0)
  #
  # recoger el valor del error
  #
  err.t <- rbind(err.t, err)
}
#
# error en la validacion cruzada 'leave one out'
#
( err.cv <- mean(err.t) )
#
##########################################################################
#
# KNN para diferentes valores de k
#
kmax <- 30
err.cv <- rep(NA, kmax)
#
for(k in 1:kmax){
  #
  err.t <- vector()
  #
  for(b in 1:B){
    #
    colknn <- knn(train=col[-b,1:40], test=col[b,1:40],
                  cl=col[-b,41], k=k)
    err <- ifelse(colknn == col[b, 41], 1, 0)
    #
    err.t <- rbind(err.t, err)
  }
  #
  err.cv[k] <- mean(err.t)
  #
}
#
plot(1:kmax, err.cv, xlab="k", ylab="", main="Taux d'erreur", las=1,
     type="b", pch=19, col="red")
#
# 
#
##########################################################################