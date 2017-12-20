load("yosu/ocr.RData")

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
training <- sample(1:200, 125)
test <- setdiff(1:200, training)
#
ocrknn <- knn(train=ocr[training,1:40], test=ocr[test,1:40],
               cl=ocr[training,41], k=11)
#
( tablocr <- table(ocr[test,41], ocrknn) )
#
# Calculo del error
#
( err <- 1-(tablocr[1,1]+tablocr[2,2]+tablocr[3,3])/sum(tablocr) )
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
  training <- sample(1:200, 125)
  test <- setdiff(1:200, training)
  for (k in 1:kmax){
    ocrknn <- knn(train=ocr[training,1:40], test=ocr[test,1:40],
                  cl=ocr[training,41], k=k)
    tablocr <- table(ocr[test,41], ocrknn)
    err[k, i] <- 1-(tablocr[1,1]+tablocr[2,2]+tablocr[3,3])/sum(tablocr)
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
n <- nrow(ocr)
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
  ocrknn <- knn(train=ocr[training,1:40], test=ocr[test,1:40],
                cl=ocr[training,41], k=11)
  tablocr <- table(ocr[test,41], ocrknn)
  err <- 1-(tablocr[1,1]+tablocr[2,2]+tablocr[3,3])/sum(tablocr)
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
    ocrknn <- knn(train=ocr[training,1:40], test=ocr[test,1:40],
                  cl=ocr[training,41], k=k)
    tablocr <- table(ocr[test,41], ocrknn)
    err <- 1-(tablocr[1,1]+tablocr[2,2]+tablocr[3,3])/sum(tablocr)
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
n <- nrow(ocr)
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
  ocrknn <- knn(train=ocr[-b,1:40], test=ocr[b,1:40],
                cl=ocr[-b,41], k=11) # k=11, arbitrario
  err <- ifelse(ocrknn == ocr[b, 41], 1, 0)
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
    ocrknn <- knn(train=ocr[-b,1:40], test=ocr[b,1:40],
                  cl=ocr[-b,41], k=k)
    err <- ifelse(ocrknn == ocr[b, 41], 1, 0)
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