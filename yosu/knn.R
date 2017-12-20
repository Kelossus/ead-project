###############################################################################
#
# Analisis descriptivo de lass variables cuantitativas
# en relación a la variable cualitativa
#
for (j in 1:4)
  stripchart(iris[,j] ~ iris[,5],
             pch=19, method="stack", col=c("red","green3","blue"),
             xlab=names(iris)[j], ylab=names(iris)[5], main="Iris Data Set")
#
# Razon de correlacion (indicador)
# http://fr.wikipedia.org/wiki/Rapport_de_corr%C3%A9lation
#
# Se define una funcion que calcula la razon de correlacion, eta2
#
eta2 <- function(x, factor)
{
  niv <- levels(factor)
  numniv <- length(niv)
  SSB <- 0
  for(i in 1:numniv)
  {
    xx <- x[factor==niv[i]]
    nxx <- length(xx)
    SSB <- SSB + nxx*(mean(xx)-mean(x))^2
  }
  SST <- (length(x)-1)*var(x)
  #
  eta2 <- SSB/SST
  #
  return(eta2)
}
#
# 0 <= eta2 <= 1
# Si eta2=0, entonces no hay correlacion  entre 'x' e 'y',
# las medias parciales son todas iguales
# Si eta2=1, entonces hay una dependencia funcional entre 'x' e 'y'
# no hay variabilidad en las categorias
#
etados <- vector()
for(j in 1:4) etados[j] <- eta2(iris[,j], iris[,5])
names(etados) <- names(iris)[1:4]
etados
#
# Las variables 'Petal.Length' y 'Petal.Width' son las variables
# mas relacionadas con la variable 'Species'
#
# Este hecho ha sido verificado antes graficamente
#
# Es de recordar que hay tres variables altamente correladas linealmente
#
cor(iris[,-5])
#
#########################################################################
#
# Analisis grafico de todos los pares (una variable cuantitativa
# y una variable qualitative) de golpe
#
# Graficas
#
pairs(iris[,-5], main="Iris Data Set",
      pch=21, bg = c("red", "green3", "blue")[iris$Species])
#
#########################################################################


#########################################################################
#
# Introduccion de los datos: 'iris'
#
#
iris
names(iris)
#
#[1] "Sepal.Length" "Sepal.Width" "Petal.Length" "Petal.Width" "Species"
#
rownames(iris)
#
dim(iris)
#
#########################################################################
#
library(class)
#
?knn
#
# k=6 , training_set = iris, test_set = new (nuevo objeto)
#
new <- c(5.8, 3.1, 3.8, 1.20)
knn(iris[,-5], new, cl=iris[,5], k=6)
#
irisknn <- knn(iris[,-5], new, cl=iris[,5], k=6)
#
attributes(irisknn)
#
irisknn <- knn(iris[,-5], new, cl=iris[,5], k=6, prob=TRUE)
#
attributes(irisknn)
#
########################################################################



##########################################################################
#
library(class)
#
# Se aplica el clasificador sobre el propio conjunto de datos
#
irisknn <- knn(train=iris[,1:4], test=iris[,1:4], cl=iris[,5], k=7)
#
( tabliris <- table(iris[,5], irisknn) )
#
# Calculo del error
#
( err <- 1-(tabliris[1,1]+tabliris[2,2]+tabliris[3,3])/sum(tabliris) )
#
##########################################################################
#
# Calculo del error para valores diferentes de k
#
kmax <- 20
#
err <- vector()
#
for (k in 1:kmax){
  irisknn <- knn(train=iris[,1:4], test=iris[,1:4], cl=iris[,5], k=k)
  tabliris <- table(iris[,5], irisknn)
  err[k] <- 1-(tabliris[1,1]+tabliris[2,2]+tabliris[3,3])/sum(tabliris)
}
#
plot(1:kmax, err, xlab="k", ylab="", main="Tasa de error", las=1,
     type="b", pch=19, col="red")
#
# Claro, si k=1 err=0
#
# El minimo se alcanza en k = 11, 12, 14, 15
#
##########################################################################



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
training <- sample(1:150, 100)
test <- setdiff(1:150, training)
#
irisknn <- knn(train=iris[training,1:4], test=iris[test,1:4],
               cl=iris[training,5], k=11)
#
( tabliris <- table(iris[test,5], irisknn) )
#
# Calculo del error
#
( err <- 1-(tabliris[1,1]+tabliris[2,2]+tabliris[3,3])/sum(tabliris) )
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
  training <- sample(1:150, 100)
  test <- setdiff(1:150, training)
  for (k in 1:kmax){
    irisknn <- knn(train=iris[training,1:4], test=iris[test,1:4],
                   cl=iris[training,5], k=k)
    tabliris <- table(iris[test,5], irisknn)
    err[k, i] <- 1-(tabliris[1,1]+tabliris[2,2]+tabliris[3,3])/sum(tabliris)
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
n <- nrow(iris)
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
  irisknn <- knn(iris[training, 1:4], iris[test, 1:4],
                 cl=iris[training, 5], k=11) # k=11, arbitrario
  tabliris <- table(iris[test,5], irisknn)
  err <- 1-(tabliris[1,1]+tabliris[2,2]+tabliris[3,3])/sum(tabliris)
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
    irisknn <- knn(iris[training, 1:4], iris[test, 1:4],
                   cl=iris[training, 5], k)
    tabliris <- table(iris[test,5], irisknn)
    err <- 1-(tabliris[1,1]+tabliris[2,2]+tabliris[3,3])/sum(tabliris)
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
n <- nrow(iris)
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
  irisknn <- knn(iris[-b, 1:4], iris[b, 1:4],
                 cl=iris[-b, 5], k=11) # k=11, arbitrario
  tabliris <- table(iris[b,5], irisknn)
  err <- 1-(tabliris[1,1]+tabliris[2,2]+tabliris[3,3])/sum(tabliris)
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
    irisknn <- knn(iris[-b, 1:4], iris[b, 1:4],
                   cl=iris[-b, 5], k)
    tabliris <- table(iris[b,5], irisknn)
    err <- 1-(tabliris[1,1]+tabliris[2,2]+tabliris[3,3])/sum(tabliris)
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
# El minimo se alcanza en k= 16, 18, 20
#
##########################################################################




##########################################################################
#
# Si no se procediera a la estandarizacion:
# datos <- iris
#
# Normalizacion (estandardizacion) de las variables cuantitativas
#
datos <- data.frame(scale(iris[,-5]), CLASS=iris[,5])
n <- nrow(datos)
p <- ncol(datos)
nlevel <- length(levels(datos[,p]))
#
# Verificacion
#
apply(datos[,-p], 2, mean); apply(datos[,-p], 2, var)
#
##########################################################################
#
library(class)
#
# Construccion de bloques
#
B <- 6
tamanno <- n%/%B
set.seed(2014)
alea <- runif(n)
rang <- rank(alea)
bloque <- (rang-1)%/%tamanno +1
bloque <- as.factor(bloque)
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
    training <- which(bloque!=b)
    test <- which(bloque==b)
    #
    datosknn <- knn(datos[training, -p], datos[test, -p], cl=datos[training, p], k)
    tabledatos <- table(datos[test,p], datosknn)
    diagtabledatos <- 0
    for(j in 1:nlevel) diagtabledatos <- diagtabledatos + tabledatos[j,j]
    err <- 1-diagtabledatos/sum(tabledatos)
    #
    err.t <- rbind(err.t, err)
  }
  #
  err.cv[k] <- mean(err.t)
  #
}
#
round(err.cv, digits=4)
#
plot(1:kmax, err.cv, xlab="k", ylab="", main="Taux d'erreur", las=1,
     type="b", pch=19, col="red")
#
# la tasa de error minimo se alcanza k=14
# Es la estimacion de la tasa de error real (validacion cruzada)
#
# Calculo de la tasa de error aparente
#
datosknn <- knn(datos[, -p], datos[, -p], cl=datos[, p], k=14)
diagtabledatos <- 0
for(j in 1:nlevel) diagtabledatos <- diagtabledatos + tabledatos[j,j]
( err <- 1-diagtabledatos/sum(tabledatos) )
#
#########################################################################
#
# Prediccion de un objeto nuevo:
#
new <- c(5.8, 3.1, 3.8, 1.20)
#
# Hay que normalizarlo (estandardizarlo)
#
media <- sapply(datos[,-p], mean)
desv <- sapply(datos[,-p], sd)
#
( newz <- (new-media)/desv )
#
knn(datos[, -p], newz, cl=datos[, p], k=14)
#
###########################################################################################


##################################################################################
#
# Discriminacion de las clases en el plano Petal.Length-Petal.Width
#
#
# Normalizacion (estandardizacion) de las variables cuantitativas
#
datos <- data.frame(scale(iris[,-5]), CLASS=iris[,5])
n <- nrow(datos)
p <- ncol(datos)
nlevel <- length(levels(datos[,p]))
#
library(class)
#
##########################
#
number <- 100
region1 <- seq(min(datos[,1]),max(datos[,1]),length=number)
region2 <- seq(min(datos[,2]),max(datos[,2]),length=number)
region3 <- seq(min(datos[,3]),max(datos[,3]),length=number)
region4 <- seq(min(datos[,4]),max(datos[,4]),length=number)
#
datosregion <- NULL
for(i in 1:number)
  for(j in 1:number)
    datosregion <- rbind(datosregion, c(region1[i], region2[j], region3[i], region4[j]))
datosregion <- as.data.frame(datosregion)
names(datosregion) <- names(datos)[1:4]
#
datosknn <- knn(datos[, -p], datosregion, cl=datos[, p], k=14)
#
plot(datos$Petal.Length, datos$Petal.Width, type="n", las=1,
     xlab="Petal.Length", ylab="Petal.Width")
#
points(datosregion$Petal.Length, datosregion$Petal.Width,
       col=c("pink", "green1", "lightblue1")[datosknn],
       pch=15, cex=1.5)
points(datos$Petal.Length, datos$Petal.Width,
       col=c("red", "green3", "blue")[datos$CLASS],
       bg= c("red", "green3", "blue")[datos$CLASS],
       pch=c(rep(22,50),rep(24,50),rep(25,50)))
#
##################################################################################

