##############################################################################
# Visualizacion con tres variables

load(file = "column.RData")
dataset <- column

library(scatterplot3d)
?scatterplot3d
attach(dataset)
#

rotazio_irudikapena <- function(a, b, c, freq=24) {
  for (i in 0:(freq-1)){
    scatterplot3d(a, b, c,
                  pch=19, color=c("red", "green3", "blue")[DIAG],
                  angle=15*i)
  }
}

rotazio_irudikapena(GS, PR, SS)

#

pairs(dataset[,-7], main="Temp",
      pch=21, bg = c("red", "green3", "blue")[DIAG])

parcoord(dataset[,c(1, 2, 3, 4,5, 6)],
         col=c("red", "green3")[unclass(DIAG)])

detach(dataset)


datosz <- scale(dataset)

datos <- datosz
#
distancias <- dist(datos, method="euclidean")
#
# Visualización de las distancias resultantes
#
hist(distancias, main="Datos estandarizados")
#
# Mapa de calor para distancias por pares de objetos:
# A menor distancia, mayor intensidad de calor (color rojo)
#
image(as.matrix(distancias), col=heat.colors(12), axes=FALSE,
      xlab="Objetos", ylab="Objetos",
      main="A menor distancia,\nmayor intensidad")

datos <- dataset
#
distancias <- dist(datos, method="euclidean")
#
hist(distancias, main="Datos originales")
#
# Mapa de calor
#
image(as.matrix(distancias), col=heat.colors(12), axes=FALSE,
      xlab="Objetos", ylab="Objetos",
      main="A menor distancia,\nmayor intensidad")
#

