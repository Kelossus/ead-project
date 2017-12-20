library(cluster)
#################################
load("yosu/ocr.RData")


# This will optimize the given database for different methods of linkage, and with different amount of clusters.
realNclus <- length(unique(ocr[,41]))
clustWindow <- 8
nClusters <- (realNclus - clustWindow):(realNclus + clustWindow)
methods <- c("complete", "single", "average", "ward.D", "ward.D2")

# Normalization
# datos <- scale(ocr[,-41], center=TRUE, scale=TRUE)
datos <- ocr[,-41]

# No normalization

distancias <- dist(datos, method="euclidean", upper=TRUE, diag=TRUE)


#Initialization of variables
bestMethod <- NaN
bestNClust <- NaN
maxMeanSilhouetteWidth <- 0

  
computeSilhouetteMean <- function(cut){
  s <- silhouette(cut, distancias)
  return(mean(silhouette(cut, distancias)[,3]))
}

# Main loop
for (method in methods){
  tree = hclust(distancias, method=method)
  cut <- cutree(tree, k=nClusters)
  
  meanSilhouettes <- apply(cut, 2, computeSilhouetteMean)
  
  if (maxMeanSilhouetteWidth < max(meanSilhouettes)){
    bestNClust <- which.max(meanSilhouettes) + realNclus - clustWindow - 1
    bestMethod <- method
    maxMeanSilhouetteWidth <- max(meanSilhouettes)
  }
}

