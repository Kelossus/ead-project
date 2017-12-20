################ MODELO ################

naiveBayes <- function(nVarPreds) {
  modelo <- list()
  modelo$nPredictors <- nVarPreds
  
  modelo$Pc <- vector("numeric",length=2)
  
  modelo$Px_c <- matrix(0,nVarPreds,2*2)

  return(modelo)
}
predecir <- function(instancia, modelo) {
  probs <- vector("numeric",length=2)
  
  probs[1] <- modelo$Pc[1]
  probs[2] <- modelo$Pc[2]
  for (i in 1:modelo$nPredictors) {
    probs[1] <- probs[1]*modelo$Px_c[i,1+instancia[i]]
    probs[2] <- probs[2]*modelo$Px_c[i,3+instancia[i]]
  }
  return(probs/sum(probs))
}
aprender <- function(dataset, modelo) {
  
  modelo$Pc <- vector("numeric",length=2)
  modelo$Px_c <- matrix(0,modelo$nPredictors,2*2)
  
  ##########################
  ##### AQUI TU CODIGO #####
  ##########################

  return(modelo)
}


################## EM ##################

inicializar <- function(datos) {
  dataset <- list()
  dataset$labmatrix <- datos[which(!is.na(datos[,ncol(datos)])),]

  unlabmatrix <- datos[which(is.na(datos[,ncol(datos)])),]
  nUnlabExs <- nrow(unlabmatrix)

  dataset$peso <- vector("numeric", length=nUnlabExs*2)
  dataset$unlabmatrix <- matrix(0,nUnlabExs*2,ncol(datos))

  iAct <- 1
  for (i in 1:nUnlabExs) {
    dataset$unlabmatrix[iAct,] <- unlabmatrix[i,]
    dataset$unlabmatrix[iAct,ncol(datos)] <- 0
    dataset$peso[iAct] <- 0.5
    iAct <- iAct+1
    
    dataset$unlabmatrix[iAct,] <- unlabmatrix[i,]
    dataset$unlabmatrix[iAct,ncol(datos)] <- 1
    dataset$peso[iAct] <- 0.5
    iAct <- iAct+1
  }
  # dataset$peso <- c(1/3,2/3,1/3,2/3,1/3,2/3,2/3,1/3,2/3,1/3,2/3,1/3,1/2,1/2,1/2,1/2)
  return(dataset)  
}


EStep <- function(dataset, modelo){

  dataset$peso <- vector("numeric", length=length(dataset$peso))

  ##########################
  ##### AQUI TU CODIGO #####
  ##########################
  
  return(dataset)
}


MStep <- function(dataset, modelo){
  
  ##########################
  ##### AQUI TU CODIGO #####
  ##########################
  
  return(modelo)
}

testConvergencia <- function(modeloA, modeloB, epsilon) {
  
  resultado <- FALSE
  
  ##########################
  ##### AQUI TU CODIGO #####
  ##########################
  
  return ( resultado )
}

EM <- function(datos, epsilon) {
  cDataset <- inicializar(datos)

  # print(cDataset$peso)
  
  modelo <- aprender(cDataset,modelo)

  # print(modelo$Pc)
  # print(modelo$Px_c)
  # readline()
  
  convergencia <- FALSE
  while (!convergencia) {
    cDataset <- EStep(cDataset,modelo)

    antModelo <- modelo
    modelo <- MStep(cDataset, antModelo)

    convergencia <- testConvergencia(modelo, antModelo, epsilon)

    # print(cDataset$peso)
    # print(modelo$Pc)
    # print(modelo$Px_c)
    # readline()
  }
  
  return(list(modelo,cDataset))
}




############### EJECUCION ###############
modelo <- naiveBayes(2)

datos <- cbind(c(0,1,0,0,1,1,1,1,0,1),c(0,0,1,1,0,0,0,1,1,1),c(1,NA,NA,NA,1,0,NA,NA,0,NA))

EM(datos, 0.001)
