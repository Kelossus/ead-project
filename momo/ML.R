
library(mlr)

# Dominio: Yeast
# The Yeast dataset is formed by micro-array expression data and phylogenetic 
# profiles (103 variables) for 2417 genes. Each gene is associated with a set 
# of functional classes whose maximum size can be potentially more than 190. The 
# set of classes is structured in a tree whose leaves are the functional categories. 
# Given a gene, knowing which edge to take from one level to another leads to a 
# leaf and, thus, to a functional class. This problem aims to solve the first decision: 
# to predict which edge to take from the root to the first level of the tree.
dataset <- getTaskData(yeast.task)
tarea <- makeMultilabelTask(id = "multi", data = dataset, target = colnames(dataset)[1:14])
tarea


labelCount <- colSums(dataset[,1:14])
labelCount

nLabelsPerInst <- rowSums(dataset[, 1:14])
nLabelsPerInst
table(nLabelsPerInst)

labelsets <- do.call(paste, c(dataset[, 1:14]+0, sep = ""))
labelsets <- table(as.factor(labelsets))
labelsets


# Label cardinality
# Media de etiquetas por instancia
labelCardinality <- function(dataset) {
  lc <- sum(rowSums(dataset[, 1:14])) / length(dataset[,1])
  return(lc)
}

labelCardinality(dataset)

# Label density
# Media de la proporcion de etiquetas por instancia
labelDensity <- function(dataset) {
  lc <- sum(rowSums(dataset[, 1:14])) / length(dataset[,1])
  ld <- lc / 14
  return(ld)
}

labelDensity(dataset)






# Seleccionar clasificador base
# Se pueden usar:
# - classif.rpart  : Arboles de decision
# - classif.nnet   : Redes Neuronales
# - classif.logreg : Regresion Logistica
# - classif.knn    : K vecinos mas cercanos
# - ...
# listLearners() para verlos todos
clasifBase <- makeLearner("classif.rpart", predict.type = "prob")
clasifBase

############## Tecnica de clasificacion multi-etiqueta ##############
# 
# Binary relevance
# 
# Convertir el problema multi-etiqueta (L etiquetas) en L subproblemas 
# de clasificacion binaria. Cada subproblema se resuelve independientemente 
# con un clasificador estandar binario.
tecnica <- makeMultilabelBinaryRelevanceWrapper(clasifBase)
tecnica


# Classifier chains
# 
# Igualmente, tenemos L clasificadores. Difiere en que la prediccion del 
# clasificador 'l' se usa como variable predictora de los clasificadores 
# subsecuentes ('l+1',...,L). Por lo tanto, es necesario establecer un 
# orden entre las etiquetas y sus respectivos clasificadores. 
# ** Incorpora la posibilidad de modelar dependencias entre etiquetas.
# tecnica <- makeMultilabelClassifierChainsWrapper(clasifBase)
# tecnica



# Stacking
# 
# Igual que Dependent Binary Relevance, pero no usa tampoco las etiquetas 
# reales en fase de aprendizaje. Usa un clasificador inicial basado en 
# Binary Relevance para obtiener una prediccion de las etiquetas y con 
# esas predicciones aprende el Dependent Binary Relevance.
# tecnica <- makeMultilabelStackingWrapper(clasifBase)
# tecnica

# Nested stacking
# 
# Igual que Classifier Chains, pero no usa tampoco las etiquetas reales 
# en fase de aprendizaje. Aprende el primer clasificador, obtiene una 
# prediccion de las etiquetas y con esas predicciones crea la columna 
# de los subsecuentes clasificadores.
# tecnica <- makeMultilabelNestedStackingWrapper(clasifBase)
# tecnica

# Falta Label Powerset!!!!!!
# Creamos todas las combinaciones posibles de subconjuntos de etiquetas (2^L-1)
# y aprendemos un clasificador multi-class en este espacio transformado.
# ** Problema: MUCHISIMAS etiquetas
# Random k-labelsets: aprender multiples clasificadores multi-class donde 
# las etiquetas representan conjuntos (elegidos aleatoriamente) de etiquetas 
# en el espacio original.

# Aprendizaje (con 1500 casos)
modelo = train(tecnica, tarea, subset = 1:1500)
modelo

# Prediccion (con 917 casos)
prediccion <- predict(modelo, newdata = dataset[1501:nrow(dataset),])
t(prediccion$data)




#################### Evaluando el modelo ####################
# Medidas "especificas" de clasificacion multi-etiqueta
# - subset01 : Proporcion de casos donde se acierta
# - hamloss  : Función de pérdida de Hamming - Proporcion de etiquetas 
#              fuera de la intersección con respecto al total de etiquetas
# - acc      : Media de la interseccion entre la union de los sets de 
#              etiquetas reales y predicho (jaccard)
# - f1       : Micro-F1 - Media armonica de precision y recall
# - ...
# listMeasures("multilabel") para verlas todas
performance(prediccion, measures = list(multilabel.subset01, multilabel.hamloss, multilabel.acc,multilabel.f1))

# Medidas "genericas" de clasificacion supervisada
# aplicadas al acierto en cada clase individualmente
# - acc  : Porcentaje de acierto
# - mmce : error de clasificacion
# - tpr  : recall
# - ppv  : precision
# - f1   : Media armonica de precision y recall
# - ...
# listMeasures() para verlas todas
getMultilabelBinaryPerformances(prediccion, measures = list(acc, mmce,tpr,ppv,f1))

