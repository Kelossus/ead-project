dat <- read.csv(file="./class/master/ead/data/HR_comma_sep.csv", header = T, sep = ",",dec = ".",stringsAsFactors = T)
########################
# 

#
# +-----------+--------------------+-----------------+----------------+----------------------+--------------------+
# | Variables | 1                  | 2               | 3              | 4                    | 5                  |
# +-----------+--------------------+-----------------+----------------+----------------------+--------------------+
# | Name      | satisfaction_level | last_evaluation | number_project | average_montly_hours | time_spend_company |
# +-----------+--------------------+-----------------+----------------+----------------------+--------------------+
# | Type      | Cuantitative       | Cuantitative    | Cuantitative   | Cuantitative         | Cuantitative       |
# +-----------+--------------------+-----------------+----------------+----------------------+--------------------+
# | Range     | [0, 1] \in \R      | [0, 1] \in \R   | \in \N         | \in \R               | \in \N             |
# +-----------+--------------------+-----------------+----------------+----------------------+--------------------+
  
# +-----------+---------------+--------------+-----------------------+----------------+--------------+
# | Variables | 6             | 7            | 8                     | 9              | 10           |
# +-----------+---------------+--------------+-----------------------+----------------+--------------+
# | Name      | work_accident | left (Class) | promotion_last_5years | dept           | salary       |
# +-----------+---------------+--------------+-----------------------+----------------+--------------+
# | Type      | Cualitative   | Cualitative  | Cualitative           | Cualitative    | Cuantitative |
# +-----------+---------------+--------------+-----------------------+----------------+--------------+
# | Range     | \in \B        | 0 \xor 1     | \in \B                | \in ["sales",  |  \in ["low", |
# |           |               |              |                       | "accounting",  | "medium",    |
# |           |               |              |                       | "hr",          | "high"]      |
# |           |               |              |                       | "technical",   |              |
# |           |               |              |                       | "support",     |              |
# |           |               |              |                       | "management",  |              |
# |           |               |              |                       | "IT",          |              |
# |           |               |              |                       | "product_mng", |              |
# |           |               |              |                       | "marketing",   |              |
# |           |               |              |                       | "RandD"]       |              |
# +-----------+---------------+--------------+-----------------------+----------------+--------------+


# -- Descripción breve de las variables:
# 
# - satisfaction level:
# Representa cuan satisfechos estan los/las empleado/as con el puesto de trabajo.
#
# - last_evaluation:
# Valor de la última evaluación que se hizo del/la trabajador/a.
#
# - number_project:
# Número de proyectos completados en el trabajo.
#
# - average_montly_hours:
# Media de las horas metidas trabajando semanalmente.
#
# - time_spend_company:
# Cantidad de años que el/la trabajador/a .
#
# - work_accident:
# Representación binaria de si el/la empleado/a ha sufrido accidentes laborales.
#
# - left (class):
# La clase a predecir. Determina si el/la empleado/a ha decidido dejar el 
# el puesto de trabajo o quedarse.
#
# - promotion_last_5years
# Representación binaria de si el/la empleado/a obtuvo una ascensión en los
# últimos 5 años.
#
# - dept:
# Departamento al cual pertenece el/la empleado/a.
#
# - salary:
# Nivel relativo de salario en comparación a la media del departamento.

# La clase a predecir es la séptima columna, la vamos a quitar
# Para analizar los datos.
dat$left <- as.factor(dat$left)
emp <- dat[,-7]


# Traduciremos la variable de salario así: low=0, medium=1, high=2. 
# Aunque esta variable sea cualitativa, contiene propiedades de orden,
# ya que un salario bajo esta mas lejos de un salario alto que un salario
# intermedio

toNumSalary <- function(salary){
  switch(salary,
         low={return(0)},
         medium={return(1)},
         high={return(2)})
}

emp$salary <- apply(emp["salary"], 1, toNumSalary)


# Para poder emplear las divisiones de los departamentos, se va a generar
# una columna de pertenencia binaria para cada tipo de departamento.
semp <- emp[,-8]

semp$isSales <- ifelse(emp$dept=="sales",  1, 0)
semp$isAccounting <- ifelse(emp$dept=="accounting", 1, 0)
semp$isHr <- ifelse(emp$dept=="hr", 1, 0)
semp$isTechnical <- ifelse(emp$dept=="technical", 1, 0)
semp$isSupport <- ifelse(emp$dept=="support", 1, 0)
semp$isManagement <- ifelse(emp$dept=="management", 1, 0)
semp$isIT <- ifelse(emp$dept=="IT", 1, 0)
semp$isProduct_mng <- ifelse(emp$dept=="product_mng", 1, 0)
semp$isMarketing <- ifelse(emp$dept=="marketing", 1, 0)
semp$isRandD <- ifelse(emp$dept=="RandD", 1, 0)


# Vamos a hacer una selección de las componentes principales. Para ello,
# usaremos la funcion prcomp con centrado y escalado ya que los rangos de las
# variables no son ni parecidas, ni tienen el centro en el origen.
pr <- prcomp(semp, center = T, scale = T)
rotated <- pr$x

eigen.values = diag(cov(rotated))

variability = eigen.values/sum(eigen.values)

var_inc <- c()
for (i in 2:length(variability)){
  var_inc<- c(var_inc, variability[i-1]-variability[i])
}

# La variable "variability" no muestra la variabilidad de cada componente 
# principal. 
# La variable "var_inc" muestra el cambio de variabilidad entre la componente
# i-1 e i. Empieza desde la componente 2.


# Vamos a imprimir la variabilidad acumulada y los cambios en variabilidad
layout(1:2)
plot(cumsum(variability), type = "s", xlab = "Total variability if cut in i-th variable",
     ylab="", yaxt="n", xaxt="n", las=1)
axisvals <- round(cumsum(variability), digits = 4)
axis(2,at=cumsum(variability),labels=axisvals, las=1)
axis(1,at=1:length(variability), labels=1:length(variability))
plot(var_inc, type = "o", 
     xlab= "Difference in variability with former eigenvalue", ylab="")
axis(1,at=1:length(variability), labels=1:length(variability), xaxt="n")

# Parece que no hay gran diferencia entre la variabilidad de cada elemento. 
# Si observamos la segunda gráfica, podemos ver que no hay grandes diferencias
# entre las componentes. Esto es mala señal, puesto que indica que las 
# variables que podemos eleminar son muy pocas.


# Para analizar la variabilidad de cada componente mejor, vamos a usar la 
# librería plotly, que dispone de gráficos interactivos muy útiles para 
# entender mejor los datos que estamos viendo en pantalla.

library(plotly)

correlacion = data.frame(abs(cor(semp, rotated)))



plot_ly(data = correlacion, x = row.names(correlacion), type = "scatter",
             y = ~PC1, name="PC1", mode="markers") %>%
  add_trace(y = ~PC2, name = "PC2", mode = 'markers') %>%
  add_trace(y = ~PC3, name = "PC3", mode = 'markers') %>%
  add_trace(y = ~PC4, name = "PC4", mode = 'markers') %>%
  add_trace(y = ~PC5, name = "PC5", mode = 'markers') %>%
  add_trace(y = ~PC6, name = "PC6", mode = 'markers') %>%
  add_trace(y = ~PC7, name = "PC7", mode = 'markers') %>%
  add_trace(y = ~PC8, name = "PC8", mode = 'markers') %>%
  add_trace(y = ~PC9, name = "PC9", mode = 'markers') %>%
  add_trace(y = ~PC10, name = "PC10", mode = 'markers') %>%
  add_trace(y = ~PC11, name = "PC11", mode = 'markers') %>%
  add_trace(y = ~PC12, name = "PC12", mode = 'markers') %>%
  add_trace(y = ~PC13, name = "PC13", mode = 'markers') %>%
  add_trace(y = ~PC14, name = "PC14", mode = 'markers') %>%
  add_trace(y = ~PC15, name = "PC15", mode = 'markers') %>%
  add_trace(y = ~PC16, name = "PC16", mode = 'markers') %>%
  add_trace(y = ~PC17, name = "PC17", mode = 'markers') %>%
  add_trace(y = ~PC18, name = "PC18", mode = 'markers')

# En esta gráfica podemos ver cuanto de cada variable inicial explica
# cada componente principal. Si pasamos el ratón por encima y pulsamos en el 
# icono de la doble etiqueta, al pasar el raton por encima de la gráfica 
# podremos visualizar, para cada variable original, en orden de importancia,
# cuales son las componentes principales que más explican cada variable.

# Lamentablemente, como ya sospechabamos en un principio, la componente más
# explicativa para cada variable es casi siempre diferente. 

# Tras todo esto, determinamos que en esta base de datos no es buena idea 
# reducir la dimensionalidad usando PCA


