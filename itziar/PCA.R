# ejemplo PCA

X <- matrix(
  c(2, 1,
    5, 4,
    3, 4,
    5, 2
    ),
  nrow=4, byrow=TRUE
)
plot(X[,1], X[,2], asp=1)
text(X[,1]+0.2,  X[,2], labels=row.names(X))

pr = prcomp(X)

C1 <- pr$rotation[,1]
C2 <- pr$rotation[,2]

Y <- pr$x

plot(Y[,1], Y[,2], asp=1)
text(Y[,1]+0.2,  Y[,2], labels=row.names(Y))

# * 
colSums(Y) < 1E-10 # checking rounding error

# * 


# *
sum(var(Y)) == sum(diag(cov(Y)))
# *
eigens$values/sum(eigens$values)


##########################################
# pint.txt

pint <- read.table("itziar/pint.txt", row.names = 1)
pint  

pintN <- pint[,3:6]
pintN

pr = prcomp(pintN)

# 1. variabilidad de cada componente
diag(cov(pr$x))/sum(diag(cov(pr$x)))

# 2.primeras dos componentes
pr$rotation[,1:2]

Y = pr$x[,1:2]


plot(Y[,1], Y[,2],
     col=rainbow(length(unique(pint[,3])))[pint[,3]], asp=1)

###########################################
# 3)



P <- matrix(
  c(  2,   2,
    1.5, 0.5,
    0.7, 0.5,
    0.5, 1.5,
    0.5, 0.7,
    0.7, 0.7),
  ncol=2, byrow=TRUE
)

# a
dibujo <- matrix(
  c(  0,   0,
      0,   0,
      0,   0,
      0,   0,
      0,   0,
      
      0,   2,
      0, 1.5,
      0, 0.7,
      0, 0.5,
      0, 0.5,
      0, 0.7,
      
      2,   0,
      0.5,   0,
      0.5,   0,
      1.5,   0,
      0.7,   0,
      0.7,   0,
      
      2,   2,
      0.5, 1.5,
      0.5, 0.7,
      1.5, 0.5,
      0.7, 0.5,
      0.7, 0.7
  ),
  ncol=2, byrow=TRUE
)
plot(dibujo[,2], dibujo[,1],
     col=rainbow(6),asp=1)

#b 
Plog <- log(P)
pr <- prcomp(Plog)
pr$rotation

#c
diag(cov(P))/sum(diag(cov(P)))

Y <- pr$x
Ysorted = Y[order(Y[,2]),]
plot(Ysorted)
