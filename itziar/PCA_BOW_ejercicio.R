BoW <- read.table("itziar/dokumentuetahitzak.txt")

BoW <- as.matrix(BoW)
BoW <- t(BoW)

colnames(BoW) <- c("Human", "Interface", "Computer",
                    "User", "System", "Response",
                    "Time", "EPS", "Survey",
                    "Trees", "Graph", "Minors")

row.names(BoW) <- c("D1", "D2", "D3",
                    "D4", "D5", "D6",
                    "D7", "D8", "D9")

pr = prcomp(BoW)

PC = pr$rotation[,1:2]

Y = pr$x[,1:2]


# 0 <- interaccíon entre persona y computador  1 <- teoría de grafos
cl <- c(1, 1, 1, 1, 1, 2, 2, 2, 2)

plot(Y[,1], Y[,2], col=c("green",  "cyan")[cl], bg=c("green",  "cyan")[cl])



# New doc = "Graph theory with applications to engineering and  computer science"
doc0_BoW <- matrix(c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0), nrow=1)

pr$center

doc0 <- (doc0_BoW - pr$center) %*% PC

points(doc0[,1], doc0[,2], col="red")


knn(Y, doc0, cl=cl, k=2)

# No se puede decidir, ya que en nuestro vocabulario solo dos de las palabras de la 
# nueva frase existen anteriormente, y cada palabra solo sale en textos que pertenecen
# a una clase o a otra.
