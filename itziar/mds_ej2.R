

dat <- matrix(
  c(
    0, 1, 0,
    1, 1, 0,
    0, 1, 1,
    0, 0, 1,
    1, 0, 0
  ), ncol=3, byrow=TRUE
)
colnames(dat) <- c("Piedra", "Bronce", "Hierro")

row.names(dat) <- c(1, 2, 3, 4, 5)


distm = as.matrix(dist(dat, method="binary", diag=TRUE, upper=TRUE))
n <- 5


H <- diag(1, nrow=n) - matrix(1, nrow=n, ncol=n)/n

A <- -1/2*(distm^2)

B <- H %*% A %*% H


x = eigen(B)$vectors[,1:2]%*% sqrt(diag(eigen(B)$values[1:2], nrow=2))


all.equal(x, cmdscale(distm), tolerance = 4e-1)

plot(x)
