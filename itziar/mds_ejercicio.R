bhattacharyya <- function(p, q){
  return(acos(sum(sqrt(p*q))))
}


dat <- matrix(
  c(0.36, 0.21, 0.23, 0.20,
    0.66, 0.18, 0.11, 0.05,
    0.01, 0.24, 0.62, 0.13,
    0.43, 0.38, 0.08, 0.11,
    0.16, 0.07, 0.09, 0.68,
    0.22, 0.37, 0.25, 0.16),
  ncol=4, byrow=TRUE
)


row.names(dat) <- c("1", "2", "3", "4", "5", "6")
colnames(dat) <- c("M1", "M2", "M3", "M4")

dists <- c()
n <- length(dat[,1])

for (i in 1:n){
  for (j in 1:n){
    dists <- c(dists, bhattacharyya(dat[i,], dat[j,]))
  }
}

distm <- matrix(dists, ncol=n)

cmds <- cmdscale(distm)

plot(cmds)



H <- diag(1, nrow=n) - matrix(1, nrow=n, ncol=n)/n

A <- -1/2*(distm^2)

B <- H %*% A %*% H


eigen(B)

