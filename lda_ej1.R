dat <- matrix(
  c(2, 1, 2,
    5, 4, 1,
    3, 4, 1,
    5, 2, 2),
  ncol=3, byrow = T
)

dat1 <- matrix(
  c(5, 4,
    3, 4),
  ncol=2, byrow = T
)

dat2 <- matrix(
  c(2, 1,
    5, 2),
  ncol=2, byrow = T
)

plot(dat[,1], dat[,2], col=c("red", "blue")[dat[,3]])
lines(c(1.5, -32.5), c(-0.5, 7.5), col="red")

m1 = colMeans(dat1)
m2 = colMeans(dat2)
cov1 <- cov(dat1)
cov2 <- cov(dat2)
S <- (cov1+cov2)/2

a = solve(S)*(m2-m1)
