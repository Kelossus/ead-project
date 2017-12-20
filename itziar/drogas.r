library(smacof)
dat <- read.table("itziar/drogas.dat", header = TRUE)
row.names(dat) <- names(dat)

# conversion de similaridad a disimilaridad
# dat <- max(dat) - dat
dat <- ((max(dat) + 1)/(dat + 1) - 1)/68

n <- length(dat[,1])


H <- diag(1, nrow=n) - matrix(1, nrow=n, ncol=n)/n

A <- -1/2*(dat^2)

B <- H %*% A %*% H


cumsum(abs(eigen(B)$values/sum(abs(eigen(B)$values))))

scaled <- cmdscale(dat,  k = 2, x.ret = TRUE)
scaled$GOF

plot(scaled$points[,1], scaled$points[,2])
text(scaled$points[,1], scaled$points[,2], labels = names(dat))

ratio.mds = mds(dat, type="ordinal")
ratio.mds$stress

plot(ratio.mds$conf[,1], ratio.mds$conf[,2])
text(ratio.mds$conf[,1], ratio.mds$conf[,2], labels = names(dat))

plot(ratio.mds, plot.type = "Shepard")
plot(ratio.mds, plot.type = "stressplot")
