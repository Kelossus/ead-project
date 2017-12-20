
Altura <- c(169.6, 166.8, 157.1, 181.1, 158.4, 165.6, 166.7, 156.5, 168.1, 165.3)
Peso <-   c( 71.2,  58.2,  56.0,  64.5,  53.0,  52.4,  56.8,  49.2,  55.6,  77.8)


lr <- lm(Peso ~ Altura)

summary(lr)

Rsq <- 159.95/(159.95+572.01)

plot(Peso ~ Altura)
abline(lr, col="red")


abs(mean(Peso) - (lr$coefficients[1] + lr$coefficients[2]*mean(Altura)))  < 1e-13

sum((Peso - mean(Peso))^2)

sum(((lr$coefficients[1] + lr$coefficients[2]*Altura) - mean(Peso))^2) + 

sum((Peso - (lr$coefficients[1] + lr$coefficients[2]*Altura))^2)



