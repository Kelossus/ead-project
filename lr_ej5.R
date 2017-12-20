library(MASS)
data("Animals")

lr <- lm(body~brain, data=Animals)

plot(lr$fitted.values, residuals(lr))
text(lr$fitted.values, residuals(lr), row.names(Animals))

loglr <- lm(body~brain, data=log(Animals))

plot(loglr$fitted.values, residuals(loglr))
text(loglr$fitted.values, residuals(loglr), row.names(Animals))

plot(lr)
plot(loglr)


data3 <- data.frame(log(Animals), 
                    as.factor(as.numeric(row.names(Animals) %in% c("Brachiosaurus", "Triceratops", "Dipliodocus"))))
colnames(data3) <- c(colnames(Animals), "esDinosaurio")


glr <- glm(brain~body+esDinosaurio, data=data3)

eval <- function(a, b, coef){
  return(coef[1] + coef[2]*a + coef[3]*b)
}

plot(log(Animals))
abline(glr$coefficients[1], glr$coefficients[2], col="red")
text(0, eval(0, 0, glr$coefficients)+1, labels = "normies", col="violet")
text(5, eval(5, 1, glr$coefficients)+1, labels = "dinosauros", col="darkgreen")

abline(glr$coefficients[1]+glr$coefficients[3], glr$coefficients[2], col="green")

plot(glr)

