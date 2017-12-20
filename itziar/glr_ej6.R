dat <- read.csv("itziar/edss.dat", header=T, dec=",", sep = "\t")

glr <- glm(EDSS..5.yr.a.o..~Age.at.onset+SEX, data=dat)

summary(glr)

glr$coefficients

eval <- function(age, sex, coef){
  return(coef[1] + coef[1]*age + coef[2]*sex)
}
