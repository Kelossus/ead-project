crd <- read.csv(file="./class/master/ead/data/creditcard.csv", header = T, sep = ",",dec = ".",stringsAsFactors = T)
# the class to predict is the 7th column

names(crd)

# pick a reduced amount to load faster
sdata <- crd[sample(nrow(crd), 2000),]

plot(sdata[,1], sdata[,30],  col=c("green", "red")[sdata[,31]+1])

dotchart(sdata[,30], pch=19, col=c("green", "red")[sdata[,31]+1],
         xlab="Size", main="Card Fraud")
