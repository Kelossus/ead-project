data("AirPassengers")
AP <- AirPassengers
class(AP)
plot(AP, ylab="Pasajeroes (miles")

length(AP); start(AP); end(AP);frequency(AP)

layout(1:3)
plot(AP, ylab="Pasajeroes (miles")
plot(aggregate(AP))
boxplot((AP~cycle(AP)))
length(aggregate(AP))

layout(1:3)
plot(AP, ylab="Pasajeroes (miles")
largo <- 12
nmedias <- length(AP)-largo+1
vmedias <- vector("numeric", length=length(AP))
vmedias[] <- NA

for (i in 1:nmedias){
  vmedias[largo/2+i] <- mean(AP[i:(i+largo-1)])
}

plot(ts(vmedias))

vestac <- AP/vmedias

plot(vestac)

aggVestac <- vector("numeric", length=largo)
for (i in 1:largo){
  aggVestac[i] <- mean(vestac[seq(i+largo/2,
                                  nmedias+largo/2, largo)])
}
aggVestac <- c(aggVestac[(largo/2+1):largo],
               aggVestac[1:(largo/2)])
print(aggVestac)


###############################

plot(decompose(AP, type = "additive"))

plot(decompose(AP, type = "multiplicative"))

###############################

require(TSdist)

variasDist <- function(ser1, ser2) {
  print(EuclideanDistance(ser1, ser2))
  print(DTWDistance(ser1, ser2))
  print(ManhattanDistance(ser1, ser2))
  print(FourierDistance(ser1, ser2))
  print(CDMDistance(ser1, ser2))
  print(EDRDistance(ser1, ser2, 0.1, length(ser1)-1))
  
}

data(example.series1)
data("example.series2")
layout(1:2)
plot(ts(example.series1))
plot(ts(example.series2))
variasDist(example.series1, example.series2)


desplaz <- 20
example.series1b <- c(example.series1[(length(example.series1)-
                                         desplaz+1):length(example.series1)],
                      example.series1[1:length(example.series1-desplaz)])
layout(1:2)
plot(ts(example.series1))
plot(ts(example.series1b))
variasDist(example.series1, example.series1b)
