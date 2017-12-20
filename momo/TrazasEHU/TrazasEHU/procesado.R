# setwd("./TrazasEHU/")

######### Organizamos los datos por dias ##############

diaxsem = 7;
minxdia = 24*60;

nsemanas = 33;
ndias = nsemanas*diaxsem;

formateddata<-matrix(nrow=ndias,ncol=minxdia)
for(i in 1:nsemanas){
  if(i<10){
    data<-read.table(paste("accesslog.week0",i,".reqmin",sep=""))
  }
  else{
    data<-read.table(paste("accesslog.week",i,".reqmin",sep=""))
  }

  for(j in 1:diaxsem){  
    formateddata[(i-1)*diaxsem+j,]<-data[((j-1)*minxdia+1):(j*minxdia),2]
  }

}


##################Las dibujamos en una hoja#################


maxx=max(formateddata)
minn=min(formateddata)

par(mfrow=c(diaxsem,diaxsem),oma=c(0,0,0,0),mar=c(0,0,0,0))

for(i in 1:ndias){
  plot(formateddata[i,],xaxt="n",yaxt="n",type="l",ylim=c(minn,maxx))
}

############ PLOT COLOREADO #################

clases<-c()

#Abril - Noviembre
for(i in c(1:ndias)){
  if (i%%7==6) { # Sabados
    clases[i]<-2
  } else if (i%%7==0) { # Domingos
    clases[i]<-3
  } else {         # Laborables
    clases[i]<-1
  }
}

# Agosto
for(i in c(123:165)){
    clases[i]<-4
}

# Festivos
clases[1]  <-4   # 1 abril
clases[31] <-4  # 1 Mayo
clases[116]<-4 # 25 Julio
clases[122]<-4 # 31 Julio
clases[208]<-4 # 25 Octubre
clases[166]<-4 # 1 Septiembre, domingo


maxx=max(formateddata)
minn=min(formateddata)
par(mfrow=c(diaxsem,diaxsem),oma=c(0,0,0,0),mar=c(0,0,0,0))
for(i in 1:ndias){
  plot(formateddata[i,],xaxt="n",yaxt="n",type="l",ylim=c(minn,maxx),col=clases[i])
}


############ PREDICCION #################

