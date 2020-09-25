acumulados <- data.frame(dia=names(estados_covid)[4:length(names(estados_covid))])

c<-0
k <- dim(estados_covid)[2]

for (estado in estados_covid$nombre[1:length(estados_covid$nombre)]){
  c<- c+1
  x<-numeric()
  for (dia in 4:k){
    siguiente <- sum(estados_covid[c,4:dia])
    x<-append(x,siguiente)
  }
  acumulados<-cbind(acumulados,x)
  names(acumulados)[c+1]<-estado
}

acumulados$dia<-as.character(acumulados$dia)
acumulados$dia<-substr(acumulados$dia,2,nchar(acumulados$dia))

tasas_4_dias<-data.frame(Dia=1:(dim(acumulados)[1]-18))
tasas_semanales<-data.frame(Dia=1:(dim(acumulados)[1]-20))

for (k in names(acumulados)[-1]){
  x<-numeric()
  y<-numeric()
  for (m in 8:(dim(acumulados)[1])){
    if (acumulados[m-4,k]>25){
      tasa <- round(acumulados[m,k]/acumulados[m-4,k],3)
      x<-append(x,tasa)
    }
    if (acumulados[m-7,k]>15){
      tasa <- round(acumulados[m,k]/acumulados[m-7,k],3)
      y<-append(y,tasa)
    }
  }
  dif<-length(tasas_4_dias$Dia)-length(x)
  x<-c(x,rep(NA,dif))
  tasas_4_dias<-cbind(tasas_4_dias,x)
  names(tasas_4_dias)[length(names(tasas_4_dias))]<-gsub(" ",".",k)
  dif<-length(tasas_semanales$Dia)-length(y)
  y<-c(y,rep(NA,dif))
  tasas_semanales<-cbind(tasas_semanales,y)
  names(tasas_semanales)[length(names(tasas_semanales))]<-gsub(" ",".",k)
}


View(tasas_4_dias)

rm(x,k,m,siguiente,c)