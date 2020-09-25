para_la_estimacion<-filter(as.data.frame(decesos_registrados),Dia_Def>25,desfase>2)
estimacion_decesos<-data.frame(FECHA_DEF=unique(para_la_estimacion$FECHA_DEF))

for (k in 1:100){
  a<-filter(para_la_estimacion,desfase==k+2)
    a<-select(a,FECHA_DEF,Decesos_contados)
    estimacion_decesos<-merge(estimacion_decesos,a,by="FECHA_DEF",all.x = TRUE)
    names(estimacion_decesos)[k+1]<-paste("registros_dia_",as.character(k+2),sep="")
}


agrega_diagonal_<-function(datos,eles,inicio){
  B<-as.data.frame(datos)
  for (k in inicio:ncol(datos)){
    columna<-k
    renglon<-nrow(B)-columna+inicio
    pre<-numeric()
    pos<-numeric()
    for (j in (renglon-eles):(renglon-1)){
      peso<-1
      if (renglon%%7==j%%7){
        peso<-2*peso
      }
      if (renglon-j<8){
        peso<-2*peso
      }
      if (abs(B[j,(columna-1)]-B[renglon,columna-1])/(B[renglon,columna-1]+0.01)<0.1){
        peso<-2*peso
      }
      if (abs(B[j,(columna-1)]-B[renglon,columna-1])/(B[renglon,columna-1]+0.01)<0.05){
        peso<-2*peso
      }
      pre<-c(pre,rep(B[j,(columna-1)],peso))

      pos<-c(pos,rep(B[j,(columna)],peso))
    }
    fit<-lm(pos~pre)
    prediccion<-fit$coefficients[2]*B[renglon,columna-1]+fit$coefficients[1]
    if (is.na(prediccion)){
      print(pre)
      print(pos)
    }
    B[renglon,columna]<-max(prediccion,B[renglon,columna-1],na.rm = TRUE)
  }
  return(B)
}

ele_general<-ceiling(mean(revisar_retrasos$Retraso)+sd(revisar_retrasos$Retraso))
for (k in 3:ncol(estimacion_decesos)){
  estimacion_decesos<-agrega_diagonal_(estimacion_decesos,ele_general,k)
}

x<-which(contados_recientes$FECHA_DEF=="2020-04-12")
x<-contados_recientes$acumulados_contados[x]
a<-rep(x,nrow(estimacion_decesos))

for (k in 1:(nrow(estimacion_decesos)-1)){
  a[k+1]<-a[k]+estimacion_decesos[k+1,ncol(estimacion_decesos)]
}

estimacion_decesos$acumulados_estimados<-a

estimacion_decesos<-merge(estimacion_decesos,select(contados_recientes,FECHA_DEF,acumulados_contados),by="FECHA_DEF")

for (k in 1:nrow(estimacion_decesos)){
  estimacion_decesos$acumulados_estimados[k]<-
    max(estimacion_decesos$acumulados_estimados[k],estimacion_decesos$acumulados_contados[k])  
}

estimacion_decesos$tasa_semanal<-NA

for (k in 8:nrow(estimacion_decesos)){
  estimacion_decesos$tasa_semanal[k]<-
    estimacion_decesos$acumulados_estimados[k]/estimacion_decesos$acumulados_estimados[k-7]
}

estimacion_decesos$tasa_4dias<-NA
estimacion_decesos$tasa_suave<-NA

for (k in 5:nrow(estimacion_decesos)){
  estimacion_decesos$tasa_4dias[k]<-
    estimacion_decesos$acumulados_estimados[k]/estimacion_decesos$acumulados_estimados[k-4]
}

x<-nrow(estimacion_decesos)-1
for (j in 6:x){
  estimacion_decesos$tasa_suave[j]<-
    0.5*estimacion_decesos$tasa_4dias[j]+0.25*estimacion_decesos$tasa_4dias[j-1]+
    0.25*estimacion_decesos$tasa_4dias[j+1]
}

estimacion_decesos<-mutate(estimacion_decesos,suave_duplex=4*log(2)/log(tasa_suave))

estimacion_decesos$por_dia_suave<-estimacion_decesos$registros_dia_60
for (j in 8:nrow(estimacion_decesos)){
  estimacion_decesos$por_dia_suave[j]<-
    mean(estimacion_decesos$registros_dia_60[(j-3):j])
}

conteos_varios<-select(estimacion_decesos,FECHA_DEF,acumulados=acumulados_estimados)
conteos_varios$tipo<-as.factor("Estimados")

temp<-select(contados_recientes,FECHA_DEF,acumulados=acumulados_contados)
temp$tipo<-as.factor("Contados")

conteos_varios<-rbind(conteos_varios,temp)

temp<-select(contados_recientes,FECHA_DEF,acumulados=acumulados.anunciados)
temp$tipo<-as.factor("Anunciados")

conteos_varios<-rbind(conteos_varios,temp)
conteos_varios<-filter(conteos_varios,as.Date(FECHA_DEF)>"2020-04-11")

conteos_varios$diarios <- NA

for (k in 2:length(conteos_varios$diarios)){
  if (conteos_varios$tipo[k]==conteos_varios$tipo[k-1]){
    conteos_varios$diarios[k]<-conteos_varios$acumulados[k]-conteos_varios$acumulados[k-1]
  }
}

# #-----------------------------------------
#
rm(k,para_la_estimacion)