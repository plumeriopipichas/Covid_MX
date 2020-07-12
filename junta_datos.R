serie=list()

testos<-dir()[grep(".csv",dir())]

for (i in 1:length(testos)){
  print(i)
  print(testos[i])
  serie[[i]]<-read.csv(testos[i],encoding="UTF-8")
  serie[[i]]<-filter(serie[[i]],!RESULTADO==2)
  serie[[i]]$Dia_registro <- i
  print("dia reg")
  print(serie[[i]]$Dia_registro[1])
  print (dim(serie[[i]])[1])
}

datos_juntos<-rbind(serie[[1]],serie[[2]])

for (i in 3:length(testos)){
  #print(dim(datos_juntos))
  datos_juntos<-rbind(datos_juntos,serie[[i]])
}

print("dimension datos juntos")
print(dim(datos_juntos))

#datos_juntos<-unique(datos_juntos)

datos_resumidos<-select(datos_juntos,Dia_registro,ID_REGISTRO,RESULTADO,FECHA_SINTOMAS,FECHA_DEF,
                         UCI,ESTADO=ENTIDAD_RES,MUNICIPIO_RES)
datos_resumidos<-unique(datos_resumidos)

datos_resumidos$Nombre_Estado<-NA
datos_resumidos$Abreviatura<-NA

ultimo_registro<-unique(filter(datos_juntos,Dia_registro==max(datos_juntos$Dia_registro)))
ultimo_registro$Nombre_Estado<-NA
ultimo_registro$Abreviatura<-NA

claves_estados<-read.csv("../claves_edos.csv",encoding = "UTF-8")

for (k in (1:length(claves_estados$CLAVE_ENTIDAD))){
  x<-which(datos_resumidos$ESTADO==k)
  y<-which(ultimo_registro$ENTIDAD_RES==k)
  datos_resumidos$Nombre_Estado[x]<-as.character(claves_estados[k,]$ENTIDAD_FEDERATIVA)
  datos_resumidos$Abreviatura[x]<-as.character(claves_estados[k,]$ABREVIATURA)
  ultimo_registro$Nombre_Estado[y]<-as.character(claves_estados[k,]$ENTIDAD_FEDERATIVA)
  ultimo_registro$Abreviatura[y]<-as.character(claves_estados[k,]$ABREVIATURA)
}

registro_DEP_COVID<-filter(ultimo_registro,RESULTADO==1,!FECHA_DEF=="9999-99-99")

rm(i,testos,x,y,serie)
