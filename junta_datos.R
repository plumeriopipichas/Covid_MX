serie=list()

#owid_covid<-read.csv(file="owid-covid-data.csv")

testos<-dir()[grep(".csv",dir())]

mes<-substr(testos,3,4)
dia<-substr(testos,5,6)
fecha<-paste(paste("2020",mes,sep="-"),dia,sep="-")

for (i in 1:length(testos)){
  print (i)
  print(fecha[i])
  serie[[i]]<-read.csv(testos[i],encoding="UTF-8")
  serie[[i]]$Dia_registro <- fecha[i]
  serie[[i]]$Nombre_Estado<-NA
  serie[[i]]$Abreviatura<-NA
  x<-which(serie[[i]]$FECHA_DEF=="9999-99-99")
  serie[[i]]$Def<-0
  serie[[i]]$Def[-x]<-1 
  if (i == length(testos)){
    ultimo_completo<-serie[[i]]
  }
  serie[[i]]<-filter(serie[[i]],!RESULTADO==2)%>%select(Dia_registro,ID_REGISTRO,RESULTADO,FECHA_SINTOMAS,FECHA_DEF,
            UCI,ENTIDAD_RES,MUNICIPIO_RES,SEXO,EDAD,Nombre_Estado,Abreviatura,Def)
}

#este se usa cuando se reini]cian los datos
# datos_resumidos<-serie[[1]]

#for (i in 2:length(serie)){
#   datos_resumidos<-rbind(datos_resumidos,serie[[i]])
#}

#este se usa para agregar datos recientes

for (i in 1:length(serie)){
    print(i)
    datos_resumidos<-rbind(datos_resumidos,serie[[i]])
}

rm(dia,mes,fecha,i,testos,serie)

print(dim(datos_resumidos))
#claves_estados<-read.csv("../claves_edos.csv",encoding = "UTF-8")

for (k in (1:length(claves_estados$CLAVE_ENTIDAD))){
  x<-which(datos_resumidos$ENTIDAD_RES==k)
  y<-which(ultimo_completo$ENTIDAD_RES==k)
  datos_resumidos$Nombre_Estado[x]<-as.character(claves_estados[k,]$ENTIDAD_FEDERATIVA)
  datos_resumidos$Abreviatura[x]<-as.character(claves_estados[k,]$ABREVIATURA)
  ultimo_completo$Nombre_Estado[y]<-as.character(claves_estados[k,]$ENTIDAD_FEDERATIVA)
  ultimo_completo$Abreviatura[y]<-as.character(claves_estados[k,]$ABREVIATURA)
}
print("D")

x<-which(ultimo_completo$Abreviatura=="MC")
ultimo_completo$Nombre_Estado[x]<-"EDOMEX"

ultimo_registro<-filter(ultimo_completo,!RESULTADO==2)

rm(k,x,y)
