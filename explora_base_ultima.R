
# # # #-------------pega municipios en base grande reciente y explora con eso
# # 

por_municipio<-group_by(filter(ultimo_registro,!MUNICIPIO_RES==999),ENTIDAD_RES,MUNICIPIO_RES,Nombre_Estado)
municipales<-summarise(por_municipio,UCI=length(which(UCI==1)),positivos=length(which(RESULTADO==1)),decesos=
                      length(which(!FECHA_DEF=="9999-99-99" & RESULTADO==1)))

temp<-mortalidad.municipios.2018
names(temp)<-c("ENTIDAD_RES","MUNICIPIO_RES","Nombre_municipio","Defunciones_2018")
municipales <- merge(municipales,temp)
rm(temp)

municipales$porc_aumento<-municipales$decesos/municipales$Defunciones_2018
municipales$CFR_municipal<-municipales$decesos/municipales$positivos

alto_positivo<-filter(municipales,positivos>50)

CFR_municipal_vs_psaumento <- ggplot(alto_positivo,aes(CFR_municipal,porc_aumento))+geom_point(alpha=0.25)+
  geom_smooth(method="lm")+xlab("CFR por municipio (Defunciones/Positivos)")+ylab("Impacto por Covid 19 en la mortalidad general")

xmorts_DEP<-CFR_municipal_vs_psaumento+
 geom_label_repel(data=filter(alto_positivo,porc_aumento>0.19 | CFR_municipal>0.31),
                   aes(label=Nombre_municipio),box.padding = 2,point.padding=0.3)+
  ggtitle("Mortalidad por covid a nivel municipal.       Municipios con más de  50 positivos confirmados")

morts_DEP_mg<-CFR_municipal_vs_psaumento+geom_label_repel(data=filter(alto_positivo,Defunciones_2018>3850),
                                              aes(label=Nombre_municipio),box.padding = 1.5,point.padding=0.1)+ggtitle("Mortalidad por covid a nivel municipal")


pcmorts_edos<-list()
 
for (k in 1:32){
  nombre<-filter(alto_positivo,ENTIDAD_RES==k)$Nombre_Estado[1]
  pcmorts_edos[[k]]<-CFR_municipal_vs_psaumento+geom_label_repel(data=filter(alto_positivo,ENTIDAD_RES==k),
                                                          aes(label=Nombre_municipio),box.padding = 0.85)+ggtitle(nombre)
}

#-------------

# # # #----------------------------------
# # # 
municipio_DEP<-function(estado,municipio){
  x<-which(clave_municipal$CLAVE_MUNICIPIO==municipio & clave_municipal$CLAVE_ENTIDAD==estado)
  nm<-clave_municipal$MUNICIPIO[x]
  temp<-filter(ultimo_registro,ENTIDAD_RES==estado,MUNICIPIO_RES==municipio,Def==1)
  temp<-group_by(temp,FECHA_DEF)
  temp<-summarise(temp,DEP_dia=n())
  g_temp<-ggplot(temp,aes(as.Date(FECHA_DEF),DEP_dia))+xlab("Fecha de defuncion")+
    ylab("Numero de casos")+ggtitle(nm)+geom_col(fill="#243591")
  return(g_temp)
}

municipio_sintomas<-function(estado,municipio){
  x<-which(clave_municipal$CLAVE_MUNICIPIO==municipio & clave_municipal$CLAVE_ENTIDAD==estado)
  nm<-clave_municipal$MUNICIPIO[x]
  temp<-filter(ultimo_registro,ENTIDAD_RES==estado,MUNICIPIO_RES==municipio)
  temp<-group_by(temp,FECHA_SINTOMAS,Def)
  temp<-summarise(temp,cuantos=n())
  g_temp<-ggplot(temp,aes(as.Date(FECHA_SINTOMAS),cuantos,color=as.factor(Def)))+
    geom_col()+xlab("Inicio de sintomas")+
    ylab("Numero de pacientes registrados")+ggtitle(nm)+theme(legend.position = "none")
  return(g_temp)
}

municipio_ingreso<-function(estado,municipio){
  x<-which(clave_municipal$CLAVE_MUNICIPIO==municipio & clave_municipal$CLAVE_ENTIDAD==estado)
  nm<-clave_municipal$MUNICIPIO[x]
  temp<-filter(ultimo_registro,ENTIDAD_RES==estado,MUNICIPIO_RES==municipio)
  temp<-group_by(temp,FECHA_INGRESO)
  temp<-summarise(temp,cuantos=n())
  g_temp<-ggplot(temp,aes(as.Date(FECHA_INGRESO),cuantos))+
    geom_col(fill="brown")+xlab("Fecha de registro")+
    ylab("Numero de casos")+ggtitle(nm)
  return(g_temp)
}

estado_DEP<-function(estado){
  x<-which(claves_estados$CLAVE_ENTIDAD==estado)
  nm<-claves_estados$ENTIDAD_FEDERATIVA[x]
  temp<-filter(ultimo_registro,ENTIDAD_RES==estado,Def==1)
  temp<-group_by(temp,FECHA_DEF)
  temp<-summarise(temp,DEP_dia=n())
  g_temp<-ggplot(temp,aes(as.Date(FECHA_DEF),DEP_dia))+xlab("Fecha de defuncion")+
    ylab("Numero de casos")+ggtitle(nm)+geom_col(fill="#243591")
  return(g_temp)
}

estado_sintomas<-function(estado){
  x<-which(claves_estados$CLAVE_ENTIDAD==estado)
  nm<-claves_estados$ENTIDAD_FEDERATIVA[x]
  temp<-filter(ultimo_registro,ENTIDAD_RES==estado)
  temp<-group_by(temp,FECHA_SINTOMAS,Def)
  temp<-summarise(temp,cuantos=n())
  g_temp<-ggplot(temp,aes(as.Date(FECHA_SINTOMAS),cuantos,color=as.factor(Def)))+
    geom_col()+xlab("Inicio de sintomas")+
    ylab("Numero de pacientes registrados")+ggtitle(nm)+theme(legend.position = "none")
  return(g_temp)
}

estado_ingreso<-function(estado){
  x<-which(claves_estados$CLAVE_ENTIDAD==estado)
  nm<-claves_estados$ENTIDAD_FEDERATIVA[x]
  temp<-filter(ultimo_registro,ENTIDAD_RES==estado)
  temp<-group_by(temp,FECHA_INGRESO)
  temp<-summarise(temp,cuantos=n())
  g_temp<-ggplot(temp,aes(as.Date(FECHA_INGRESO),cuantos))+
    geom_col(fill="brown")+xlab("Fecha de registro")+
    ylab("Numero de casos")+ggtitle(nm)
  return(g_temp)
}

# #Para explorar el CFR 

por_sintomas_edos<-group_by(ultimo_registro,FECHA_SINTOMAS,Abreviatura,ENTIDAD_RES,Nombre_Estado)

CFR_estados<-function(estados){
      temp<-list()
      for (j in estados){
          temp[[j]]<-filter(ungroup(CFR_todos),ENTIDAD_RES==j)
          temp[[j]]$CFR_semanal<-NA
          for (k in 8:(nrow(temp[[j]])-1)){
                tempo<-filter(temp[[j]],as.Date(FECHA_SINTOMAS)<as.Date(temp[[j]][k+1, ]$FECHA_SINTOMAS) &
                         as.Date(FECHA_SINTOMAS)>as.Date(temp[[j]][k, ]$FECHA_SINTOMAS)-7)
                temp[[j]]$CFR_semanal[k]<-sum(tempo$DEP)/sum(tempo$cuantos)
          }
      }
      CFR_estos<-temp[[estados[1]]]

      print(estados[1])
      for (j in estados[-1]){
          CFR_estos<-rbind(CFR_estos,temp[[j]])
       }

      CFR_estos<-filter(CFR_estos,!is.na(CFR_semanal))%>%filter(as.integer(as.Date(FECHA_SINTOMAS))>as.integer(as.Date("2020-04-5")) &
                                                               as.integer(as.Date(FECHA_SINTOMAS))<as.integer(as.Date(hoy))-15)

      return(as.data.frame(CFR_estos))
}

CFR_todos<-CFR_estados(c(1:32))
por_sintomas<-group_by(ultimo_registro,FECHA_SINTOMAS)
ver_CFR<-summarise(por_sintomas,cuantos=n(),DEP=sum(Def))
ver_CFR$CFR_semanal<-NA
for (k in 8:(nrow(ver_CFR))){ver_CFR$CFR_semanal[k]<-sum(ver_CFR$DEP[(k-7):(k-1)])/sum(ver_CFR$cuantos[(k-7):(k-1)])}
ver_CFR<-filter(ver_CFR,!is.na(CFR_semanal))%>%filter(as.Date(FECHA_SINTOMAS)>"2020-03-22"&as.Date(FECHA_SINTOMAS)<as.Date(hoy)-30)

CFR_todos<-group_by(CFR_todos,Nombre_Estado,Abreviatura,ENTIDAD_RES)
totales_por_estado<-summarise(CFR_todos,positivos=sum(cuantos),DEP=sum(DEP))
totales_por_estado<-mutate(totales_por_estado,Porcentaje_CFR=100*DEP/positivos)

totales_por_estado<-ungroup(totales_por_estado)
mayor_CFR<-arrange(totales_por_estado,desc(Porcentaje_CFR))$ENTIDAD_RES[1:3]
menor_CFR<-arrange(totales_por_estado,Porcentaje_CFR)$ENTIDAD_RES[1:3]
extremos_CFR<-arrange(totales_por_estado,Porcentaje_CFR)$ENTIDAD_RES[c(1:2,(nrow(totales_por_estado)-1):nrow(totales_por_estado))]



