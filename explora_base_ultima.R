# 
# 
# 
# #------------retardos en informar

por_paciente<-group_by(filter(reg_diarios_decesos,as.Date(FECHA_DEF)>"2020-04-11"),
                       ID_REGISTRO,FECHA_DEF,Nombre_Estado,UCI)
revisar_retrasos<-summarise(por_paciente,Primer_registro=min(Dia_registro))
revisar_retrasos$Dia_deceso<-as.integer(as.Date(revisar_retrasos$FECHA_DEF))-as.integer(as.Date("2020-04-11"))
revisar_retrasos<-mutate(revisar_retrasos,Retraso=Primer_registro-Dia_deceso)

revisar_estado<-group_by(revisar_retrasos,Nombre_Estado)
retrasos_estado<-summarise(revisar_estado,Retraso_Promedio=mean(Retraso),Mediana=median(Retraso),st_d=sd(Retraso))

rg<-ggplot(retrasos_estado,aes(Nombre_Estado,Retraso_Promedio))+geom_col(fill="orange")+
  theme(axis.text.x = element_text(angle = 90))+ylab("Retraso Promedio")+xlab("Estado")+ggtitle("Tiempo promedio para registro de decesos Covid")

rg2<-ggplot(retrasos_estado,aes(Nombre_Estado,Mediana))+geom_col(fill="purple",color="green")+
  theme(axis.text.x = element_text(angle = 90))+ylab("Mediana de los retrasos")+xlab("Estado")+ggtitle("Tiempo promedio para registro de decesos Covid")

# #--------------------------------
ultimo_registro$Def<-0
x<-which(!ultimo_registro$FECHA_DEF=="9999-99-99")
ultimo_registro$Def[x]<-1

x<-which(ultimo_registro$Abreviatura=="MC")
ultimo_registro$Nombre_Estado[x]<-"EDOMEX"


rm(x)

# #-------------pega municipios en base grande reciente y explora con eso

#todos_ultima<-filter(datos_juntos,Dia_registro==max(datos_juntos$Dia_registro))


por_municipio<-group_by(todos_ultima,ENTIDAD_RES,MUNICIPIO_RES,Nombre_Estado,Municipio)
municipales<-summarise(por_municipio,UCI=length(which(UCI==1)),positivos=length(which(RESULTADO==1)),decesos=
                      length(which(!FECHA_DEF=="9999-99-99")))

temp<-mortalidad.municipios.2018
names(temp)<-c("ENTIDAD_RES","MUNICIPIO_RES","municipio","Defunciones_2018")
municipales <- merge(municipales,temp)
rm(temp)

municipales$porc_aumento<-municipales$decesos/municipales$Defunciones_2018
municipales$porc_mortalidad<-municipales$decesos/municipales$positivos

alto_UCI<-filter(municipales,UCI>8)
alto_positivo<-filter(municipales,positivos>50)

gruci1<-ggplot(alto_UCI,aes(log(positivos+1),log(UCI+1)))+geom_point()+geom_smooth(method="lm")
gruci2<-ggplot(alto_positivo,aes(log(positivos+1),log(UCI+1)))+geom_point()+geom_smooth(method="lm")

delta_DEP1<-ggplot(alto_UCI,aes(log(Defunciones_2018+1),log(decesos+1)))+geom_point()+
  geom_smooth(method="lm")
delta_DEP2<-ggplot(alto_positivo,aes(log(Defunciones_2018+1),log(decesos+1)))+geom_point()+geom_smooth(method="lm")+
  geom_smooth(method="lm")

DEP_vs_posit1<-ggplot(alto_UCI,aes(log(positivos),log(decesos)))+geom_point()+
  geom_smooth(method="lm")
DEP_vs_posit2<-ggplot(alto_positivo,aes(log(positivos),log(decesos)))+geom_point()+geom_smooth(method="lm")+
  geom_smooth(method="lm")+ylab("Defunciones estimadas. Escala log.")+xlab("Positivos confirmados. Escala log.")

pcmort_vs_psaumento <- ggplot(alto_positivo,aes(porc_mortalidad,porc_aumento))+geom_point(alpha=0.5)+geom_smooth(method="lm")+
  geom_smooth(method="lm")+xlab("Defunciones / Positivos")+ylab("Impacto por Covid 19 en la mortalidad general")

xmorts_DEP<-pcmort_vs_psaumento+geom_label_repel(data=filter(alto_positivo,porc_aumento>0.15 | porc_mortalidad>0.29),
                                              aes(label=municipio),box.padding = 2,
                                              point.padding=0.3)+
  ggtitle("Mortalidad por covid a nivel municipal.       Municipios con más de  50 positivos confirmados")

explora_morts_cg<-pcmort_vs_psaumento+geom_label_repel(data=filter(alto_positivo,Defunciones_2018>3850),
                                              aes(label=municipio),box.padding = 1.5,point.padding=0.1)+ggtitle("Mortalidad por covid a nivel municipal")

delta_DEP_edos<-list()
DEP_vs_posit_edos<-list()
pcmorts_edos<-list()

for (k in 1:32){
  nombre<-filter(alto_positivo,ENTIDAD_RES==k)$Nombre_Estado[1]
  delta_DEP_edos[[k]]<-delta_DEP2+geom_label_repel(data=filter(alto_positivo,ENTIDAD_RES==k),
                                                   aes(label=municipio),box.padding = 1,point.padding=0.1)+
    ggtitle(nombre)
  DEP_vs_posit_edos[[k]]<-DEP_vs_posit2+geom_label_repel(data=filter(alto_positivo,ENTIDAD_RES==k),
                                                   aes(label=municipio),box.padding = 1,point.padding=0.1)+
    ggtitle(nombre)
  pcmorts_edos[[k]]<-pcmort_vs_psaumento+geom_label_repel(data=filter(alto_positivo,ENTIDAD_RES==k),
                                                         aes(label=municipio),box.padding = 0.85)+
    ggtitle(nombre)

}

conteo_hoy<-filter(as.data.frame(revisar_retrasos),Primer_registro==max(revisar_retrasos$Primer_registro))
por_estados_anunciadoshoy<-group_by(conteo_hoy,Nombre_Estado)
retrasados_por_estado<-group_by(filter(conteo_hoy,Retraso>15),Nombre_Estado,UCI)
checar_retrasos<-summarise(retrasados_por_estado,cuantos=n())

rm(nombre)
# #----------------------------------
# 
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



#Para explorar el CFR 

por_sintomas_edos<-group_by(ultimo_registro,FECHA_SINTOMAS,Abreviatura,ENTIDAD_RES,Nombre_Estado)
CFR_todos<-summarise(por_sintomas_edos,cuantos=n(),DEP=sum(Def))

CFR_estados<-function(estados){
  temp<-filter(CFR_todos,ENTIDAD_RES%in%estados)
  temp$CFR_semanal<-NA
  for (k in 8:(nrow(temp))){temp$CFR_semanal[k]<-sum(temp$DEP[(k-7):k])/sum(temp$cuantos[(k-7):(k-1)])}
  temp<-filter(temp,!is.na(CFR_semanal))%>%filter(as.Date(FECHA_SINTOMAS)>"2020-04-5" & as.Date(FECHA_SINTOMAS)<as.Date(hoy)-30)
  
  return(temp)
}

por_sintomas<-group_by(ultimo_registro,FECHA_SINTOMAS)
ver_CFR<-summarise(por_sintomas,cuantos=n(),DEP=sum(Def))
ver_CFR$CFR_semanal<-NA
for (k in 8:(nrow(ver_CFR))){ver_CFR$CFR_semanal[k]<-sum(ver_CFR$DEP[(k-7):(k-1)])/sum(ver_CFR$cuantos[(k-7):(k-1)])}
ver_CFR<-filter(ver_CFR,!is.na(CFR_semanal))%>%filter(as.Date(FECHA_SINTOMAS)>"2020-03-22"&as.Date(FECHA_SINTOMAS)<as.Date(hoy)-30)

CFR_todos<-group_by(CFR_todos,Nombre_Estado,Abreviatura,ENTIDAD_RES)
totales_por_estado<-summarise(CFR_todos,positivos=sum(cuantos),DEP=sum(DEP))
totales_por_estado<-mutate(totales_por_estado,Porcentaje_CFR=100*DEP/positivos)
ungroup(CFR_todos)