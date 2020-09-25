
reg_diarios_positivos<-filter(datos_resumidos,RESULTADO==1)
reg_diarios_decesos<-filter(reg_diarios_positivos,Def==1)

por_fechas<-group_by(reg_diarios_decesos,Dia_registro,FECHA_DEF)

decesos_registrados <- summarise(por_fechas,n())
names(decesos_registrados)[3]<-"Decesos_contados"

decesos_registrados<-ungroup(decesos_registrados)

decesos_registrados$desfase<-
  as.integer(as.Date(decesos_registrados$Dia_registro))-
  as.integer(as.Date(decesos_registrados$FECHA_DEF))
             #-as.integer(as.Date("2020-04-11")))
decesos_registrados<-filter(decesos_registrados,as.Date(FECHA_DEF)>"2020-03-17")

decesos_registrados$Dia_Def<-
  as.numeric(as.Date(decesos_registrados$FECHA_DEF)-as.Date("2020-03-17"))

anunciados_d<-read.csv("anunciados_conferencias.csv")

contados_recientes<-filter(decesos_registrados,as.integer(as.Date(Dia_registro))==
                             max(as.integer(as.Date(Dia_registro))))
contados_recientes<-select(contados_recientes,-Dia_registro)
contados_recientes<-merge(contados_recientes,anunciados_d,by="Dia_Def")

a<-rep(contados_recientes$Decesos_contados[1],length(contados_recientes$Decesos_contados))

for (k in 1:(length(contados_recientes$Decesos_contados)-1)){
  a[k+1]<-a[k]+contados_recientes$Decesos_contados[k+1]
}

contados_recientes$acumulados_contados<-a
contados_recientes$Delta<-
  (contados_recientes$acumulados_contados-contados_recientes$acumulados.anunciados)/
  contados_recientes$acumulados.anunciados
contados_recientes$weekday<-weekdays(as.Date(contados_recientes$FECHA_DEF))
#-------

contados_anteriores<-filter(decesos_registrados,as.integer(as.Date(Dia_registro))==
                              max(as.integer(as.Date(decesos_registrados$Dia_registro)))-1)
contados_anteriores<-select(contados_anteriores,-Dia_registro)

a<-rep(contados_anteriores$Decesos_contados[1],length(contados_anteriores$Decesos_contados))

print(length(a))
for (k in 1:(length(contados_anteriores$Decesos_contados)-1)){
  print(length(a))
  a[k+1]<-a[k]+contados_anteriores$Decesos_contados[k+1]
print(length(a))
}

contados_anteriores$acumulados_contados<-a

compara_2_dias<-merge(contados_recientes,contados_anteriores,by="Dia_Def")

# #
# # #------------retardos en informar
#
por_paciente<-group_by(filter(reg_diarios_decesos,as.Date(FECHA_DEF)>"2020-04-11"),
                       ID_REGISTRO,FECHA_DEF,Nombre_Estado,UCI)
revisar_retrasos<-summarise(por_paciente,Primer_registro=min(as.Date(Dia_registro)))
revisar_retrasos$Dia_deceso<-as.integer(as.Date(revisar_retrasos$FECHA_DEF))-as.integer(as.Date("2020-04-11"))
revisar_retrasos<-mutate(revisar_retrasos,Retraso=as.integer(as.Date(Primer_registro))-as.integer(as.Date(FECHA_DEF)))

revisar_estado<-group_by(revisar_retrasos,Nombre_Estado)
retrasos_estado<-summarise(revisar_estado,Retraso_Promedio=mean(Retraso),Mediana=median(Retraso),st_d=sd(Retraso))

rg<-ggplot(retrasos_estado,aes(Nombre_Estado,Retraso_Promedio))+geom_col(fill="orange")+
  theme(axis.text.x = element_text(angle = 90))+ylab("Retraso Promedio")+xlab("Estado")+ggtitle("Tiempo promedio para registro de decesos Covid")

rg2<-ggplot(retrasos_estado,aes(Nombre_Estado,Mediana))+geom_col(fill="purple",color="green")+
  theme(axis.text.x = element_text(angle = 90))+ylab("Mediana de los retrasos")+xlab("Estado")+ggtitle("Tiempo promedio para registro de decesos Covid")

revisar_retrasos<-ungroup(revisar_retrasos)

conteo_hoy <- filter(revisar_retrasos,Primer_registro==max(revisar_retrasos$Primer_registro))
por_estados_anunciados_hoy <- group_by(conteo_hoy,Nombre_Estado)

checar_retrasos <- function(n){
  retrasados_por_estado <- group_by(filter(conteo_hoy,Retraso>n),Nombre_Estado)
  corta<-summarise(retrasados_por_estado,cuantos=n())
  corta<-as.data.frame(ungroup(corta))
  return(corta)
}

decesos_registrados<-as.data.frame(decesos_registrados)

rm(por_estados_anunciados_hoy)

rm(a,k,contados_anteriores,revisar_estado)
