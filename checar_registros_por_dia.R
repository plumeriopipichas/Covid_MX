library(dplyr)

x<-which(datos_resumidos$FECHA_DEF=="9999-99-99")
datos_resumidos$Def<-0
datos_resumidos$Def[-x]<-1

reg_diarios_positivos<-filter(datos_resumidos,RESULTADO==1)
reg_diarios_decesos<-filter(reg_diarios_positivos,Def==1)

por_fechas<-group_by(reg_diarios_decesos,Dia_registro,FECHA_DEF)

decesos_registrados <- summarise(por_fechas,n())
names(decesos_registrados)[3]<-"Decesos_contados"

decesos_registrados<-ungroup(decesos_registrados)

decesos_registrados$desfase<-
  decesos_registrados$Dia_registro-as.integer(as.Date(decesos_registrados$FECHA_DEF)-
                                                as.Date("2020-04-11"))
decesos_registrados<-filter(decesos_registrados,as.Date(FECHA_DEF)>"2020-03-17")

decesos_registrados$Dia_Def<-
  as.numeric(as.Date(decesos_registrados$FECHA_DEF)-as.Date("2020-03-17"))


anunciados_d<-read.csv("anunciados_conferencias.csv")

contados_recientes<-filter(decesos_registrados,Dia_registro==max(decesos_registrados$Dia_registro))
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

contados_anteriores<-filter(decesos_registrados,Dia_registro==max(decesos_registrados$Dia_registro)-1)
contados_anteriores<-select(contados_anteriores,-Dia_registro)

a<-rep(contados_anteriores$Decesos_contados[1],length(contados_anteriores$Decesos_contados))

for (k in 1:(length(contados_anteriores$Decesos_contados)-1)){
  a[k+1]<-a[k]+contados_anteriores$Decesos_contados[k+1]
}

contados_anteriores$acumulados_contados<-a

compara_2_dias<-merge(contados_recientes,contados_anteriores,by="Dia_Def")

rm(a,x,k)
