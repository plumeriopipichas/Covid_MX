library(RColorBrewer)



compara_acumulados<-ggplot(contados_recientes,aes(acumulados.anunciados,acumulados_contados))+
  geom_point()+geom_line()+geom_segment(x=0,y=0,xend=max(contados_recientes$acumulados_contados),
                                        yend=max(contados_recientes$acumulados_contados),size=2,color="green")+
  xlab("Decesos acumulados anunciados en conferencia vespertina")+ylab("Decesos registrados del dìa en base actualizada")+
  ggtitle(paste("Decesos acumulados por  COVID-19. México.",as.character(hoy)))

# acumulados_dos_conteos_log<-ggplot(contados_recientes)+geom_point(aes(Dia_Def,log(acumulados.anunciados)),color="blue")+
#   geom_point(aes(Dia_Def,log(acumulados_contados)),color="green")+
#   geom_line(aes(Dia_Def,log(acumulados.anunciados)),color="blue")+
#   geom_line(aes(Dia_Def,log(acumulados_contados)),color="green")+
#   xlab("Días transcurridos desde el 18 de marzo")+
#   ylab("Acumulado de defunciones.")

acumulados_dos_conteos<-ggplot(filter(acumulados_varios_,!tipo=="Estimados"),aes(as.Date(FECHA_DEF),acumulados,color=tipo))+
  geom_point(size=2.3)+geom_line()+xlab("Fecha")+ggtitle(paste("Decesos acumulados COVID al ",hoy))

acumulados_tres_conteos<-ggplot(acumulados_varios_,aes(as.Date(FECHA_DEF),acumulados,color=tipo))+
  geom_point(size=2.3)+geom_line()+ggtitle(paste("Decesos acumulados por COVID",hoy))+xlab("Fecha")+ylab("Decesos")

# acumulados_tres_conteos_log<-ggplot(acumulados_varios_,aes(as.Date(FECHA_DEF),log(acumulados),color=tipo))+
#   geom_point(size=2.1)+geom_line()+ggtitle(paste("Decesos acumulados por COVID en México. Escala log.",hoy))+xlab("Fecha")+ylab("Decesos")

los_del_dia<-ggplot(compara_2_dias,aes(as.Date(FECHA_DEF.x),Decesos_contados.x-Decesos_contados.y))+
  geom_point(size=2.5,color="blue")+geom_line()+xlab("Fecha del deceso")+ylab("Decesos")+
  ggtitle(paste("Nuevos decesos anunciados. ",hoy))

los_del_dia_<-ggplot(filter(compara_2_dias,as.Date(FECHA_DEF.x)>as.Date(hoy)-35),aes(as.Date(FECHA_DEF.x),Decesos_contados.x-Decesos_contados.y))+
  geom_point(size=2.5,color="blue")+geom_line()+xlab("Fecha del deceso")+ylab("Decesos")+
  ggtitle(paste("Nuevos decesos anunciados. ",hoy))

las_tasas_4dias<-ggplot(filter(estimacion_decesos,!is.na(tasa_4dias)),aes(as.Date(FECHA_DEF),tasa_4dias))+geom_point(color="blue",size=3)+
  geom_line()+ggtitle("Decesos por COVID en México")+xlab("Fecha")+
  ylab("Decesos acumulados")+ylim(1,max(estimacion_decesos$tasa_4dias,na.rm=TRUE)+
                                    min(estimacion_decesos$tasa_4dias,na.rm=TRUE)-1)+
  geom_segment(x=as.Date("2020-04-15"),xend=as.Date(hoy),y=2**(4/7),yend=2**(4/7),color="purple",size=1.5)+
  geom_segment(x=as.Date("2020-04-15"),xend=as.Date(hoy),y=2**(4/15),yend=2**(4/15),color="yellow",size=1.5)+
  geom_segment(x=as.Date("2020-04-15"),xend=as.Date(hoy),y=2**(4/31),yend=2**(4/31),color="blue",size=1.5)+
  ylab("Tasa de aumento a 4 días")

duplicacion<-ggplot(filter(estimacion_decesos,!is.na(tasa_4dias)),aes(as.Date(FECHA_DEF),suave_duplex))+geom_point(color="brown",size=3)+
  geom_line()+ggtitle("Decesos por COVID en México.  Tasa de duplicacion")+xlab("Fecha")+
  ylab("Dias para duplicar")

pc_vs_tasuave<-ggplot(estos_ultimos,aes(per_capita,tasa_suave))+geom_point()+
  geom_smooth(method="lm")+geom_label_repel(aes(label = Estado),size=3.2,box.padding=0.85,point.padding = 0.15)+
  xlab("Decesos por mil hab.")+ylab("Tasa de aumento (4 días)")+
  ggtitle(paste(paste("Decesos por covid 19.  ",as.Date(estos_ultimos$FECHA_DEF[1])),".",sep=""))

por_dia_ponderado<-list()
for (k in unique(datos_resumidos$Abreviatura)){
  por_dia_ponderado[[k]]<-ggplot()+geom_point(data=estimacion_decesos_edos[[k]],
                                              aes(as.Date(FECHA_DEF),por_dia_suave))+
    geom_line(data=estimacion_decesos_edos[[k]],
              aes(as.Date(FECHA_DEF),por_dia_suave))
}

diarios_todos<-
  ggplot()+geom_point(data=estimacion_decesos,aes(as.Date(FECHA_DEF),por_dia_suave),color="blue")+
    geom_line(data=estimacion_decesos,aes(as.Date(FECHA_DEF),por_dia_suave),color="blue")+
    ggtitle("Evolución Covid por dia en México")+ylab("Decesos estimados (promedio 4 dias)")+xlab("Fecha")

#------------ Lo que sea por estados

# considerados<-c("MS","MC")
# considerados2<-c(considerados,"EUM")
# 
# datasas<-filter(juntos_estados,Abreviatura%in%considerados,as.Date(FECHA_DEF)>"2020-04-15")
# datasas2<-filter(juntos_estados,Abreviatura%in%considerados2,as.Date(FECHA_DEF)>"2020-04-15")
# 
# compara_edos_tasas<-ggplot(datasas,aes(as.Date(FECHA_DEF),tasa_4dias,color=Estado))+
#   geom_line(size=0.85)+geom_point()+ylim(1,max(datasas$tasa_4dias))+xlab("Fecha")+ylab("Tasa")+
#   ggtitle("Tasas a cuatro dias de aumento de decesos.")
# 
# compara_edos_tasuave<-ggplot()+
#   geom_line(data=datasas,aes(as.Date(FECHA_DEF),tasa_suave,color=Estado),size=0.85)+
#   geom_line(data=filter(juntos_estados,Abreviatura=="EUM"),aes(as.Date(FECHA_DEF),tasa_suave))+
#   geom_point(data=datasas,aes(as.Date(FECHA_DEF),tasa_suave,color=Estado))+
#   #ylim(1,max(datasas$tasa_4dias))+
#   xlab("Fecha")+ylab("Tasa")+ggtitle("Tasas a cuatro dias (Decesos Covid-19)")
# 
# compara_edos_totales<-ggplot(datasas,
#        aes(as.Date(FECHA_DEF),acumulados_estimados,color=Estado))+geom_line(size=0.85)+geom_point()+
#   xlab("Fecha")+ylab("Acumulados estimados")+ggtitle("Decesos Covid")
# 
# compara_edos_totalogs<-ggplot(datasas,aes(as.Date(FECHA_DEF),log(acumulados_estimados),color=Estado))+
#                               geom_line(size=0.85)+geom_point()+xlab("Fecha")+ylab("Log. de acumulados estimados")
# 
# compara_edos_percapita<-ggplot(datasas,
#                              aes(as.Date(FECHA_DEF),per_capita,color=Estado))+geom_line(size=0.85)+geom_point()+
#   xlab("Fecha")+ylab("Acumulados por mil hab.")+ggtitle("Decesos Covid")


tasuave<-function(x){
  considerados<-x
  datasas<-filter(juntos_estados,Abreviatura%in%considerados,as.Date(FECHA_DEF)>"2020-04-15")
  grafica<-ggplot()+
    geom_line(data=datasas,aes(as.Date(FECHA_DEF),tasa_suave,color=Estado),size=0.85)+
    geom_line(data=filter(juntos_estados,Abreviatura=="EUM"),aes(as.Date(FECHA_DEF),tasa_suave))+
    geom_point(data=datasas,aes(as.Date(FECHA_DEF),tasa_suave,color=Estado))+
    #ylim(1,max(datasas$tasa_4dias))+
    xlab("Fecha")+ylab("Tasa")+ggtitle("Tasas a cuatro dias (Decesos Covid-19)")
  
  return(grafica)
}

duplicaciones<-function(x){
  considerados<-x
  datasas<-filter(juntos_estados,Abreviatura%in%considerados,as.Date(FECHA_DEF)>"2020-04-15")
  grafica<-ggplot()+
    geom_line(data=datasas,aes(as.Date(FECHA_DEF),suave_duplex,color=Estado),size=0.85)+
    geom_line(data=filter(juntos_estados,Abreviatura=="EUM"),aes(as.Date(FECHA_DEF),suave_duplex))+
    geom_point(data=datasas,aes(as.Date(FECHA_DEF),suave_duplex,color=Estado))+
    #ylim(1,max(datasas$tasa_4dias))+
    xlab("Fecha")+ylab("Tasa")+ggtitle("Indice de duplicacion de casos en dias")
  
  return(grafica)
}

totales<-function(x){
  considerados<-x
  datasas<-filter(juntos_estados,Abreviatura%in%considerados,as.Date(FECHA_DEF)>"2020-04-15")
  grafica<-ggplot(datasas,
                  aes(as.Date(FECHA_DEF),acumulados_estimados,color=Estado))+geom_line(size=0.85)+geom_point()+
    xlab("Fecha")+ylab("Acumulados estimados")+ggtitle("Decesos Covid")
  
  return(grafica)
}

totales_log<-function(x){
  considerados<-x
  datasas<-filter(juntos_estados,Abreviatura%in%considerados,as.Date(FECHA_DEF)>"2020-04-15")
  grafica<-ggplot(datasas,
                  aes(as.Date(FECHA_DEF),log(acumulados_estimados),color=Estado))+geom_line(size=0.85)+geom_point()+
    xlab("Fecha")+ylab("Acumulados estimados (log)")+ggtitle("Decesos Covid")
  
  return(grafica)
}

percapitas<-function(x){
  considerados<-x
  datasas<-filter(juntos_estados,Abreviatura%in%considerados,as.Date(FECHA_DEF)>"2020-04-15")
  grafica<-ggplot(datasas,
                  aes(as.Date(FECHA_DEF),per_capita,color=Estado))+geom_line(size=0.85)+geom_point()+
    xlab("Fecha")+ylab("Acumulados por mil hab.")+ggtitle("Decesos Covid")
  
  
  return(grafica)
}

diarios_y_duplex<-function(x){
  datasas<-filter(juntos_estados,Abreviatura==x,as.Date(FECHA_DEF)>"2020-04-15")
  por_dia_ponderado[[x]]+geom_point(data=datasas,aes(as.Date(FECHA_DEF),suave_duplex),color="brown")+
    geom_line(data=datasas,aes(as.Date(FECHA_DEF),suave_duplex),color="brown")+ylab("Por dia y tasa deduplicacion")+
    xlab("")
}

diarios_2_estados<-function(a,b){
  estado1<-estimacion_decesos_edos[[a]]
  estado2<-estimacion_decesos_edos[[b]] 
  n1<-estado1$Estado[1]
  n2<-estado2$Estado[1]
  ggplot()+geom_point(data=estado1,aes(as.Date(FECHA_DEF),por_dia_suave),color="blue")+
    geom_line(data=estado1,aes(as.Date(FECHA_DEF),por_dia_suave),color="blue")+
    geom_point(data=estado2,aes(as.Date(FECHA_DEF),por_dia_suave),color="brown")+
    geom_line(data=estado2,aes(as.Date(FECHA_DEF),por_dia_suave),color="brown")+
    geom_label_repel(data = estado1[nrow(estado1), ],aes(label=Estado),x=as.Date(hoy)+7,
                    y=estado1$por_dia_suave[nrow(estado1)],color="blue",size=4)+
                    geom_label_repel(data = estado2[nrow(estado1), ],aes(label=Estado),
                    x=as.Date(hoy)+7,y=estado2$por_dia_suave[nrow(estado2)],
                    color="brown",size=4)+xlim(as.Date("2020-04-11"),as.Date(hoy)+2)+
                    ggtitle(paste(paste("Evolución Covid por dia        ",n1),paste(" y ", n2)))+
                    ylab("Decesos estimados (promedio 4 dias)")+xlab("Fecha")
}

colorines<-c("blue","brown","purple","red")

diarios_n_estados<-function(estados){
  g<-ggplot()
  contador<-0
  titulo<-"Evolución Covid por dia.        "
  ye<-numeric()
  for (k in estados){
    contador<-contador+1
    estado<-estimacion_decesos_edos[[k]]
    n1<-estado$Estado[1]
    ye[contador]<-estado$por_dia_suave[nrow(estado)]
    print(ye)
    for (j in c(1:contador)){
      if (ye[contador]-ye[j]<1.5 & ye[contador]-ye[j]>0){
        ye[contador] <- ye[contador]+1  
      }
      if (ye[contador]-ye[j]>-1.5 & ye[contador]-ye[j]<0){
        ye[contador] <- ye[contador]-1  
      }
    }
    g<-g+geom_point(data=estado,aes(as.Date(FECHA_DEF),por_dia_suave),color=colorines[contador])+
      geom_line(data=estado,aes(as.Date(FECHA_DEF),por_dia_suave),color=colorines[contador])+
      geom_label_repel(data = estado[nrow(estado), ],aes(label=Estado),x=as.Date(hoy)+4,
                       y=ye[contador],color=colorines[contador],size = 3.2)
    #titulo<-paste(titulo,paste(n1,".",sep=""),"   ")  
    print(contador)
    print(n1)
  }
  g<-g+ggtitle(titulo)+
    ylab("Decesos estimados (promedio 4 dias)")+xlab("Fecha")+xlim(as.Date("2020-04-11"),as.Date(hoy)+2)
  return(g)
}

#rm(fecha)