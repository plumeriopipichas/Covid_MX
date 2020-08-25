datos_resumidos_edo<-list()
decesos_por_dia_estado<-list()
decesos_registrados_edos<-list()
contados_recientes_edos<-list()
para_la_estimacion_edos<-list()
estimacion_decesos_edos<-list()

fechas<-unique(filter(contados_recientes,as.Date(FECHA_DEF)>"2020-04-11",as.Date(FECHA_DEF)<hoy)$FECHA_DEF)

d<-max(decesos_registrados$Dia_registro)

for (k in unique(datos_resumidos$Abreviatura)){
  datos_resumidos_edo[[k]]<-filter(datos_resumidos,Abreviatura==k)
  decesos_por_dia_estado[[k]]<-filter(reg_diarios_decesos,Abreviatura==k)
  por_fechas<-group_by(decesos_por_dia_estado[[k]],Dia_registro,FECHA_DEF)
  decesos_registrados_edos[[k]] <- summarise(por_fechas,Decesos_contados=n())
  decesos_registrados_edos[[k]]$Estado<-datos_resumidos_edo[[k]]$Nombre_Estado[1]
  decesos_registrados_edos[[k]]<-ungroup(decesos_registrados_edos[[k]])
  decesos_registrados_edos[[k]]$FECHA_DEF<-as.character(decesos_registrados_edos[[k]]$FECHA_DEF)
  decesos_registrados_edos[[k]]$desfase<-
    as.integer(as.Date(decesos_registrados_edos[[k]]$Dia_registro))-
    as.integer(as.Date(decesos_registrados_edos[[k]]$FECHA_DEF))
  decesos_registrados_edos[[k]]<-filter(decesos_registrados_edos[[k]],as.Date(FECHA_DEF)>"2020-03-17")
  decesos_registrados_edos[[k]]$Dia_Def<-
    as.numeric(as.Date(decesos_registrados_edos[[k]]$FECHA_DEF)-as.Date("2020-03-17"))
  
  contados_recientes_edos[[k]]<-filter(decesos_registrados_edos[[k]],Dia_registro==d)
  contados_recientes_edos[[k]]<-ungroup(contados_recientes_edos[[k]])
  
  
  for (fecha in fechas){
    print(fecha)
    if(!fecha %in% contados_recientes_edos[[k]]$FECHA_DEF){
      fila_extra <- 
        data.frame(Dia_Registro=d,FECHA_Def=fecha,nuevos=0,Estado=contados_recientes_edos[[k]]$Estado[1],
                             desfase=as.integer(as.Date(d))-as.integer(as.Date(fecha)),
                   Dia_Def=as.integer(as.Date(fecha))-as.integer(as.Date("2020-03-17")))
      names(fila_extra) <- names(contados_recientes_edos[[k]])
      contados_recientes_edos[[k]] <- rbind(contados_recientes_edos[[k]],fila_extra)
      print(fila_extra)
      rm(fila_extra)
    }
  }
  contados_recientes_edos[[k]]<-arrange(contados_recientes_edos[[k]],as.Date(FECHA_DEF))

  a<-rep(contados_recientes_edos[[k]]$Decesos_contados[1],
         length(contados_recientes_edos[[k]]$Decesos_contados))

  for (j in 1:(length(contados_recientes_edos[[k]]$Decesos_contados)-1)){
    a[j+1]<-a[j]+contados_recientes_edos[[k]]$Decesos_contados[j+1]
  }
  
  contados_recientes_edos[[k]]$acumulados_contados<-a

  para_la_estimacion_edos[[k]]<-filter(as.data.frame(decesos_registrados_edos[[k]]),Dia_Def>25,desfase>2)

  estimacion_decesos_edos[[k]]<-data.frame(FECHA_DEF=unique(para_la_estimacion_edos[[k]]$FECHA_DEF))
  for (j in 1:58){
    a<-filter(para_la_estimacion_edos[[k]],desfase==j+2)
    a<-select(a,FECHA_DEF,Decesos_contados)
    estimacion_decesos_edos[[k]]<-merge(estimacion_decesos_edos[[k]],a,by="FECHA_DEF",all.x = TRUE)
    names(estimacion_decesos_edos[[k]])[j+1]<-paste("registros_dia_",as.character(j+2),sep="")
  }

  estimacion_decesos_edos[[k]]$FECHA_DEF<-as.character(estimacion_decesos_edos[[k]]$FECHA_DEF)
  for (fecha in fechas){
    if (!fecha %in% estimacion_decesos_edos[[k]]$FECHA_DEF){
        fila_extra<-c(fecha,rep(0,ncol(estimacion_decesos_edos[[k]])-1))
        estimacion_decesos_edos[[k]]<-rbind(estimacion_decesos_edos[[k]],fila_extra)
    }
  }
   estimacion_decesos_edos[[k]]<-arrange(estimacion_decesos_edos[[k]],FECHA_DEF)
   estimacion_decesos_edos[[k]] <- estimacion_decesos_edos[[k]][1:(nrow(estimacion_decesos_edos[[k]])-4), ]
   
   for (i in 1:nrow(estimacion_decesos_edos[[k]])){
     x<-which(is.na(estimacion_decesos_edos[[k]])[i, ])
     estimacion_decesos_edos[[k]][i,x]<-0
   }
   
   estimacion_decesos_edos[[k]] <- estimacion_decesos_edos[[k]][ ,-2]
 
   for (j in 2:ncol(estimacion_decesos_edos[[k]])){
     estimacion_decesos_edos[[k]][ ,j]<-as.numeric(estimacion_decesos_edos[[k]][,j])
   }

  nombre<-contados_recientes_edos[[k]]$Estado[1]
  x<-which(retrasos_estado$Nombre_Estado==nombre)
  eles<-ceiling(retrasos_estado$Retraso_Promedio[x]+min(7,retrasos_estado$st_d[x]))
  
  for (j in 4:ncol(estimacion_decesos_edos[[k]])){
    #print(j)
    estimacion_decesos_edos[[k]]<-agrega_diagonal_(estimacion_decesos_edos[[k]],eles,j)
    #x<-estimacion_decesos_edos[[k]][nrow(estimacion_decesos_edos[[k]]),j]
    #print(paste("valor nuevo",as.character(x)))
  }
  rm(eles,nombre)
  
  x<-which(contados_recientes_edos[[k]]$FECHA_DEF=="2020-04-12")
  x<-contados_recientes_edos[[k]]$acumulados_contados[x]
  ab<-rep(x,nrow(estimacion_decesos_edos[[k]]))

  for (j in 1:(nrow(estimacion_decesos_edos[[k]])-1)){
    ab[j+1]<-ab[j]+(estimacion_decesos_edos[[k]])[j+1,ncol(estimacion_decesos_edos[[k]])]
  }
  print(k)
  print("A1")
  print(dim(estimacion_decesos_edos[[k]]))
   estimacion_decesos_edos[[k]]$acumulados_estimados<-ab
   estimacion_decesos_edos[[k]]$tasa_4dias<-NA
   estimacion_decesos_edos[[k]]$tasa_suave<-NA
   print(dim(estimacion_decesos_edos[[k]]))
   print("A2")
   for (j in 5:nrow(estimacion_decesos_edos[[k]])){
     if (estimacion_decesos_edos[[k]]$acumulados_estimados[j-4]>4){
        estimacion_decesos_edos[[k]]$tasa_4dias[j]<-
        estimacion_decesos_edos[[k]]$acumulados_estimados[j]/estimacion_decesos_edos[[k]]$acumulados_estimados[j-4]
     }
    }
   estimacion_decesos_edos[[k]]$Estado<-contados_recientes_edos[[k]]$Estado[1]
   estimacion_decesos_edos[[k]]$Abreviatura<-k

   estimacion_decesos_edos[[k]]$tasa_suave[5]<-estimacion_decesos_edos[[k]]$tasa_4dias[5]
   estimacion_decesos_edos[[k]]$tasa_suave[nrow(estimacion_decesos_edos[[k]])]<-
     estimacion_decesos_edos[[k]]$tasa_4dias[nrow(estimacion_decesos_edos[[k]])]
   print("B1")
   print(dim(estimacion_decesos_edos[[k]]))
   print("B2")
   x<-nrow(estimacion_decesos_edos[[k]])-1
   for (j in 6:x){
     estimacion_decesos_edos[[k]]$tasa_suave[j]<-
       0.5*estimacion_decesos_edos[[k]]$tasa_4dias[j]+0.25*estimacion_decesos_edos[[k]]$tasa_4dias[j-1]+
       0.25*estimacion_decesos_edos[[k]]$tasa_4dias[j+1]
   }
   estimacion_decesos_edos[[k]]$por_dia_suave<-estimacion_decesos_edos[[k]]$registros_dia_60
   for (j in 8:nrow(estimacion_decesos_edos[[k]])){
     estimacion_decesos_edos[[k]]$por_dia_suave[j]<-
       mean(estimacion_decesos_edos[[k]]$registros_dia_60[(j-3):j])
   }
   print("C1")
   estimacion_decesos_edos[[k]]<-mutate(estimacion_decesos_edos[[k]],suave_duplex=4*log(2)/log(tasa_suave))
   print(dim(estimacion_decesos_edos[[k]]))
   print("C2")
}

estimacion_decesos_edos[["MC"]]$Estado<-"EDOMEX"

estimacion_decesos$tasa_suave[nrow(estimacion_decesos)]<-estimacion_decesos$tasa_4dias[nrow(estimacion_decesos)]

juntos_estados<-select(estimacion_decesos_edos[[1]],FECHA_DEF,acumulados_estimados,
                       tasa_4dias,tasa_suave,Estado,Abreviatura,suave_duplex)

for (k in 2:length(estimacion_decesos_edos)){
  temp<-select(estimacion_decesos_edos[[k]],FECHA_DEF,acumulados_estimados,tasa_4dias,tasa_suave,
               Estado,Abreviatura,suave_duplex)
  juntos_estados <- rbind(juntos_estados,temp)
}

pegar_global<-select(estimacion_decesos,FECHA_DEF,acumulados_estimados,tasa_4dias,tasa_suave,suave_duplex)
pegar_global$Estado<-rep("Nacional",nrow(pegar_global))
pegar_global$Abreviatura<-rep("EUM",nrow(pegar_global))
pegar_global$FECHA_DEF<-as.character(pegar_global$FECHA_DEF)

juntos_estados$Estado<-as.character(juntos_estados$Estado)
juntos_estados$FECHA_DEF<-as.character(juntos_estados$FECHA_DEF)
juntos_estados<-rbind(juntos_estados,pegar_global)

juntos_estados <- merge(juntos_estados,select(poblacion,Abreviatura,Poblacion2010))
juntos_estados$per_capita<-1000*juntos_estados$acumulados_estimados/juntos_estados$Poblacion2010

estos_ultimos <- filter(juntos_estados,as.Date(FECHA_DEF)==max(as.Date(juntos_estados$FECHA_DEF))-2)

# #--------subconjuntos de estados -------------

frontera_norte=c("BC","SR","CH","CL","NL","TS")
frontera_sur=c("CC","CS","QR")

mas_acumulados<-arrange(estos_ultimos,desc(acumulados_estimados))$Abreviatura[2:5]
menos_acumulados<-arrange(estos_ultimos,acumulados_estimados)$Abreviatura[1:4]
mayor_tasa_reciente<-arrange(estos_ultimos,desc(tasa_suave))$Abreviatura[1:4]
menor_tasa_reciente<-arrange(estos_ultimos,tasa_suave)$Abreviatura[1:4]
mayor_pc<-arrange(estos_ultimos,desc(per_capita))$Abreviatura[1:4]
menor_pc<-arrange(estos_ultimos,per_capita)$Abreviatura[1:4]

rm(temp,k,a,j,datos_resumidos_edo,decesos_por_dia_estado,para_la_estimacion_edos,ab,
   pegar_global,datos_resumidos_edo,fechas,d,contados_recientes_edos)

edos_pob<-arrange(poblacion,desc(Poblacion2010))

