library(ggplot2)
library(dplyr)
library(lubridate)



a_diarios_DEP<-function(estados=1:32){
  temp <- filter(a_segmentar_DEP,ENTIDAD_RES%in%estados)
  temp <- group_by(temp,FECHA_DEF)
  temp <- ungroup(summarise(temp,defunciones=sum(defunciones)))
  x<-which(pob_estados$ENTIDAD_RES%in%estados)
  pob<-sum(pob_estados$Poblacion_2020[x])
  temp<-mutate(temp,suave_DEP=suavizar(defunciones))%>%
    mutate(DEP_pmh=1000*suave_DEP/pob)
  return (temp)
}

a_diarios_graves<-function(estados=1:32){
  temp <- filter(a_segmentar_graves,ENTIDAD_RES%in%estados)
  temp <- group_by(temp,FECHA_SINTOMAS)
  temp <- ungroup(summarise(temp,casos=sum(casos)))
  x<-which(pob_estados$ENTIDAD_RES%in%estados)
  pob<-sum(pob_estados$Poblacion_2020[x])
  temp<-mutate(temp,suave_graves=suavizar(casos))%>%
    mutate(casos_pmh=1000*suave_graves/pob)
  return (temp)
}

a_diarios_DEP_municipio <- function(estado,municipio){
  temp <- filter(a_segmentar_DEP,ENTIDAD_RES==estado,MUNICIPIO_RES==municipio)
  temp <- group_by(temp,FECHA_DEF)
  temp <- ungroup(summarise(temp,defunciones=sum(defunciones)))
  #g <- ggplot(temp,aes(FECHA_DEF,defunciones))+geom_col()
  #show(g)
  return (temp)
}

a_diarios_graves_municipio <- function(estado,municipio){
  temp <- filter(a_segmentar_graves,ENTIDAD_RES==estado,MUNICIPIO_RES==municipio)
  temp <- group_by(temp,FECHA_SINTOMAS)
  temp <- ungroup(summarise(temp,casos=sum(casos)))
  #g <- ggplot(temp,aes(FECHA_SINTOMAS,defunciones))+geom_col()
  #show(g)
  return (temp)
}

ambos_edo_DEP<-function(edo){
  ggplot()+geom_line(data=a_diarios_DEP(edo),aes(as.Date(FECHA_DEF),suave_DEP),color="blue")+  
  geom_point(data=a_diarios_DEP(edo),aes(as.Date(FECHA_DEF),suave_DEP),color="blue")+
  geom_line(data=diarios_DEP(edo),aes(as.Date(FECHA_DEF),suave_DEP),color="brown")+
  geom_point(data=diarios_DEP(edo),aes(as.Date(FECHA_DEF),suave_DEP),color="brown")+
  geom_line(data=juntar_tablas_DEP(edo),aes(as.Date(FECHA_DEF),estimado_suave),color="#109910")+
  geom_point(data=juntar_tablas_DEP(edo),aes(as.Date(FECHA_DEF),estimado_suave),color="#109010")     
}

ambos_edo_g<-function(edo){
  ggplot()+geom_line(data=a_diarios_graves(edo),aes(as.Date(FECHA_SINTOMAS),suave_graves),color="blue")+  
    geom_point(data=a_diarios_graves(edo),aes(as.Date(FECHA_SINTOMAS),suave_graves),color="blue")+
    geom_line(data=diarios_graves(edo),aes(as.Date(FECHA_SINTOMAS),suave_graves),color="brown")+
    geom_point(data=diarios_graves(edo),aes(as.Date(FECHA_SINTOMAS),suave_graves),color="brown")+
    geom_line(data=juntar_tablas_graves(edo),aes(as.Date(FECHA_SINTOMAS),estimado_suave),color="#109910")+
    geom_point(data=juntar_tablas_graves(edo),aes(as.Date(FECHA_SINTOMAS),estimado_suave),color="#109010")   
}

ambos_edo_DEP_pcj<-function(edo){
  ggplot()+geom_line(data=a_diarios_DEP(edo),aes(as.Date(FECHA_DEF),DEP_pmh),color="blue")+  
    geom_point(data=a_diarios_DEP(edo),aes(as.Date(FECHA_DEF),DEP_pmh),color="blue")+
    geom_line(data=diarios_DEP(edo),aes(as.Date(FECHA_DEF),DEP_pmh),color="brown")+
    geom_point(data=diarios_DEP(edo),aes(as.Date(FECHA_DEF),DEP_pmh),color="brown")  
}

ambos_edo_g_pcj<-function(edo){
  ggplot()+geom_line(data=a_diarios_graves(edo),aes(as.Date(FECHA_SINTOMAS),casos_pmh),color="blue")+  
    geom_point(data=a_diarios_graves(edo),aes(as.Date(FECHA_SINTOMAS),casos_pmh),color="blue")+
    geom_line(data=diarios_graves(edo),aes(as.Date(FECHA_SINTOMAS),casos_pmh),color="brown")+
    geom_point(data=diarios_graves(edo),aes(as.Date(FECHA_SINTOMAS),casos_pmh),color="brown")  
}

cambio_nombre<-function(una_lista,items,reemplazo){
  x <- which(una_lista==items)
  una_lista[x] <- reemplazo
  return(una_lista)
}

diarios_DEP<-function(estados=1:32){
  temp <- filter(segmentar_DEP,ENTIDAD_RES%in%estados)
  temp <- group_by(temp,FECHA_DEF)
  temp <- ungroup(summarise(temp,defunciones=sum(defunciones)))
  x<-which(pob_estados$ENTIDAD_RES%in%estados)
  pob<-sum(pob_estados$Poblacion_2020[x])
  temp<-mutate(temp,suave_DEP=suavizar(defunciones))%>%
    mutate(DEP_pmh=1000*suave_DEP/pob)
  return (temp)
}

diarios_graves<-function(estados=1:32){
  temp <- filter(segmentar_graves,ENTIDAD_RES%in%estados)
  temp <- group_by(temp,FECHA_SINTOMAS)
  temp <- ungroup(summarise(temp,casos=sum(casos)))
  temp$suave_graves<-temp$casos
  x<-which(pob_estados$ENTIDAD_RES%in%estados)
  pob<-sum(pob_estados$Poblacion_2020[x])
  temp<-mutate(temp,suave_graves=suavizar(casos))%>%
    mutate(casos_pmh=1000*suave_graves/pob)
  return (temp)
}

diarios_graves_municipio <- function(estado,municipio){
  temp <- filter(segmentar_graves,ENTIDAD_RES==estado,MUNICIPIO_RES==municipio)
  temp <- group_by(temp,FECHA_SINTOMAS)
  temp <- ungroup(summarise(temp,casos=sum(casos)))
  nombre_estado <- nombrar_estado(estado)
  nombre_municipio <-nombrar_municipio(estado,municipio)
  
  g <- ggplot(temp,aes(as.Date(FECHA_SINTOMAS),casos))+geom_col()+
    ggtitle(paste("Estado:",nombre_estado,"          Municipio:",nombre_municipio))+
    xlab("Fecha")+ylab("Casos UCI Confirmadas por COVID19")
  
  show(g)
  
  return (temp)
}

diarios_DEP_municipio <- function(estado,municipio){
  temp <- filter(segmentar_DEP,ENTIDAD_RES==estado,MUNICIPIO_RES==municipio)
  temp <- group_by(temp,FECHA_DEF)
  temp <- ungroup(summarise(temp,defunciones=sum(defunciones)))
  
  g <- ggplot(temp,aes(as.Date(FECHA_DEF),defunciones))+geom_col()+
    ggtitle(paste("Estado:",nombrar_estado(estado),"          Municipio:",nombrar_municipio(estado,municipio)))+
    xlab("Fecha")+ylab("Defunciones Confirmadas por COVID19")
  
  show(g)
  
  return (temp)
}


diarios_rango_edad <- function(min =0, max = 120){
  base <- filter(ultimo_DEP,EDAD>min-1,EDAD<max+1)%>%
    select(ID_REGISTRO,FECHA_DEF)%>%
    group_by(FECHA_DEF)%>%
    summarise(contados_rango=n())
  base <- ungroup(base)
  base_aux <- filter(ultimo_DEP,EDAD < min | EDAD>max)%>%
    select(ID_REGISTRO,FECHA_DEF)%>%
    group_by(FECHA_DEF)%>%
    summarise(contados_otros=n())
  base <- merge(base,base_aux,by="FECHA_DEF")
  base <- mutate(base,porcentaje_en_rango=round(100*contados_rango/(contados_rango+contados_otros)))%>%
    mutate(proporcion_suave=suavizar(porcentaje_en_rango)/100)
  base <- base[7:nrow(base), ]
  
  #base <- merge(base,diarios_estimados,by="FECHA_DEF")%>%
   # mutate(estimados_en_rango=proporcion_suave*suave_DEP)
  
  base_aux <- filter(fecha_edad_DEP2019,edad>min-1,edad<max+1)%>%
    group_by(FECHA_DEF)
  base_aux <- summarise(base_aux,decesos2019=sum(decesos2019))
  base_aux <- ungroup(base_aux)
  base_aux$mes_dia <- paste(month(as.Date(base_aux$FECHA_DEF)),day(as.Date(base_aux$FECHA_DEF)),sep='-')
  base$mes_dia <- paste(month(as.Date(base$FECHA_DEF)),day(as.Date(base$FECHA_DEF)),sep='-')
  base <- merge(base,base_aux,by="mes_dia")%>%
    #mutate(DEP2019_suave=suavizar(decesos2019))%>%
    select(-mes_dia)
    #mutate(razon_aumento=round(estimados_en_rango/DEP2019_suave,4))
  names(base) <- cambio_nombre(names(base),"decesos2019","DEP2019_rango")
  names(base) <- cambio_nombre(names(base),"DEP2019_suave","DEP2019sv_en_rango")
  base <- base[7:(nrow(base)-4), ]
  
  
  return(base)
}

diarios_rango_edad_graves <- function(min =0, max = 120,edos=c(1:32)){
  base <- filter(ultimo_graves,EDAD>min-1,EDAD<max+1,ENTIDAD_RES%in%edos)%>%
    select(ID_REGISTRO,FECHA_SINTOMAS)%>%
    group_by(FECHA_SINTOMAS)%>%
    summarise(contados_rango=n())
  base <- ungroup(base)
  base_aux <- filter(ultimo_graves,EDAD < min | EDAD>max,ENTIDAD_RES%in%edos)%>%
    select(ID_REGISTRO,FECHA_SINTOMAS)%>%
    group_by(FECHA_SINTOMAS)%>%
    summarise(contados_otros=n())
  base <- merge(base,base_aux,by="FECHA_SINTOMAS")
  base <- mutate(base,porcentaje_en_rango=round(100*contados_rango/(contados_rango+contados_otros)))%>%
    mutate(proporcion_suave=suavizar(porcentaje_en_rango)/100)
  base <- base[7:nrow(base), ]
 
  
  return(base)
}


grafica_DEP_municipio_edad<-function(estado,municipio,a=0.5,s=1.5){
  temp<-filter(ultimo_DEP,ENTIDAD_RES==estado,MUNICIPIO_RES==municipio)%>%
    filter(as.Date(FECHA_DEF)>"2020-03-19")
  g<-ggplot(temp,aes(as.Date(FECHA_DEF),EDAD))+geom_point(size=s,alpha=a,aes(color=as.factor(SEXO)))+
    ggtitle(paste(nombrar_municipio(estado,municipio),nombrar_estado(estado),sep="     "))+
    xlab("Fecha de defunción")
  g<-g+scale_color_manual(name="Sexo",values=c("#990099","#109910"),breaks=c(1,2),labels=c("F","M"))
  show(g)
}

grafica_DEP_estado_edad<-function(estado,a=0.32,s=2){
  temp<-filter(ultimo_DEP,ENTIDAD_RES==estado)%>%
    filter(as.Date(FECHA_DEF)>"2020-03-19")
  g<-ggplot(temp,aes(as.Date(FECHA_DEF),EDAD,color=as.factor(SEXO)))+geom_point(size=s,alpha=a)+  
      ggtitle(nombrar_estado(estado))+xlab("Fecha de defunción")
  g<-g+scale_color_manual(name="Sexo",values=c("#990099","#109910"),breaks=c(1,2),labels=c("F","M"))
  show(g)
}

grafica_graves_municipio_edad<-function(estado,municipio,a=0.32,s=2){
  temp<-filter(ultimo_graves,ENTIDAD_RES==estado,MUNICIPIO_RES==municipio)%>%
    filter(as.Date(FECHA_SINTOMAS)>"2020-03-19")
  g<-ggplot(temp,aes(as.Date(FECHA_SINTOMAS),EDAD,color=as.factor(SEXO)))+
    geom_point(alpha=a,size=s)+
    ggtitle(paste(nombrar_municipio(estado,municipio),nombrar_estado(estado),sep="     "))+
    xlab("Fecha de inicio de síntomaas")   
  g<-g+scale_color_manual(name="Sexo",values=c("#990099","#109910"),breaks=c(1,2),labels=c("F","M"))
  show(g)
}

grafica_graves_estado_edad<-function(estado,a=0.2,desfinic=0,desfin=0,s=1.5){
  temp<-filter(ultimo_graves,ENTIDAD_RES==estado)%>%
    filter(as.Date(FECHA_SINTOMAS)>"2020-03-19")
  g<-ggplot(temp,aes(as.Date(FECHA_SINTOMAS),EDAD,color=as.factor(SEXO)))+
    geom_point(size=s,alpha=a)
  g<-g+ggtitle(paste(nombrar_estado(estado),"   Casos severos de COVID19"))+
    xlab("Fecha de inicio de síntomas")+
    xlim(as.Date("2020-04-11")+desfinic,as.Date(hoy_comp_)-desfin)+ylim(0,100)+
    xlab("Fecha de inicio de síntomaas")    
  g<-g+scale_color_manual(name="Sexo",values=c("#990099","#109910"),breaks=c(1,2),labels=c("F","M"))    
show(g)
}

hacer_comparativo <- function(estados=1:32){
  antes<-a_diarios_DEP(estados)
  despues<-diarios_DEP(estados)
  comparativo<-merge(antes,despues,by="FECHA_DEF",all=TRUE)
  comparativo<-select(comparativo,FECHA_DEF,DEP_anterior=defunciones.x,DEP_reciente=defunciones.y)%>%
    mutate(diferencia=DEP_reciente-DEP_anterior)%>%
    filter(as.Date(FECHA_DEF)>"2020-04-11")%>% 
    filter(as.Date(FECHA_DEF)<as.Date(fechear(as.character(hoy_comp)))-10)
  veces <- floor(nrow(comparativo)/delta)-1
  estimado <- data.frame(actualizado=comparativo$DEP_reciente,referencia=comparativo$DEP_anterior)
  estimado$ajuste<-1
  estimado$ajustado<-estimado$actualizado
  for (k in 1:veces){
    for (j in (k*delta+1):nrow(comparativo)){
      estimado$ajuste[j] <- estimado$actualizado[j-delta]/estimado$referencia[j-delta]
      estimado$ajustado[j]<-estimado$actualizado[j]*estimado$ajuste[j]
    }
    estimado$referencia<-estimado$actualizado
    estimado$actualizado<-estimado$ajustado
  }
  comparativo$estimacion_DEP<-estimado$actualizado
  comparativo$suave_DEP <- suavizar(comparativo$estimacion_DEP)
  comparativo<-ungroup(comparativo)
  return(comparativo)
}

# hacer_comparativo_g <- function(estados=1:32){
#   comparativo<-merge(a_diarios_graves(estados),diarios_graves(estados),
#                      by="FECHA_SINTOMAS",all=TRUE)
#   comparativo<-select(comparativo,FECHA_SINTOMAS,casos_anterior=casos.x,
#                       casos_reciente=casos.y)%>%
#     mutate(diferencia=casos_reciente-casos_anterior)%>%
#     filter(as.Date(FECHA_SINTOMAS)>"2020-04-11")%>%
#     filter(as.Date(FECHA_SINTOMAS)<as.Date(fechear(as.character(hoy_comp)))-10)
#   #veces <- floor(nrow(comparativo)/delta)-1
#   estimado <- data.frame(actualizado=comparativo$casos_reciente,
#                          referencia=comparativo$casos_anterior)
#   estimado$ajuste<-1
#   estimado$ajustado<-estimado$actualizado
#   for (k in 1:veces){
#     for (j in (k*delta+1):nrow(comparativo)){
#       estimado$ajuste[j] <- estimado$actualizado[j-delta]/estimado$referencia[j-delta]
#       estimado$ajustado[j]<-estimado$actualizado[j]*estimado$ajuste[j]
#     }
#     estimado$referencia<-estimado$actualizado
#     estimado$actualizado<-estimado$ajustado
#   }
#   comparativo$estimacion_graves<-estimado$actualizado
#   comparativo$suave_graves <- suavizar(comparativo$estimacion_graves)
#   comparativo<-ungroup(comparativo)
#   print('hc')
#   return(comparativo)
# }

juntar_tablas_DEP<-function(estados=1:32){
  tabla<-merge(a_diarios_DEP(estados),diarios_DEP(estados),by="FECHA_DEF",all.y=TRUE)%>%
    mutate(diferencia=suave_DEP.y-suave_DEP.x,estimado_suave=suave_DEP.y)
  ayeres<-length(tabla$diferencia)- min(which(is.na(tabla$diferencia)))
  for (i in (ayeres+1):length(tabla$diferencia)){
    tabla$estimado_suave[i]<-tabla$estimado_suave[i]+tabla$diferencia[i-ayeres]
  }
  return(tabla)
}


juntar_tablas_graves<-function(estados=1:32){
  tabla<-merge(a_diarios_graves(estados),diarios_graves(estados),by="FECHA_SINTOMAS",all.y=TRUE)%>%
    mutate(diferencia=suave_graves.y-suave_graves.x,estimado_suave=suave_graves.y)
  ayeres<-length(tabla$diferencia)- min(which(is.na(tabla$diferencia)))
  for (i in (ayeres+1):length(tabla$diferencia)){
    tabla$estimado_suave[i]<-tabla$estimado_suave[i]+tabla$diferencia[i-ayeres]
  }
  return(tabla)
 }

nombrar_estado<-function(clave){
  x<-which(claves_estados$ENTIDAD_RES==clave)
  temp<-claves_estados[x,]
  return(temp$Nombre_Estado[1])
}


nombrar_municipio<-function(a,b){
  x<-which(claves_municipios$ENTIDAD_RES==a & claves_municipios$MUNICIPIO_RES==b)
  temp<-claves_municipios[x,]
  return(temp$Municipio[1])
}

nr_municipio<-function(estado,municipio){
  temp1<-filter(ultimo_DEP,ENTIDAD_RES==estado,MUNICIPIO_RES==municipio)
  temp2<-filter(anterior_DEP,ENTIDAD_RES==estado,MUNICIPIO_RES==municipio)
  x<-which(!temp1$ID_REGISTRO%in%temp2$ID_REGISTRO)
  temp3<-temp1[x, ]
  View(temp3)
  return(temp3)
}

proporcion_unos <- function(x){
  p <- round(length(which(x==1))/length(which(x<3)),2)
  return(p)
}

suavizar <- function(listado,n=7){
  aux<-listado
  for (j in n:length(listado)){
    aux[j] <- mean(listado[(j-n+1):j],na.rm = TRUE)
  }
  return(aux)
}


grafica_porcentaje_edad<-function(inferior,superior){
    t<-paste("Casos graves de COVID en México 2021. Proporción de casos que corresponden a poblacion de",as.character(inferior),
             "a",as.character(superior),"años.")
    g<-ggplot(diarios_rango_edad_graves(inferior,superior),
            aes(as.Date(FECHA_SINTOMAS),100*proporcion_suave))+
    geom_line(size=1.32,color="#238521")+
    xlim(as.Date("2021-01-01"),as.Date("2021-08-03"))+
    ylab("porcentaje de casos en ese rango de edad")+
    xlab("Fecha de inicio de síntomas")+ggtitle(t)
    return(g)
}