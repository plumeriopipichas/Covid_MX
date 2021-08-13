ultimo_DEP_pasados<-filter(ultimo_DEP,
                  as.integer(as.Date(FECHA_DEF))<as.integer(as.Date(fechear(hoy_comp)))-31)
ultimo_DEP_recientes<-filter(ultimo_DEP,
                  as.integer(as.Date(FECHA_DEF))>=as.integer(as.Date(fechear(hoy_comp)))-31)

municipales_r <- group_by(ultimo_DEP_recientes,ENTIDAD_RES,MUNICIPIO_RES,Nombre_Estado,Municipio)
municipales_r <- ungroup(summarise(municipales_r,defunciones_covid=n()))
municipales_r<-merge(municipales_r,select(pob_municipios,ENTIDAD_RES,MUNICIPIO_RES,Poblacion_2020),
                   c("ENTIDAD_RES","MUNICIPIO_RES"))%>%
   mutate(DEP_C19_p_mil=round(1000*defunciones_covid/Poblacion_2020,2))
 
municipales_p <- group_by(ultimo_DEP_pasados,ENTIDAD_RES,MUNICIPIO_RES)
municipales_p <- ungroup(summarise(municipales_p,defunciones_covid=n()))
municipales_p <- merge(municipales_p,select(pob_municipios,
                ENTIDAD_RES,MUNICIPIO_RES,Poblacion_2020),c("ENTIDAD_RES","MUNICIPIO_RES"))%>%
mutate(DEP_C19_p_mil=round(1000*defunciones_covid/Poblacion_2020,2))

compara_municipales_<-merge(municipales_r,municipales_p,by=c("MUNICIPIO_RES","ENTIDAD_RES"))%>%
   mutate(pob=Poblacion_2020.x/1000)
cambiox<-function(x){gsub(".x","_ultimo",x)}
cambioy<-function(x){gsub(".y","_anterior",x)}
names(compara_municipales_)<-lapply(names(compara_municipales_),cambiox)
names(compara_municipales_)<-lapply(names(compara_municipales_),cambioy)

 
rm(cambiox,cambioy,municipales_p,municipales_r)