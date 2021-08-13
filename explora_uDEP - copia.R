library(lubridate)

#pob_estados <- read.csv("pob_estados.csv")

ultimo_DEP$dif<-as.integer(as.Date(ultimo_DEP$FECHA_DEF))-
  as.integer(as.Date(ultimo_DEP$FECHA_SINTOMAS))

edades_DEP <- group_by(filter(ultimo_DEP,dif>0),EDAD)%>%
  summarise(DEP_Covid=n(),lapsos=mean(dif),lapso_median=median(dif),
            diabetes=proporcion_unos(DIABETES),obes=proporcion_unos(OBESIDAD))

names(DEP_2019) <- cambio_nombre(names(DEP_2019),"edad","EDAD")
por_edad <- group_by(DEP_2019,EDAD)
edades_DEP <- merge(edades_DEP,summarise(por_edad,DEP_2019=n()),by="EDAD")%>%
  mutate(razon_aumento=DEP_Covid/DEP_2019)

edades_DEP <- ungroup(edades_DEP)
rm(por_edad)

ultimo_DEP <- mutate(ultimo_DEP,mes_nombre=meses[month(FECHA_DEF)],mes=month(FECHA_DEF))

meses_DEP <- group_by(filter(ultimo_DEP,dif>0),mes,mes_nombre)%>%
  summarise(decesos=n(),lapsos=mean(dif),lapso_median=median(dif),edad_media=mean(EDAD),
            edad_mediana=median(EDAD))

meses_DEP <- ungroup(meses_DEP)

por_edad <- group_by(ultimo_DEP,EDAD)
edad_DEP <- summarise(por_edad,DEP_Covid=n())

acumulados_DEP_edos <- group_by(ultimo_DEP,ENTIDAD_RES)%>%
  summarise(DEP_acumulados=n())
acumulados_DEP_edos <- ungroup(acumulados_DEP_edos)%>%
  merge(pob_estados,by="ENTIDAD_RES")%>%
  mutate(DEP_por_mil_hab=round(1000*DEP_acumulados/Poblacion_2020,2))


municipales <- group_by(ultimo_DEP,ENTIDAD_RES,MUNICIPIO_RES,Nombre_Estado,Municipio)
municipales <- ungroup(summarise(municipales,defunciones_covid=n()))
print(dim(municipales))
municipales <- merge(municipales,Conteo_Def2019,by=c("ENTIDAD_RES","MUNICIPIO_RES"))
municipales$porcentaje_aumento <- round(100*municipales$defunciones_covid/municipales$Defunciones_2019,2)
municipales<-merge(municipales,select(pob_municipios,ENTIDAD_RES,MUNICIPIO_RES,Poblacion_2020),
                   c("ENTIDAD_RES","MUNICIPIO_RES"))%>%
  mutate(DEP_C19_p_mil=round(1000*defunciones_covid/Poblacion_2020,2))
print(dim(municipales))

municipales_old <- group_by(anterior_DEP,ENTIDAD_RES,MUNICIPIO_RES)
municipales_old <- ungroup(summarise(municipales_old,defunciones_covid=n()))
print(dim(municipales_old))
municipales_old <- merge(municipales_old,Conteo_Def2019,by=c("ENTIDAD_RES","MUNICIPIO_RES"))
municipales_old$porcentaje_aumento <- round(100*municipales_old$defunciones_covid/municipales_old$Defunciones_2019,2)
municipales_old<-merge(municipales_old,select(pob_municipios,ENTIDAD_RES,MUNICIPIO_RES,Poblacion_2020),
                   c("ENTIDAD_RES","MUNICIPIO_RES"))%>%
                    mutate(DEP_C19_p_mil=round(1000*defunciones_covid/Poblacion_2020,2))


compara_municipales<-merge(municipales,municipales_old,by=c("MUNICIPIO_RES","ENTIDAD_RES"))%>%
  mutate(pob=Poblacion_2020.x/1000)

cambiox<-function(x){gsub(".x","_ultimo",x)}
cambioy<-function(x){gsub(".y","_anterior",x)}
names(compara_municipales)<-lapply(names(compara_municipales),cambiox)
names(compara_municipales)<-lapply(names(compara_municipales),cambioy)
compara_municipales<-mutate(compara_municipales,diferencia_DEP=defunciones_covid_ultimo-defunciones_covid_anterior)
print(names(compara_municipales))
compara_municipales<-mutate(compara_municipales,dif_DEP_pmil=DEP_C19_p_mil_ultimo-DEP_C19_p_mil_anterior)
  
rm(cambiox,cambioy)

#temp_por_edos <- group_by(municipales,ENTIDAD_RES,Nombre_Estado)
#entidades <- ungroup(summarise(temp_por_edos,defunciones_covid=sum(defunciones_covid),
 #                              Defunciones_2019=sum(Defunciones_2019)))
#entidades <- mutate(entidades,porcentaje_aumento=round(100*defunciones_covid/Defunciones_2019,2))

###############separar tipos de clasificacion final

ultimo_completo$Def <-0
x<-which(ultimo_completo$FECHA_DEF=="9999-99-99")
ultimo_completo$Def[-x]<-1

por_clasificacion<-group_by(ultimo_completo,CLASIFICACION_FINAL,Def)%>%
  summarise(cuantos=n())%>%
  ungroup()

total<-sum(por_clasificacion$cuantos)
x<-which(por_clasificacion$Def==1)
parcial_Def1<-sum(por_clasificacion$cuantos[x])
parcial_Def0<-sum(por_clasificacion$cuantos[-x])

print(total)
print(parcial_Def0)
print(parcial_Def1)

por_clasificacion$pje_del_total<-round(100*por_clasificacion$cuantos/total,2)
por_clasificacion$pje_claseDef<-NA
por_clasificacion$pje_claseDef[x]<-round(100*por_clasificacion$cuantos[x]/parcial_Def1,2)
por_clasificacion$pje_claseDef[-x]<-round(100*por_clasificacion$cuantos[-x]/parcial_Def0,2)

rm(x,total,parcial_Def0,parcial_Def1)



