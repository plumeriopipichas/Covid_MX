# temp<-read.csv("poblacional/base_municipios_final_datos_01.csv") %>%
#   filter(AÑO==2020)
# temp<-rbind(temp,read.csv("poblacional/base_municipios_final_datos_02.csv")) %>%
#   filter(AÑO==2020)
# 
# pob_municipios <- group_by(temp,CLAVE,NOM_ENT,MUN)%>%
#   summarise(Poblacion_2020=sum(POB))%>%
#   ungroup()%>%
#   mutate(ENTIDAD_RES=floor(CLAVE/1000),MUNICIPIO_RES=CLAVE-ENTIDAD_RES*1000)
#   
# rm(temp)

expob<-read.csv("poblacional/conjunto_de_datos/conjunto_de_datos_iter_00_cpv2020.csv",encoding = "UTF-8")

pob_municipios <- select(expob,ENTIDAD_RES=X.U.FEFF.ENTIDAD,Nombre_Estado=NOM_ENT,
                         MUNICIPIO_RES=MUN,Municipio=NOM_MUN,Poblacion_2020=POBTOT,LOC)%>% 
                          filter(LOC==0,!MUNICIPIO_RES==0)%>%
                          select(-LOC)

pob_estados <- filter(expob,NOM_LOC=="Total de la Entidad")%>% 
                select(ENTIDAD_RES=X.U.FEFF.ENTIDAD,Nombre_Estado=NOM_ENT,Poblacion_2020=POBTOT)