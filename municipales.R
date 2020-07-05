historicos.mortalidad<-read.csv("mortalidad_00.csv",as.is=TRUE,encoding="UTF-8")

clave_municipal<-read.csv("diccionario_datos_covid19/claves_municipales.csv",encoding="UTF-8")

temp<-clave_municipal
names(temp)<-c("MUNICIPIO_RES","Municipio","ENTIDAD_RES")
todos_ultima<-merge(ultimo_registro,temp)
rm(temp)

mortalidad.municipios.2018<-filter(historicos.mortalidad,!cve_municipio%in%c(0,996),
                                   indicador=="Defunciones generales")%>%
                            select(cve_entidad,cve_municipio,desc_municipio,X2018)    



