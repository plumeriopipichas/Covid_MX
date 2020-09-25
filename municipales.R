# historicos.mortalidad<-read.csv("mortalidad_00.csv",as.is=TRUE,encoding="UTF-8")
# 

claves_estados<-read.csv("claves_estados.csv",fileEncoding = "UTF-8")
clave_municipal<-read.csv("diccionario_datos_covid19/claves_municipales.csv",encoding="UTF-8")

clave_municipal<-mutate(clave_municipal,CLAVE_DOBLE=paste(CLAVE_ENTIDAD,CLAVE_MUNICIPIO,sep="_"))

#temp<-clave_municipal
#names(temp)<-c("MUNICIPIO_RES","Municipio","ENTIDAD_RES")
# todos_ultima<-merge(ultimo_registro,temp)
# rm(temp)
# 
# mortalidad.municipios.2018<-filter(historicos.mortalidad,!cve_municipio%in%c(0,996),
#                                    indicador=="Defunciones generales")%>%
#                             select(cve_entidad,cve_municipio,desc_municipio,X2018)    
# 

MEX_2_sf <- readRDS("C:/Users/plume/OneDrive/Documents/Erre/epidemia/gadm36_MEX_2_sf.rds")
temp<-select(MEX_2_sf,GID_2,NAME_1,NAME_2)
temp$ENTIDAD_FEDERATIVA<-toupper(temp$NAME_1)
x<-which(temp$NAME_1=="Distrito Federal")
temp$ENTIDAD_FEDERATIVA[x]<-"CDMX"
temp<-merge(temp,claves_estados,by="ENTIDAD_FEDERATIVA",all.x = TRUE)

temp$GID_2<-gsub("_1","",temp$GID_2)
temp$GID_2<-gsub("MEX.","",temp$GID_2)
temp$CLAVE_MUNICIPIO<-NA

for (k in 1:length(temp$GID_2)){
  for (j in 2:3){
    if (substr(temp$GID_2[k],j,j)=="."){
      temp$CLAVE_MUNICIPIO[k]<-as.integer(substr(temp$GID_2[k],j+1,nchar(temp$GID_2[k])))
    }
  }  
}

temp<-mutate(temp,CLAVE_DOBLE=paste(CLAVE_ENTIDAD,CLAVE_MUNICIPIO,sep="_"))

# for (k in length(MEX_2_sf_depurado$GID_2)){
#   stopa[k]<-4
#   if(substr(MEX_2_sf_depurado$GID_2[k],6,6)=="."){
#     stopa[k]<-5
#   }
#   ini2[k]<-stopa[k]+2
#   MEX_2_sf_depurado$CLAVE_ENTIDAD[k]<-as.integer(substr(MEX_2_sf_depurado$GID_2[k],5,stopa[k]))
#   for (j in 3:5){
#     if(substr(MEX_2_sf_depurado$GID_2,stopa[k]+j,stopa[k]+j)=="_"){
#       stopa2[k]<-stopa[k]+j-1
#     }
#   MEX_2_sf_depurado$CLAVE_MUNICIPIO[k]<-as.integer(substr(MEX_2_sf_depurado$GID_2,ini2[k],stopa2[k]))
#   }
# }