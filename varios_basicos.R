source('C:/Users/plume/Federico Yarepso/Trabajo/explora_epidemia/son_funciones.R')

hoy_comp <- 210812
anterior <- 210811

fechear <- function(d){
  a <- substr(d,1,2)
  b <- substr(d,3,4)
  c <- substr(d,5,6)
  d <- paste("20",a,sep="")
  d <- paste(d,b,sep="-")
  d <- paste(d,c,sep="-")
  d<-as.Date(d)
  return(d)
}

hoy_comp_ <- fechear(as.character(hoy_comp))
anterior_ <- fechear(as.character(anterior))
delta <- as.integer(hoy_comp_)-as.integer(anterior_)
print(delta)

ultimo_completo<-read.csv(paste(hoy_comp,"COVID19MEXICO.csv",sep=""))
anterior<-read.csv(paste(anterior,"COVID19MEXICO.csv",sep=""))

print(dim(ultimo_completo))
print(dim(anterior))

claves_estados<-read.csv("claves_edos.csv",encoding = "latin1")
claves_municipios<-read.csv("Claves_municipios.csv", encoding = "latin1")
x<-grep("^ ",claves_municipios$Municipio)
claves_municipios$Municipio[x]<-gsub(pattern = "^ ","",claves_municipios$Municipio[x])
rm(x)
claves_municipios <- filter(claves_municipios,!MUNICIPIO_RES==999)

ultimo_completo<-inner_join(ultimo_completo,claves_estados,by="ENTIDAD_RES")

ultimo_completo<-inner_join(ultimo_completo,claves_municipios,by=c("ENTIDAD_RES","MUNICIPIO_RES"))
anterior<-inner_join(anterior,claves_estados,by="ENTIDAD_RES")
anterior<-inner_join(anterior,claves_municipios,by=c("ENTIDAD_RES","MUNICIPIO_RES"))

selecciones<-c("ID_REGISTRO","SEXO","ENTIDAD_RES","MUNICIPIO_RES",
               "FECHA_INGRESO","FECHA_SINTOMAS","FECHA_DEF","EDAD","CLASIFICACION_FINAL",
               names(claves_estados),names(claves_municipios),"DIABETES","OBESIDAD")

selecciones2<-c(selecciones,"INTUBADO","UCI","NEUMONIA")

ultimo_graves <- select(ultimo_completo,all_of(selecciones2))%>% 
  filter(CLASIFICACION_FINAL%in%c(1,2,3))%>%
  filter(UCI==1|!FECHA_DEF=="9999-99-99"|NEUMONIA==1|INTUBADO==1)

anterior_graves <- select(anterior,all_of(selecciones2))%>%
  filter(CLASIFICACION_FINAL%in%c(1,2,3))%>%
  filter(UCI==1|!FECHA_DEF=="9999-99-99"|NEUMONIA==1|INTUBADO==1)

 x<-which(!as.character(ultimo_graves$FECHA_DEF)== "9999-99-99")
 ultimo_DEP<-ultimo_graves[x, ]
 x<-which(!anterior_graves$FECHA_DEF=="9999-99-99")
 anterior_DEP<-anterior_graves[x, ]
 rm(x)

print(c("ag",dim(anterior_graves)))
print(c("ad",dim(anterior_DEP)))

print(c("ug",dim(ultimo_graves)))
print(c("ud",dim(ultimo_DEP)))


#------SEGMENTAR

a_segmentar_DEP <- group_by(anterior_DEP,FECHA_DEF,ENTIDAD_RES,MUNICIPIO_RES)
a_segmentar_DEP <-  ungroup(summarise(a_segmentar_DEP,defunciones=n()))

a_segmentar_graves <- group_by(anterior_graves,FECHA_SINTOMAS,ENTIDAD_RES,MUNICIPIO_RES)
a_segmentar_graves <- ungroup(summarise( a_segmentar_graves,casos=n()))

segmentar_DEP <- group_by(ultimo_DEP,FECHA_DEF,ENTIDAD_RES,MUNICIPIO_RES)
segmentar_DEP <-  ungroup(summarise(segmentar_DEP,defunciones=n()))

segmentar_graves <- group_by(ultimo_graves,FECHA_SINTOMAS,ENTIDAD_RES,MUNICIPIO_RES)
segmentar_graves <-  ungroup(summarise(segmentar_graves, casos=n()))



#---graficas generales

ambos_graves<-ggplot()+
   geom_line(data=a_diarios_graves(),aes(as.Date(FECHA_SINTOMAS),suave_graves),color="blue")+
   geom_point(data=a_diarios_graves(),aes(as.Date(FECHA_SINTOMAS),suave_graves),color="blue")+
   geom_line(data=diarios_graves(),aes(as.Date(FECHA_SINTOMAS),suave_graves),color="brown")+
   geom_point(data=diarios_graves(),aes(as.Date(FECHA_SINTOMAS),suave_graves),color="brown")+
  xlim(as.Date("2020-03-31"),hoy_comp_)

ambos_DEP<-ggplot()+
   geom_line(data=a_diarios_DEP(),aes(as.Date(FECHA_DEF),suave_DEP),color="blue")+
   geom_point(data=a_diarios_DEP(),aes(as.Date(FECHA_DEF),suave_DEP),color="blue")+
   geom_line(data=diarios_DEP(),aes(as.Date(FECHA_DEF),suave_DEP),color="brown")+
   geom_point(data=diarios_DEP(),aes(as.Date(FECHA_DEF),suave_DEP),color="brown")+
  xlim(as.Date("2020-03-31"),hoy_comp_)

rm(anterior,k,anterior_)
