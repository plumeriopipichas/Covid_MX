library(readr)
library(dplyr)

seleccion2019 <- c("ent_resid","mun_resid","sexo","edad","mes_ocurr","dia_ocurr")

DEP_2019 <- read_csv("mortalidad2019/conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2019.CSV")

print(dim(DEP_2019))

DEP_2019 <- filter(DEP_2019,anio_ocur==2019)%>%
  select(all_of(seleccion2019))

print(dim(DEP_2019))

DEP_2019$edad <- DEP_2019$edad-4000

x <- which(DEP_2019$edad<0)
y <- which(DEP_2019$edad==998)

DEP_2019$edad[x]<-0
DEP_2019$edad[y]<-NA

DEP_2019<-mutate(DEP_2019,FECHA_DEF=paste("2019",mes_ocurr,dia_ocurr,sep='-'))

Conteo_Def2019 <- summarise(group_by(DEP_2019,ent_resid,mun_resid),Defunciones_2019=n())
Conteo_Def2019 <- ungroup(Conteo_Def2019)

x <- which(names(Conteo_Def2019)=="ent_resid")
y <- which(names(Conteo_Def2019)=="mun_resid")

names(Conteo_Def2019)[x] <- "ENTIDAD_RES"
names(Conteo_Def2019)[y] <- "MUNICIPIO_RES"

Conteo_Def2019$ENTIDAD_RES <- as.integer(Conteo_Def2019$ENTIDAD_RES)
Conteo_Def2019$MUNICIPIO_RES <- as.integer(Conteo_Def2019$MUNICIPIO_RES)

fecha_edad_DEP2019 <- group_by(DEP_2019,FECHA_DEF,edad)%>%
  summarise(decesos2019=n())
fecha_edad_DEP2019<-ungroup(fecha_edad_DEP2019)

rm(x,y)
 

