library(dplyr)


for (k in unique(datos_resumidos$ESTADO)){
    datos_resumidos_edo[[k]]<-filter(datos_resumidos,ESTADO==k)
    x<-which(datos_resumidos_edo[[k]]$FECHA_DEF=="9999-99-99")
    datos_resumidos_edo[[k]]$Def<-0
    datos_resumidos_edo[[k]]$Def[-x]<-1
    
    reg_diarios_positivos_edo[[k]]<-filter(datos_resumidos_edo[[k]],RESULTADO==1)
    reg_diarios_decesos_edo[[k]]<-filter(reg_diarios_positivos_edo[[k]],Def==1)
    
    por_fechas<-group_by(reg_diarios_decesos_edo[[k]],Dia_registro,FECHA_DEF)
    
    decesos_registrados_edo[[k]] <- summarise(por_fechas,n())
    names(decesos_registrados_edo[[k]])[3]<-"Decesos_contados"
    
    decesos_registrados_edo[[k]]$desfase<-
      decesos_registrados_edo[[k]]$Dia_registro-as.integer(as.Date(decesos_registrados_edo[[k]]$FECHA_DEF)-
                                                             as.Date("2020-04-11"))
    decesos_registrados_edo[[k]]<-filter(decesos_registrados_edo[[k]],as.Date(FECHA_DEF)>"2020-03-17")
    
    decesos_registrados_edo[[k]]$Dia_Def<-
      as.numeric(as.Date(decesos_registrados_edo[[k]]$FECHA_DEF)-as.Date("2020-03-17"))
}


# duno<-21
# diferencias<-numeric()
# 
# for (k in unique(decesos_registrados$Dia_Def)){
#   tempo <- filter(decesos_registrados,Dia_Def==k)$Decesos_contados
#   if (length(tempo)>duno){
#     diferencias<-append(diferencias,(tempo[duno+1]-tempo[duno])/tempo[duno])
#   }
# 
# }
# print(summary(diferencias))
# 
# decesos_estados<-list()
# for (k in unique(datos_resumidos$ESTADO)){
#   decesos_estados[[k]]<-filter(reg_diarios_decesos,ESTADO==k)
#   
# }

#rm(x,tempo,duno)