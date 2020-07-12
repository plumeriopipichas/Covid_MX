write.csv(x = estimacion_decesos,file="csv_generados/estimacion_decesos_nacional.csv")
write.csv(x = alto_positivo,file="csv_generados/municipales1.csv")
write.csv(x = acumulados_varios_,file="csv_generados/acumulados_DEP.csv")
write.csv(x = ultimo_registro,file="csv_generados/registro_reciente.csv")
write.csv(x = juntos_estados,file="csv_generados/juntos_estados.csv")

write.csv(x = checar_retrasos,file=paste(paste("csv_generados/anunciados_",hoy,sep=""),"_con_retraso.csv"))

for (k in unique(datos_resumidos$Abreviatura)){
  estado<-paste(filter(datos_resumidos,Abreviatura==k)$Nombre_Estado[1],".csv",sep="")
  archivo<-paste("csv_generados/por_estados/estimacion_decesos_",estado,sep="")
  write.csv(estimacion_decesos_edos[[k]],archivo)
}