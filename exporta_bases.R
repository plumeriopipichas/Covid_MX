print("estimacion nacional")
write.csv(x = estimacion_decesos,file="csv_generados/estimacion_decesos_nacional.csv")
print("municipales")
write.csv(x = alto_positivo,file="csv_generados/municipales1.csv")
print("acumulados")
write.csv(x = acumulados_varios_,file="csv_generados/acumulados_DEP.csv")
print("reciente")
write.csv(x = ultimo_registro,file="csv_generados/registro_reciente.csv")
print("juntos")
write.csv(x = juntos_estados,file="csv_generados/juntos_estados.csv")
print("retrasos")
write.csv(x = checar_retrasos,file=paste(paste("csv_generados/anunciados_",hoy,sep=""),"_con_retraso.csv"))

for (k in unique(datos_resumidos$Abreviatura)){
  print(k)
  estado<-paste(filter(datos_resumidos,Abreviatura==k)$Nombre_Estado[1],".csv",sep="")
  archivo<-paste("csv_generados/por_estados/estimacion_decesos_",estado,sep="")
  write.csv(estimacion_decesos_edos[[k]],archivo)
}