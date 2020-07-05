chequeo<-numeric()
columnas<-integer()
filas<-integer()

for (rous in 10:(nrow(estimacion_decesos)-1)){
  for (cols in 3:(nrow(estimacion_decesos)+2-rous)){
    a<-estimacion_decesos[(rous-7):(rous-1),(cols-1):cols]
    x<-mean(a[,2]/a[ ,1])*estimacion_decesos[rous,cols-1]-estimacion_decesos[rous,cols]
    chequeo<-append(chequeo,x)
    columnas<-append(columnas,cols)
    filas<-append(filas,rous)
  }  
}

revisar_estimados<-data.frame(filas,columnas,diferencia=chequeo)
por_desfase<-group_by(revisar_estimados,columnas)
por_evento<-group_by(revisar_estimados,filas)
por_desfase<-summarise(por_desfase,dif=mean(diferencia))
por_evento<-summarise(por_evento,dif=mean(diferencia))

ajuste<-c(0,0,por_desfase$dif)

rm(a,x,chequeo,filas,columnas,cols,rous)

