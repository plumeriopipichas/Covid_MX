por_tipo_paciente<-group_by(datos_resumidos,ID_REGISTRO,RESULTADO)

sospechosos<-summarise(por_tipo_paciente,n=n())

sospechosos<-filter(sospechosos,RESULTADO==3)
print(dim(sospechosos))
s<-which(sospechosos$ID_REGISTRO %in% 
           unique(filter(datos_resumidos,!FECHA_DEF=="9999-99-99",RESULTADO==1)$ID_REGISTRO))

sospechosos <- sospechosos[s, ]

print(dim(sospechosos))