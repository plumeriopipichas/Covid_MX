etiquetas<-as.data.frame(matrix(data = c("MEX","CFR Mundial"),1,2),stringsAsFactors = FALSE)
names(etiquetas)<-etiquetas[1, ]

CFR_pais<-ggplot(Encoding="UTF-8")+geom_line(data=ver_CFR,aes(as.Date(FECHA_SINTOMAS),100*CFR_semanal),size=1.1,color="brown")+
  ggtitle("Evolución del CFR por Covid en México")+ylab("Porcentaje de decesos (ventana de 7 días)")+
  xlab("Fecha de inicio de síntomas")+geom_line(data=para_CFR_global,aes(as.Date(date),100*CFR_semanal))+
  geom_text_repel(data=etiquetas,aes(label="MEX"),x=as.Date(hoy)-25,y=10,color="brown",size=7.1)+ylim(0,22)+
  geom_text_repel(data=etiquetas,aes(label="CFR Mundial"),x=as.Date(hoy)-25,y=4,size=4.3)

compara_CFR_estados<-function(estados){
  g<-ggplot(Encoding="UTF-8")
  contador<-0
  titulo<-"Evolución del CFR por Covid."        
  ye<-numeric()
  for (k in estados){
    contador<-contador+1
    estado<-CFR_estados(k)
    n1<-estado$Nombre_Estado[1]
    ye[contador]<-100*estado$CFR_semanal[nrow(estado)]
    print(ye)
    for (j in c(1:contador)){
      if (ye[contador]-ye[j]<1.5 & ye[contador]-ye[j]>0){
        ye[contador] <- ye[contador]+1  
      }
      if (ye[contador]-ye[j]>-1.5 & ye[contador]-ye[j]<0){
        ye[contador] <- ye[contador]-1  
      }
    }
    g<-g+geom_point(data=estado,aes(as.Date(FECHA_SINTOMAS),100*CFR_semanal),color=colorines[contador])+
      geom_line(data=estado,aes(as.Date(FECHA_SINTOMAS),100*CFR_semanal),color=colorines[contador])+
      geom_label_repel(data = estado[nrow(estado), ],aes(label=Nombre_Estado),x=as.Date(hoy)+4,
                       y=ye[contador],color=colorines[contador],size = 3.2)
  }
  g<-g+ggtitle(titulo)+
    ylab("Porcentaje de defunciones en positivos")+xlab("Fecha de inicio de sintomas")+xlim(as.Date("2020-04-05"),as.Date(hoy)-29)
 
  g<-g+geom_line(data=ver_CFR,aes(as.Date(FECHA_SINTOMAS),100*CFR_semanal))  
  return(g)
}

totales_por_estado<-ungroup(totales_por_estado)
temp<-totales_por_estado
temp$Estado<-as.factor(temp$Nombre_Estado)
#levels(temp$Estado)<-arrange(totales_por_estado,desc(Porcentaje_CFR))$Nombre_Estado

barras_CFR<-ggplot(temp,aes(reorder(Estado,-Porcentaje_CFR),Porcentaje_CFR))+geom_col(color="blue",fill="#123213")+
  theme(axis.text.x = element_text(angle = 90))+ylab("CFR (Case Fatality Rate)")+xlab("Estado")+ggtitle("CFR acumulado por estados")

#rm(temp)