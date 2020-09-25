# edades<-read.csv("Mexico-2019_edades.csv")
# 
# edades$Rango_de_edad<-paste(paste("(",edades$Rango_de_edad,sep=""),"]",sep="")
# poblacion<-sum(edades$total)
# 
# for (k in 1:nrow(edades)){
#   edades$acumulados[k]<-sum(edades$total[1:k])  
#   edades$porcentaje[k]<-round(100*(edades$total[k]/poblacion),2)
#   edades$porcentaje_acumulados[k]<-round(sum(edades$porcentaje[1:k]),2)  
# }

suavizar<-function(numerica,ele){
  if (!class(numerica)=="numeric" & !class(numerica)=="integer" ){
    stop("no numerica")
  }
  lista_suave<-numeric()
  lista_suave[1:ele]<-NA
  for (k in (ele+1):length(numerica)){
    lista_suave[k]<-mean(numerica[(k-ele):k])
  }
  return(lista_suave)
}

ultimo_completo$corte_edad<-NA
ultimo_completo$Def<-as.factor(ultimo_completo$Def)

razon_rangos_edad_DEP<-function(a,b,c,d){
  temp<-select(ultimo_completo,EDAD,RESULTADO,Def,FECHA_SINTOMAS,FECHA_DEF)%>%
    filter(Def==1,RESULTADO==1)%>%
    filter(as.Date(FECHA_DEF)>"2020-04-01")%>%
    filter(as.integer(as.Date(FECHA_DEF))<as.integer(as.Date(hoy))-15)
  temp1<-filter(temp,EDAD>=a & EDAD<=b)
  temp2<-filter(temp,EDAD>=c & EDAD<=d)
  temp1<-group_by(temp1,FECHA_DEF)
  temp2<-group_by(temp2,FECHA_DEF)
  temp1<-ungroup(summarise(temp1,DEP_rango1=n()))
  temp2<-ungroup(summarise(temp2,DEP_rango2=n()))
  print(str(temp1))
  temp1$DEP_suave1<-suavizar(temp1$DEP_rango1,7)
  temp2$DEP_suave2<-suavizar(temp2$DEP_rango2,7)
  temp<-merge(temp1,temp2,by="FECHA_DEF")
  temp<-mutate(temp,porcentaje_R1=round(100*DEP_rango1/(DEP_rango1+DEP_rango2),2))
  temp<-mutate(temp,razon_r1_a_r2=DEP_rango1/DEP_rango2)
  temp$razon_suave<-suavizar(temp$razon_r1_a_r2,7)
  temp$porcentaje_R1_suave<-suavizar(temp$porcentaje_R1,7)
  g1<-ggplot()+geom_line(data=temp1,aes(as.Date(FECHA_DEF),DEP_suave1),color="blue")+
    geom_line(data=temp2,aes(as.Date(FECHA_DEF),DEP_suave2),color="magenta")
  g2<-ggplot(temp,aes(as.Date(FECHA_DEF),porcentaje_R1_suave))+geom_line()+geom_point()
  show(g1)
  show(g2)
  return(temp)
}

razon_sexo_DEP<-function(suavidad){
  temp<-select(ultimo_completo,SEXO,RESULTADO,Def,FECHA_SINTOMAS,FECHA_DEF)%>%
    filter(Def==1,RESULTADO==1)%>%
    filter(as.Date(FECHA_DEF)>"2020-04-01")%>%
    filter(as.integer(as.Date(FECHA_DEF))<as.integer(as.Date(hoy))-15)
  temp1<-filter(temp,SEXO==1)
  temp2<-filter(temp,SEXO==2)
  temp1<-group_by(temp1,FECHA_DEF)
  temp2<-group_by(temp2,FECHA_DEF)
  temp1<-ungroup(summarise(temp1,DEP_sexo1=n()))
  temp2<-ungroup(summarise(temp2,DEP_sexo2=n()))
  temp<-merge(temp1,temp2,by="FECHA_DEF")
  temp<-mutate(temp,porcentaje_s1=round(100*DEP_sexo1/(DEP_sexo1+DEP_sexo2),2))
  temp<-mutate(temp,razon_s1_a_s2=DEP_sexo1/DEP_sexo2)
  temp$razon_suave<-suavizar(temp$razon_s1_a_s2,suavidad)
  temp$porcentaje_s1_suave<-suavizar(temp$porcentaje_s1,suavidad)
  return(temp)
}

razon_rangos_edad_positivos<-function(a,b,c,d){
  temp<-select(ultimo_completo,EDAD,RESULTADO,Def,FECHA_SINTOMAS)%>%
    filter(as.Date(FECHA_SINTOMAS)>"2020-03-23")%>%
    filter(as.integer(as.Date(FECHA_SINTOMAS))<as.integer(as.Date(hoy))-15)
  temp1<-filter(temp,EDAD>=a & EDAD<=b)
  temp2<-filter(temp,EDAD>=c & EDAD<=d)
  temp1<-group_by(temp1,FECHA_SINTOMAS)
  temp2<-group_by(temp2,FECHA_SINTOMAS)
  temp1<-ungroup(summarise(temp1,rango1=n()))
  temp2<-ungroup(summarise(temp2,rango2=n()))
  temp<-merge(temp1,temp2,by="FECHA_SINTOMAS")
  temp<-mutate(temp,porcentaje_R1=round(100*rango1/(rango1+rango2),2))
  temp<-mutate(temp,razon_r1_a_r2=rango1/rango2)
  temp$razon_suave<-suavizar(temp$razon_r1_a_r2,7)
  temp$porcentaje_R1_suave<-suavizar(temp$porcentaje_R1,7)
  return(temp)
}

# DEP_por_cortes_edad<-function(N){
#   temp<-select(ultimo_completo,EDAD,RESULTADO,Def,FECHA_SINTOMAS,FECHA_DEF,corte_edad)%>%
#   temp$corte_edad<-as.character(cut(temp$EDAD,N))
#   temp<-group_by(filter(temp,RESULTADO==1,Def==1),FECHA_DEF,corte_edad)
#   tempedad<-filter(ungroup(summarise(temp,cuantos=n())),!is.na(corte_edad))
#   tempedad<-filter(tempedad,as.integer(as.Date(FECHA_DEF))<as.integer(as.Date(hoy))-15)
#   tempedad$cuantos_suave<-NA
#   tempedad<-arrange(tempedad,corte_edad)
#   for (j in 8:nrow(tempedad)){
#     if (tempedad$corte_edad[j]==tempedad$corte_edad[j-1]){
#       tempedad$cuantos_suave[j]<-mean(tempedad$cuantos[(j-7):j]) 
#     }
#   }
#   g<-ggplot(data=filter(tempedad,!is.na(corte_edad)),aes(as.Date(FECHA_DEF),cuantos_suave,color=corte_edad))+
#     geom_point()+geom_line()
#   return(g)
# }
# 
# todos_por_cortes_edad<-function(N){
#   temp<-select(ultimo_completo,EDAD,RESULTADO,Def,FECHA_SINTOMAS,FECHA_DEF,corte_edad)
#   temp$corte_edad<-as.character(cut(temp$EDAD,N))
#   temp<-group_by(filter(temp,RESULTADO==1),FECHA_SINTOMAS,corte_edad)
#   tempedad<-filter(ungroup(summarise(temp,cuantos=n())),!is.na(corte_edad))
#   tempedad<-filter(tempedad,as.integer(as.Date(FECHA_DEF))<as.integer(as.Date(hoy))-15)
#   tempedad$cuantos_suave<-NA
#   for (j in 8:nrow(tempedad)){
#     tempedad$cuantos_suave[j]<-mean(tempedad$cuantos[(j-7):j])
#   }
#   g<-ggplot(tempedad,aes(as.Date(FECHA_SINTOMAS),cuantos_suave,color=corte_edad))+
#     geom_point()+geom_line()
#   return(g)
# }
# 
# 
# ultimo_completo$cortes_edades<-
#   as.character(cut(ultimo_completo$EDAD,c(0,seq(4,99,5),120)))
# temp<-group_by(filter(ultimo_completo,RESULTADO==1,Def==1),FECHA_DEF,cortes_edades,Def)
# temp_todos<-group_by(filter(ultimo_completo,RESULTADO==1),FECHA_SINTOMAS,cortes_edades,Def)
# poredad<-filter(ungroup(summarise(temp,cuantos=n())),!is.na(cortes_edades))%>%
#   filter(as.integer(as.Date(FECHA_DEF))<as.integer(as.Date(hoy))-15)
# poredad_todos<-filter(ungroup(summarise(temp_todos,cuantos=n())),!is.na(cortes_edades))%>%
#   filter(as.integer(as.Date(FECHA_SINTOMAS))<as.integer(as.Date(hoy))-15)
# 
# rm(rangos_edad_DEP,rangos_edad_todos,rangos_edad_ambos,temp)
# 
# rangos_edad_DEP<-list()
# rangos_edad_todos<-list()
# rangos_edad_ambos<-list()
# 
# for (k in 1:nrow(edades)){
#    temp<-filter(poredad,cortes_edades==edades$Rango_de_edad[k])
#    temp2<-filter(poredad_todos,cortes_edades==edades$Rango_de_edad[k])
# 
#    temp$cuantos_suave<-NA
#    temp2$cuantos_suave<-NA
#    
#    for (j in 8:nrow(temp)){
#      temp$cuantos_suave[j]<-mean(temp$cuantos[(j-7):j])
#    }
#    for (j in 8:nrow(temp2)){
#      temp2$cuantos_suave[j]<-mean(temp2$cuantos[(j-7):j])
#    }
#    rangos_edad_DEP[[k]]<-ggplot(temp,aes(as.Date(FECHA_DEF),cuantos_suave))+
#      geom_line()+geom_point()+ggtitle(paste("Edad ",edades$Rango_de_edad[k]))
#    rangos_edad_todos[[k]]<-ggplot(temp2,aes(as.Date(FECHA_SINTOMAS),cuantos_suave))+
#      geom_line()+geom_point()+ggtitle(paste("Edad ",edades$Rango_de_edad[k]))
#    rangos_edad_ambos[[k]]<-ggplot()+geom_line(data=temp,aes(as.Date(FECHA_DEF),cuantos_suave),color="blue")+
#      geom_point(data=temp,aes(as.Date(FECHA_DEF),cuantos_suave),color="blue")+
#      geom_line(data=filter(temp2,Def==0),aes(as.Date(FECHA_SINTOMAS),cuantos_suave),color="magenta")+
#      geom_point(data=filter(temp2,Def==0),aes(as.Date(FECHA_SINTOMAS),cuantos_suave),color="magenta")+
#      ggtitle(paste("Edad ",edades$Rango_de_edad[k]))
# }
#  
# rm(j,k,temp,temp2) 