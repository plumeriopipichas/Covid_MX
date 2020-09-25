setwd("Ejemplos graficas/")

#---------Bloque agrupar estados

 comparando_dup<-list()
 for (k in 1:8){
   comparando_dup[[k]]<-
   duplicaciones(edos_pob$Abreviatura[(4*k-2):(4*k+1)])+scale_color_brewer(palette="Dark2")
   comparando[[k]]<-
   diarios_n_estados(edos_pob$Abreviatura[(4*k-2):(4*k+1)])
   ggsave(plot=comparando[[k]],filename = paste(as.character(k),"por_dia_pob.png"),device = "png")
   ggsave(plot=comparando_dup[[k]],filename = paste(as.character(k),"tasa_dupl_pob.png"),device = "png") 
 }


#----Bloque CFR

ggsave(CFR_pais,filename = "CFR_pais.png",device = "png")
ggsave(barras_CFR,filename = "barras_CFR.png",device = "png")
ggsave(compara_CFR_estados(extremos_CFR),filename = "extremos_CFR.png",device="png")

setwd("..")