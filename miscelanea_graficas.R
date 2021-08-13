source('C:/Users/plume/Federico Yarepso/Trabajo/explora_epidemia/son_funciones.R')

graficas_DEP_edos<-list()
graficas_graves_edos<-list()
graficas_DEP_edos_pcj<-list()
graficas_graves_edos_pcj<-list()
graficas_DEP_edos21<-list()
graficas_graves_edos21<-list()

for (k in 1:32){
  g<-ambos_edo_DEP(k)
  g<-g+ggtitle(nombrar_estado(k))+xlim(as.Date("2020-04-01"),as.Date("2021-08-07"))+
    ylab("Defunciones por Covid (promedio a 7 días)")+xlab("Fecha de defunción")
  graficas_DEP_edos[[nombrar_estado(k)]]<-g
  g<-ambos_edo_g(k)
  g<-g+ggtitle(nombrar_estado(k))+xlim(as.Date("2020-04-01"),as.Date("2021-08-07"))+
    ylab("Casos graves (promedio a 7 días)")+xlab("Fecha de inicio de síntomas")
  graficas_graves_edos[[nombrar_estado(k)]]<-g
  #g<-ambos_edo_DEP_pcj(k)
  #g<-g+ggtitle(nombrar_estado(k))+xlim(as.Date("2020-04-01"),as.Date("2021-07-23"))
  #graficas_DEP_edos_pcj[[nombrar_estado(k)]]<-g
  #g<-ambos_edo_g_pcj(k)
  #g<-g+ggtitle(nombrar_estado(k))+xlim(as.Date("2020-04-01"),as.Date("2021-07-23"))
  #graficas_graves_edos_pcj[[nombrar_estado(k)]]<-g
}

for (k in 1:32){
  g<-ambos_edo_DEP(k)
  g<-g+ggtitle(nombrar_estado(k))+xlim(as.Date("2021-01-01"),as.Date("2021-08-07"))+
    ylab("Defunciones por Covid (promedio a 7 días)")+xlab("Fecha de defunción")
  graficas_DEP_edos21[[nombrar_estado(k)]]<-g
  g<-ambos_edo_g(k)
  g<-g+ggtitle(nombrar_estado(k))+xlim(as.Date("2021-01-01"),as.Date("2021-08-07"))+
    ylab("Casos graves (promedio a 7 días)")+xlab("Fecha de inicio de síntomas")
  graficas_graves_edos21[[nombrar_estado(k)]]<-g
  #g<-ambos_edo_DEP_pcj(k)
  #g<-g+ggtitle(nombrar_estado(k))+xlim(as.Date("2020-04-01"),as.Date("2021-07-23"))
  #graficas_DEP_edos_pcj[[nombrar_estado(k)]]<-g
  #g<-ambos_edo_g_pcj(k)
  #g<-g+ggtitle(nombrar_estado(k))+xlim(as.Date("2020-04-01"),as.Date("2021-07-23"))
  #graficas_graves_edos_pcj[[nombrar_estado(k)]]<-g
}

rm(g)