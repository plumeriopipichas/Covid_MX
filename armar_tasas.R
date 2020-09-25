covid_8mayo<-read.csv(file = "datos_abiertos_covid19_08.05.2020/200508COVID19MEXICO.csv")

x<-which(covid_8mayo$FECHA_DEF=="9999-99-99")
covid_8mayo$def<-0
covid_8mayo$def[-x]<-1

positivos_covid<-filter(covid_8mayo,RESULTADO==1)
decesos_covid<-filter(positivos_covid,def==1)
decesos_covid$Dia_RIP<-as.numeric(as.Date(decesos_covid$FECHA_DEF)-as.Date("2020-03-17"))

para_tasas_def<-summarise(group_by(decesos_covid,FECHA_DEF),n())
names(para_tasas_def)<-c("Dia","Decesos")

acumulados<-numeric()
n<-0
for (k in 1:dim(para_tasas_def)[1]){
  n<-para_tasas_def$Decesos[k]+n
  acumulados<-append(acumulados,n)
}

para_tasas_def$acumulados<-acumulados
para_tasas_def$tasa4dias<-NA

for (k in 5:length(acumulados)){
  para_tasas_def$tasa4dias[k]<-para_tasas_def$acumulados[k]/para_tasas_def$acumulados[k-4]
}

# para_tasas_def$tasa_semana<-NA
# 
# for (k in 8:length(acumulados)){
#   para_tasas_def$tasa_semana[k]<-para_tasas_def$acumulados[k]/para_tasas_def$acumulados[k-7]
# }

covid_1mayo<-read.csv(file = "datos_abiertos_covid19_01.05.2020/200501COVID19MEXICO.csv")

x<-which(covid_1mayo$FECHA_DEF=="9999-99-99")
covid_1mayo$def<-0
covid_1mayo$def[-x]<-1

positivos_covida<-filter(covid_1mayo,RESULTADO==1)
decesos_covida<-filter(positivos_covida,def==1)
decesos_covida$Dia_RIP<-as.numeric(as.Date(decesos_covida$FECHA_DEF)-as.Date("2020-03-17"))

para_tasas_defa<-summarise(group_by(decesos_covida,FECHA_DEF),n())
names(para_tasas_defa)<-c("Dia","Decesos")

acumulados<-numeric()
n<-0
for (k in 1:dim(para_tasas_defa)[1]){
  n<-para_tasas_defa$Decesos[k]+n
  acumulados<-append(acumulados,n)
}

para_tasas_defa$acumulados<-acumulados