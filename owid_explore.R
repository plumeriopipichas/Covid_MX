owid_covid<-read.csv(file="owid-covid-data.csv")

owid_por_fecha<-group_by(owid_covid,date)
para_CFR_global<-summarise(owid_por_fecha,cuantos=sum(new_cases,na.rm=TRUE),DEP=sum(new_deaths,na.rm=TRUE))
para_CFR_global<-filter(para_CFR_global,as.Date(date)>"2020-03-15" & as.Date(date)<as.Date(hoy)-30)
para_CFR_global$CFR_semanal<-NA

for (k in 7:nrow(para_CFR_global)){para_CFR_global$CFR_semanal[k]<-
  sum(para_CFR_global$DEP[(k-6):k])/sum(para_CFR_global$cuantos[(k-6):k])}
para_CFR_global<-para_CFR_global[7:nrow(para_CFR_global), ] 

rm(owid_por_fecha)