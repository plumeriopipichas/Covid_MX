library(dplyr)
library(ggplot2)
library(ggrepel)

hoy<-"2020-09-24"

setwd("datos_abiertos_covid")

source('../junta_datos.R')

setwd("..")

print("uno uno")
source('algunos_registros.R')

print("municipios")
source('municipales.R')

print("base ultima")
source("explora_base_ultima.R",encoding = "UTF-8")

print('tasas')
source('para_tasas.R')

print('estados')
source('armar_por_estados.R')

print('graficas')
source('hacer_grafiquitas.R',encoding = "UTF-8")

print("OWID")
source('owid_explore.R')

print("mas graficas")
source('mas_graficas.R')

print('exporta')
#source('exporta_graficas.R',encoding = "UTF-8")
source('exporta_bases.R',encoding = "UTF-8")

rm(por_fechas,por_municipio,por_paciente,por_sintomas,por_sintomas_edos,reg_diarios_positivos,reg_diarios_decesos)
rm(para_la_estimacion,para_la_estimacion_edos,x,n,k,i)