library(dplyr)
library(ggplot2)

hoy<-"2020-07-10"

setwd("datos_abiertos_covid")

source('../junta_datos.R')

setwd("..")

source('checar_registros_por_dia.R')
source('para_tasas.R')
source('armar_por_estados.R')

source('hacer_grafiquitas.R',encoding = "UTF-8")

source('municipales.R')
source("explora_base_ultima.R",encoding = "UTF-8")


source('exporta_bases.R',encoding = "UTF-8")