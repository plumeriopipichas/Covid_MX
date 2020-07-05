library(dplyr)
library(ggplot2)

hoy<-"2020-07-04"

options(encoding="native.enc")
setwd("datos_abiertos_covid")

source('../junta_datos.R')

setwd("..")

source('checar_registros_por_dia.R')
#source('~/Erre/epidemia/bustrap.R')
source('para_tasas.R')
source('armar_por_estados.R')

options(encoding="latin1")
source('hacer_grafiquitas.R')
options(encoding = "native.enc")
source('municipales.R')
source('explora_base_ultima.R')

