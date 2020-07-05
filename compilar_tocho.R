library(dplyr)
library(ggplot2)

hoy<-"2020-07-04"

options(encoding="native.enc")
setwd("C:/Users/plume/OneDrive/Documents/Erre/epidemia/datos_abiertos_covid")

source('~/Erre/epidemia/junta_datos.R')

setwd("C:/Users/plume/OneDrive/Documents/Erre/epidemia")

source('~/Erre/epidemia/checar_registros_por_dia.R')
#source('~/Erre/epidemia/bustrap.R')
source('~/Erre/epidemia/para_tasas.R')
source('~/Erre/epidemia/armar_por_estados.R')

options(encoding="latin1")
source('~/Erre/epidemia/hacer_grafiquitas.R')
options(encoding = "native.enc")
source('~/Erre/epidemia/municipales.R')
source('~/Erre/epidemia/explora_base_ultima.R')

