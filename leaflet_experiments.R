################################################################################
# Mapas de reporte diario para situación epidemiológica EDOMEX
# Versión LEAFLET para Shiny
# Rodrigo Zepeda
# rodrigo.zepeda [at] imss [dot] gob [dot] mx
################################################################################

rm(list = ls())
library("sf")
library("vroom")
library("leaflet")
library("leafpop")
library("leaflet.extras")
library("tidyverse")
library("lubridate")
library("mapview")

for (fname in list.files("mapcovid/scripts/", pattern = "*.R", full.names = T)){source(fname)}

#Get data from conacyt and preprocess
#this has to be previously calculated daily
#Pre-processing stage
#---------------------------------------------------------------------------
mydata      <- download_hugo()
dict        <- download_data_dic()
mx_shp      <- download_inegi(data_dict = dict)
dats        <- process_hugo(mydata, dict, save_file = "mapcovid/data/Processed_dats.rds")

