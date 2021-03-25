################################################################################
# Mapas de reporte diario para situación epidemiológica EDOMEX
# Guillermo de Anda-Jáuregui
# gdeanda at inmegen dot edu dot mx
################################################################################

################################################################################

# libraries ----

library(vroom)
library(tidyverse)
library(sf)

################################################################################

# leer datos ----

# leer shp federal ----

mx_shp <- read_sf("mg_2020_integrado/conjunto_de_datos/00mun.shp") #descargar de https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463807469/mg_2020_integrado.zip
edomex_shp <-
  mx_shp %>% 
  filter(CVE_ENT=="15")

el_path <- "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
temp <- tempfile()
download.file(url = el_path, destfile = temp)
el_file <- unzip(zipfile = temp, list = T)
el_file <- unz(description = temp, filename = el_file[1])
my_data <- vroom::vroom(el_file)
unlink(temp)
rm(temp, el_file)


################################################################################

# contar casos (siete días atras) ----
# residentes edomex en unidades edomex

casos_edomex <-
  my_data %>% 
  filter(ENTIDAD_UM=="15") %>% 
  filter(ENTIDAD_RES=="15") %>% 
  filter(CLASIFICACION_FINAL%in%1:3) %>% 
  filter(FECHA_SINTOMAS >= Sys.Date() - 30 ) %>% 
  mutate(grupo_edad = ifelse(EDAD < 60, "under_60", "60_over")) %>%
  group_by(MUNICIPIO_RES, grupo_edad) %>% 
  tally(name = "casos") 

################################################################################

# contar hospitalizados (siete días atras) ----

hospitalizaciones_edomex <-
  my_data %>% 
  filter(ENTIDAD_UM=="15") %>% 
  filter(ENTIDAD_RES=="15") %>% 
  filter(CLASIFICACION_FINAL%in%1:3) %>% 
  filter(FECHA_SINTOMAS >= Sys.Date() - 30 ) %>% 
  filter(TIPO_PACIENTE==2) %>% 
  mutate(grupo_edad = ifelse(EDAD < 60, "under_60", "60_over")) %>%
  group_by(MUNICIPIO_RES, grupo_edad) %>% 
  tally(name = "hospitalizados")

################################################################################

# contar muertes (siete días atras) ----

muertes_edomex <-
  my_data %>% 
  filter(ENTIDAD_UM=="15") %>% 
  filter(ENTIDAD_RES=="15") %>% 
  filter(CLASIFICACION_FINAL%in%1:3) %>%
  filter(!is.na(FECHA_DEF)) %>% 
  filter(FECHA_DEF >= Sys.Date() - 30 ) %>%
  mutate(grupo_edad = ifelse(EDAD < 60, "under_60", "60_over")) %>%
  group_by(MUNICIPIO_RES, grupo_edad) %>% 
  tally(name = "muertes")

################################################################################

#  letalidad (4 semanas anteriores a la actual) ----

letalidad_edomex <-
  my_data %>% 
  filter(ENTIDAD_UM=="15") %>% 
  filter(ENTIDAD_RES=="15") %>% 
  filter(CLASIFICACION_FINAL%in%1:3) %>%
  mutate(deceased = !is.na(FECHA_DEF)) %>% 
  filter(FECHA_SINTOMAS >= Sys.Date() - 30 ) %>%
  #filter(FECHA_SINTOMAS < Sys.Date() - 7 ) %>%
  mutate(grupo_edad = ifelse(EDAD < 60, "under_60", "60_over")) %>%
  group_by(MUNICIPIO_RES, grupo_edad, deceased) %>% 
  tally(name = "conteo") %>%
  ungroup() %>% 
  complete(MUNICIPIO_RES, grupo_edad, deceased, fill = list(conteo=0)) %>% 
  group_by(MUNICIPIO_RES, grupo_edad) %>% 
  mutate(total = sum(conteo)) %>% 
  mutate(letalidad = 100*conteo/total) %>% 
  filter(deceased) %>% 
  select(MUNICIPIO_RES, grupo_edad, letalidad)

################################################################################

# calcular tasa de hospitalizacion (4 semanas anteriores a la actual) ----

tasaHosp_edomex <-
  my_data %>% 
  filter(ENTIDAD_UM=="15") %>% 
  filter(ENTIDAD_RES=="15") %>% 
  filter(CLASIFICACION_FINAL%in%1:3) %>%
  filter(FECHA_SINTOMAS >= Sys.Date() - 30 ) %>%
  #filter(FECHA_SINTOMAS < Sys.Date() - 7 ) %>%
  mutate(grupo_edad = ifelse(EDAD < 60, "under_60", "60_over")) %>%
  group_by(MUNICIPIO_RES, grupo_edad, TIPO_PACIENTE) %>% 
  tally(name = "conteo") %>%
  ungroup() %>% 
  complete(MUNICIPIO_RES, grupo_edad, TIPO_PACIENTE, fill = list(conteo=0)) %>% 
  group_by(MUNICIPIO_RES, grupo_edad) %>% 
  mutate(total = sum(conteo)) %>% 
  mutate(tasa_hosp = 100*conteo/total) %>% 
  filter(TIPO_PACIENTE == 2) %>% 
  select(MUNICIPIO_RES, grupo_edad, tasa_hosp)

################################################################################

# calcular positividad (4 semanas anteriores a la actual) ----

positividad_edomex <-
  my_data %>% 
  filter(ENTIDAD_UM=="15") %>% 
  filter(ENTIDAD_RES=="15") %>% 
  mutate(positivo = case_when(CLASIFICACION_FINAL%in%1:3 ~ T,
                              CLASIFICACION_FINAL%in%7 ~ F, 
                              TRUE ~ NA)
         ) %>%
  filter(!is.na(positivo)) %>% 
  filter(FECHA_SINTOMAS >= Sys.Date() - 30 ) %>%
  #filter(FECHA_SINTOMAS < Sys.Date() - 7 ) %>%
  mutate(grupo_edad = ifelse(EDAD < 60, "under_60", "60_over")) %>%
  group_by(MUNICIPIO_RES, grupo_edad, positivo) %>% 
  tally(name = "conteo") %>%
  ungroup() %>% 
  complete(MUNICIPIO_RES, grupo_edad, positivo, fill = list(conteo=0)) %>% 
  group_by(MUNICIPIO_RES, grupo_edad) %>% 
  mutate(total = sum(conteo)) %>% 
  mutate(positividad = 100*conteo/total) %>% 
  filter(positivo) %>% 
  select(MUNICIPIO_RES, grupo_edad, positividad)

################################################################################

#make a list 

resultados <-
  ls(pattern = "edomex")

resultados <- resultados[resultados!="edomex_shp"]
names(resultados) <- str_remove(string = resultados, pattern = "_edomex")

resultados_under60 <-
  lapply(X = resultados, FUN = function(i){
  r <- get(i)
  r <-
    r %>% 
    filter(grupo_edad == "under_60")
})

resultados_60over <-
  lapply(X = resultados, FUN = function(i){
    r <- get(i)
    r <-
      r %>% 
      filter(grupo_edad == "60_over")
  })
# map it ----

resultados_60over$casos %>%
  ungroup() %>% 
  mutate(across(.cols = c(3), as.double))

mapas_under60 <- 
  lapply(seq_along(resultados_under60), function(i){
    df <- 
      resultados_under60[[i]] %>% 
      ungroup() %>% 
      mutate(across(.cols = c(3), as.double))
      
    nomen <- names(resultados_under60)[[i]]
    my_var = colnames(df)[[3]]
    
    nu_shp <-
      left_join(x = edomex_shp, 
                y = df, 
                by=c("CVE_MUN"="MUNICIPIO_RES")
      ) 
    
    nu_shp %>% 
    ggplot() +
      geom_sf(mapping = aes(fill = !!sym(my_var))) + 
      theme_minimal() + 
      scale_fill_viridis_c(na.value = "white") + 
      ggtitle(nomen,
              "menores 60")
    
  })
names(mapas_under60) <- names(resultados_under60)

mapas_60over <- 
  lapply(seq_along(resultados_60over), function(i){
    df <- resultados_60over[[i]] %>% 
      ungroup() %>% 
      mutate(across(.cols = c(3), as.double))
    nomen <- names(resultados_60over)[[i]]
    my_var = colnames(df)[[3]]
    
    nu_shp <-
      left_join(x = edomex_shp, 
                y = df, 
                by=c("CVE_MUN"="MUNICIPIO_RES")
      ) 
    
    nu_shp %>% 
      ggplot() +
      geom_sf(mapping = aes(fill = !!sym(my_var))) + 
      theme_minimal() + 
      scale_fill_viridis_c(na.value = "white") + 
      ggtitle(nomen,
              "60 en adelante")
    
  })
names(mapas_60over) <- names(resultados_60over)

