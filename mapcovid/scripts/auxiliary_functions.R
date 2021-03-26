#Función que descarga los datos de la página de CONACYT
#pero si no puede entonces lee de disco.
#Sólo descarga los datos si ha pasado un día desde la última descarga
#de datos
download_hugo <- function (last_download = ymd(read_lines("Ultima_descarga.txt")), download = last_download < today(),
                           save_file = "data/Conacyt_dats.rds"){
  if (download | !file.exists(save_file)){
    mydata <- tryCatch({
        message("Estamos descargando tu base de datos, no desesperes.")
        el_path <- "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
        temp    <- tempfile()
        download.file(url = el_path, destfile = temp)
        el_file <- unzip(zipfile = temp, list = T)
        el_file <- unz(description = temp, filename = el_file[1])
        my_data <- vroom::vroom(el_file)
        unlink(temp)
        rm(temp, el_file)
        my_data %>% write_rds(save_file)
        if(file.exists("Ultima_descarga.txt")){file.remove("Ultima_descarga.txt")}
        today() %>% write_lines("Ultima_descarga.txt")
    },
    error=function(cond) {
      message("Unable to download reading previous datasave. Reading from memory.")
      my_data <- read_rds(save_file)
    },
    warning=function(cond){
      message(cond)
      message("\nEs normal que tengas estos problemas de lectura...")
      if (exists("my_data")){
        my_data %>% write_rds(save_file)
        if(file.exists("Ultima_descarga.txt")){file.remove("Ultima_descarga.txt")}
        today() %>% write_lines("Ultima_descarga.txt")
      }
    })
  } else {
    message("Reading data from memory")
    my_data <- read_rds(save_file)
  }
  return(my_data)
}

#From https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
read_excel_allsheets <- function(filename, tibble = T) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

download_data_dic <- function(catalogo_path = "data/201128 Catalogos.xlsx",
                              descriptores_path = "data/201128 Descriptores_.xlsx"){
  if (!file.exists(catalogo_path) | !file.exists(descriptores_path)){
    mydata <- tryCatch({
      message("Estamos descargando el diccionario de datos.")
      el_path <- "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/diccionario_datos_covid19.zip"
      temp    <- tempfile()
      download.file(url = el_path, destfile = temp)
      if (!dir.exists("data")){dir.create("data")}
      el_file <- unzip(zipfile = temp, list = T)
      unzip(zipfile = temp, files = el_file$Name[1:2], exdir = "data")
      data_dictionary <- read_excel_allsheets(catalogo_path)
    },
    error=function(cond) {
      message("Unable to download")
    },
    warning=function(cond){
      message(cond)
    })
  } else {
    message("Loading data dictionary from memory")
    data_dictionary <- read_excel_allsheets(catalogo_path)
  }
  return(data_dictionary)
}

download_inegi <- function (data_dict, filepath = "conjunto_de_datos/00mun.shp",
                            savefile = "mapcovid/data/inegi.rds"){
  if (!file.exists(filepath)){
    message(paste0("Te estoy descargando los datos de INEGI porque no estaban en el filepath=",filepath))
    mx_shp <- tryCatch({
      el_path <- "https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463807469/mg_2020_integrado.zip"
      temp    <- tempfile()
      download.file(url = el_path, destfile = temp)
      el_file   <- unzip(zipfile = temp, list = T)
      file_list <- str_subset(el_file$Name, "00mun")
      unzip(zipfile = temp, files = file_list, exdir = getwd())
      unlink(temp)
      rm(temp)
      mx_shp <- read_sf(filepath) 
    },
    error=function(cond) {
      message("No pude descargar")
    },
    warning=function(cond){
      message(cond)
    })
  } else {
    message("Leyendo los datos de inegi del path")
    mx_shp <- read_sf(filepath) 
  }
  mx_shp <- mx_shp %>% 
    left_join(data_dict$`Catálogo de ENTIDADES`, 
              by = c("CVE_ENT" = "CLAVE_ENTIDAD"))
  mx_shp %>% write_rds(savefile)
  return(mx_shp)
}

process_hugo <- function(my_data, data_dictionary, save_file = NULL){
  
  #Limpiamos la base de los NA
  my_data <- my_data %>%
    mutate(FECHA_SINTOMAS = ymd(FECHA_SINTOMAS)) %>%
    mutate(FECHA_DEF = ymd(FECHA_DEF))  %>%
    left_join(data_dictionary$`Catálogo de ENTIDADES`, by = c("ENTIDAD_RES" = "CLAVE_ENTIDAD")) %>%
    drop_na(ENTIDAD_FEDERATIVA, MUNICIPIO_RES, FECHA_SINTOMAS, EDAD) 
    
  message("Calculando casos totales")
  casos_totales <-
    my_data %>% 
    group_by(ENTIDAD_RES, ENTIDAD_FEDERATIVA, MUNICIPIO_RES, FECHA_SINTOMAS, EDAD) %>% 
    tally(name = "Casos") 
  
  message("Calculando casos confirmados")
  casos_confirmados <-
    my_data %>% 
    filter(CLASIFICACION_FINAL %in% 1:3) %>% 
    group_by(ENTIDAD_RES, ENTIDAD_FEDERATIVA, MUNICIPIO_RES, FECHA_SINTOMAS, EDAD) %>% 
    tally(name = "Casos Confirmados") 
  
  message("Calculando casos confirmados por laboratorio")
  casos_confirmados_laboratorio <-
    my_data %>% 
    filter(CLASIFICACION_FINAL == 3) %>% 
    group_by(ENTIDAD_RES, ENTIDAD_FEDERATIVA, MUNICIPIO_RES, FECHA_SINTOMAS, EDAD) %>% 
    tally(name = "Casos Confirmados por Laboratorio") 
  
  message("Calculando casos que no fueron")
  casos_no_covid_laboratorio <-
    my_data %>% 
    filter(CLASIFICACION_FINAL == 7) %>% 
    group_by(ENTIDAD_RES, ENTIDAD_FEDERATIVA, MUNICIPIO_RES, FECHA_SINTOMAS, EDAD) %>% 
    tally(name = "Casos No COVID por Laboratorio") 
  
  # contar hospitalizados (siete días atras) ----
  message("Calculando casos hospitalizados")
  hospitalizaciones <-
    my_data %>% 
    filter(CLASIFICACION_FINAL %in% 1:3) %>% 
    filter(TIPO_PACIENTE==2) %>% 
    group_by(ENTIDAD_RES, ENTIDAD_FEDERATIVA, MUNICIPIO_RES, FECHA_SINTOMAS, EDAD) %>% 
    tally(name = "Hospitalizados")
  
  message("Calculando casos hospitalizados confirmados")
  hospitalizaciones_confirmadas <-
    my_data %>% 
    filter(CLASIFICACION_FINAL %in% 1:3) %>% 
    filter(TIPO_PACIENTE==2) %>% 
    group_by(ENTIDAD_RES, ENTIDAD_FEDERATIVA, MUNICIPIO_RES, FECHA_SINTOMAS, EDAD) %>% 
    tally(name = "Hospitalizados Confirmados")
  
  message("Calculando defunciones")
  muertes <-
    my_data %>% 
    filter(!is.na(FECHA_DEF)) %>% 
    group_by(ENTIDAD_RES, ENTIDAD_FEDERATIVA, MUNICIPIO_RES, FECHA_SINTOMAS, EDAD) %>% 
    tally(name = "Defunciones")
  
  message("Calculando defunciones confirmadas")
  muertes_confirmadas <-
    my_data %>% 
    filter(CLASIFICACION_FINAL %in% 1:3) %>% 
    filter(!is.na(FECHA_DEF)) %>% 
    group_by(ENTIDAD_RES, ENTIDAD_FEDERATIVA, MUNICIPIO_RES, FECHA_SINTOMAS, EDAD) %>% 
    tally(name = "Defunciones Confirmadas")
  
  by_join <- c("ENTIDAD_RES", "MUNICIPIO_RES", "FECHA_SINTOMAS", "EDAD", "ENTIDAD_FEDERATIVA")
  
  message("Generando la base de datos. Falta poco...")
  dats    <- full_join(casos_totales, casos_confirmados, by = by_join) %>%
    full_join(casos_confirmados_laboratorio, by = by_join) %>%
    full_join(casos_no_covid_laboratorio, by = by_join) %>%
    full_join(hospitalizaciones, by = by_join) %>%
    full_join(hospitalizaciones_confirmadas, by = by_join) %>%
    full_join(muertes, by = by_join) %>%
    full_join(muertes_confirmadas, by = by_join) %>%
    mutate(across(c(`Casos`:`Defunciones Confirmadas`), ~replace_na(.x,0)))
  
  if (!is.null(save_file)){
    dats %>% write_rds(save_file)
  }
  message("¡Listo!")
  return(dats)
}

data_2_map <- function(dats, inegi, min_date = ymd("1000/01/01"),  
                       max_date = ymd("3000/01/01"),
                       entidad = unique(dats$ENTIDAD_FEDERATIVA)[15],
                       min_age = -Inf, max_age = Inf){
  
  dats <- dats %>%
    filter(ENTIDAD_FEDERATIVA == entidad) %>%
    filter(FECHA_SINTOMAS >= min_date & FECHA_SINTOMAS <= max_date) %>%
    filter(EDAD >= min_age & EDAD <= max_age) %>%
    group_by(ENTIDAD_RES, ENTIDAD_FEDERATIVA, MUNICIPIO_RES) %>%
    summarise(
      across(c(Casos:`Defunciones Confirmadas`), ~sum(.x)), .groups = "keep"
    ) %>%
    mutate(`Positividad` = 100*`Casos Confirmados por Laboratorio`/(`Casos Confirmados por Laboratorio` + `Casos No COVID por Laboratorio`)) %>%
    mutate(`Letalidad`   = 100*`Defunciones Confirmadas`/(`Casos Confirmados`)) %>%
    mutate(`Tasa de Hospitalización` = 100*`Hospitalizados Confirmados`/(`Casos Confirmados`)) 
  
  inegi <- inegi %>% 
    filter(ENTIDAD_FEDERATIVA == entidad) %>%
    select(-CVEGEO,-CVE_ENT) %>%
    left_join(dats, by=c("CVE_MUN"="MUNICIPIO_RES", "ENTIDAD_FEDERATIVA")) %>%
    mutate(across(c(Casos:`Defunciones Confirmadas`), ~replace_na(.x, 0))) %>%
    rename(`Clave del municipio` = CVE_MUN,
           `Municipio` = NOMGEO)
    
  return(inegi)
}


