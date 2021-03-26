#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mapview) #Crashes in shinyapps io if from CRAN devtools::install_github("r-spatial/mapview@develop")
library(leaflet)
library(leafpop)
library(sf)
library(lubridate)
library(tidyverse)
library(readr)

source("scripts/auxiliary_functions.R")
message("Loading data...")
dats        <- read_rds("data/Processed_dats.rds")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    #Valores iniciales
    valores <- reactiveValues(edad = c(60, Inf), 
                              variable = "Casos",
                              estado = "MÉXICO",
                              fecha = c(today() - 30, today()))
    
    observeEvent(input$button,{
        message("Button press")
        valores$edad     <- input$edad
        valores$estado   <- toupper(input$estado)
        valores$variable <- input$variable
        valores$fecha    <- format(as.Date(input$fecha,origin="1970-01-01"))
        cat(valores$fecha)
        })
    
    dataset <- reactive({
        message("Setting up")
        inegi         <- read_rds("data/inegi.rds")
        st_crs(inegi) <- 6372 #Fixes shinyapps io crash
        dats_entity <- data_2_map(dats, 
                                  inegi = inegi, 
                                  min_date = valores$fecha[1], 
                                  max_date = valores$fecha[2],
                                  entidad = valores$estado, 
                                  min_age = valores$edad[1], 
                                  max_age = valores$edad[2])
        dats_entity
    })
    
    
    observe({
    output$map <- renderMapview({
        message("Rendering map")
        data_pop <- dataset() %>% select(Municipio)
        mapview(dataset(), zcol = valores$variable, 
                popup = popupTable(data_pop, zcol = c("Municipio"), row.numbers = F,
                                   feature.id = F))
    })
    })

})
