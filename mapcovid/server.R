#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mapview)
library(leaflet)
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
        dats_entity <- data_2_map(dats, 
                                  inegi = read_rds("data/inegi.rds"), 
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
        mapview(dataset(), zcol = valores$variable, 
                popup = popupTable(dataset(), zcol = c("Municipio")))
    })
    })

})
