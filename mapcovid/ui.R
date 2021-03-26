#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mapview)
library(lubridate)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Mapa COVID-19"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 2,
            selectInput("estado","Estado",
                        c("Aguascalientes", "Baja California", 
                          "Baja California Sur", "Campeche",
                          "Coahuila de Zaragoza", "Colima",
                          "Chiapas", "Chihuahua",
                          "Ciudad de México", "Durango",
                          "Guanajuato", "Guerrero", "Hidalgo",
                          "Jalisco", "México", "Michoacán de Ocampo",
                          "Morelos", "Nayarit","Nuevo León", "Oaxaca",
                          "Puebla", "Querétaro", "Quintana Roo",
                          "San Luis Potosí", "Sinaloa", "Sonora",
                          "Tabasco", "Tamaulipas", "Tlaxcala",
                          "Veracruz de Ignacio de La Llave", 
                          "Yucatán", "Zacatecas"), selected = "México"),
            selectInput("variable","Variable",
                        c("Casos", 
                          "Casos Confirmados",
                          "Hospitalizados",
                          "Hospitalizados Confirmados",
                          "Defunciones",
                          "Defunciones Confirmadas",
                          "Tasa de Hospitalización",
                          "Letalidad",
                          "Positividad")),
            sliderInput("edad", "Rango de edad:",
                        min = 0, max = 99,
                        value = c(60,99)),
            sliderInput("fecha",
                        "Fechas:",
                        min = ymd("2020/01/01"),
                        max = today(),
                        step = 1,
                        value=c(today() - 30, today()),
                        timeFormat="%Y/%m/%d"),
            actionButton("button", "¡Mapear!", width = "100%")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            mapviewOutput("map", width = "100%", height = 700),
            wellPanel(
                p(
                    "@Mapcovid elaborado por Guillermo de Anda-Jáuregui y Rodrigo Zepeda | Fuente: SINAVE"
                )
            )
        )
    )
))
