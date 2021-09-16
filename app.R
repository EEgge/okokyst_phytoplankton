#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(tmap)
library(mapview)
library(leaflet)
library(ggspatial)
library(ggplot2)
library(ggmap)
library(maps)
library(spData)
library(rgdal)
library(tmaptools)
library(maptools)
library(raster)
library(readr)
library(readxl)
library(here)
library(dplyr)

norway <- getData("GADM", country = "NO", level = 1)
dummydf <- data.frame("year" = c(2015, 2015, 2016, 2016, 2017, 2017), 
                      "lat" = c(59, 58, 57, 58, 60, 59.5),
                      "lng" = rep(10,6))

# Define UI
#source("panels/panel_ui_stationmap.R", local = TRUE)
#source("panels/panel_ui_timeseries.R", local = TRUE)
ui <- navbarPage(
    "Overview over Økokyst plankton count data",
    tabPanel("Økokyst stations", )

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("year",
                        "Year:",
                        c("2017", "2018", "2019")),
            sliderInput("month", "Month", 
                         min = 1, max = 12, value = 1
                        ),
            selectInput("varriable", "Size proportional to:", c("KLFA", "Total cell count" = "logTotal",
                                                               "sqrtN-NH4", "sqrtN-SNOX", "sqrtN-TOT", "sqrtP-PO4", "sqrtP-TOT"))
        ),

        # 
        mainPanel(
           leafletOutput("map")
        )
    )
)

# Define server logic required to draw a map with leaflet
server <- function(input, output, session) {
    
    filteredData <- reactive({
        monthname <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                       "Aug", "Sep", "Oct", "Nov", "Dec")[[input$month]]
        st_year <-  cellc_meta_total %>% dplyr::filter(Year == input$year, Month == monthname)
        return(st_year)
    })
    
    variableReact <- reactive({
        return(input$varriable)
        print(input$varriable)
    })

    output$map <- renderLeaflet({
        leaflet(cellc_meta_total) %>%
            addTiles() %>% 
                fitBounds(~min(long, na.rm = TRUE), 58.5, ~max(long, na.rm = TRUE), ~max(lat, na.rm = TRUE))
    })
    
    observe({
        varriable <- input$varriable
        leafletProxy("map", data = filteredData()) %>%
            clearMarkers() %>%
            addCircleMarkers(color = "red", radius = ~get(varriable), popup = ~paste(get(varriable)))
        print(as.vector(filteredData()[,varriable][1]))
        #print(varriable)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
