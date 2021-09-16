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
source("panels/panel_ui_stationmap.R", local = TRUE)
source("panels/panel_ui_timeseries.R", local = TRUE)
ui <- navbarPage(
    "Overview over Økokyst plankton count data",
    tabPanel("Økokyst stations", stationmappage),
    tabPanel("Time series", timeseriespage)

)

# Define server logic required to draw a map with leaflet
server <- function(input, output, session) {
    
    source("panels/panel_server_stationmap.R", local = TRUE)
    source("panels/panel_server_timeseries.R", local = TRUE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
