#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#shiny::runGitHub("okokyst_phytoplankton","EEgge", ref = "main")

library(shiny)
#library(sf)
library(tmap)
#library(mapview)
library(leaflet)
library(ggspatial)
library(ggplot2)
#library(ggmap)
library(maps)
#library(spData)
#library(rgdal)
library(tmaptools)
library(maptools)
library(raster)
library(readr)
library(readxl)
library(here)
library(dplyr)
library(plotly)
library(tidyverse)

# Read data
 stations <- read_xlsx(here("data", "Copy of OKOKYST_Hydrografi_Stasjoner_v5.xlsx"), sheet = "KML")
 cellcounts <- read_xlsx(here("data", "Okokyst_cellcounts.xlsx"))
# names(cellcounts)
 metadat <- read_xlsx(here("data", "Okokyst_metadata.xlsx"), col_types = c("text", "text", "date", "text", "text",
                                                                           "numeric", "text", "text", "text", rep("numeric", 11))) %>%
     mutate(sqrtKLFA = sqrt(KLFA),
            `sqrtN-NH4` = sqrt(`N-NH4`),
            `sqrtN-SNOX` = sqrt(`N-SNOX`),
            `sqrtN-TOT` = sqrt(`N-TOT`),
            `sqrtP-PO4` = sqrt(`P-PO4`),
            `sqrtP-TOT` = sqrt(`P-TOT`),
            `sqrtSI-SIO2` = sqrt(`SI-SIO2`),
            sqrtTEMP = sqrt(TEMP))
metadat2 <- left_join(metadat, stations, by = c("Station_code" = "StationCode")) %>% 
    dplyr::arrange(., by = Latitude.y)

plusone <- function(x) {x+1}

# Create column with total cell counts
cellcounts$total <- cellcounts %>% select(-Sample) %>% rowSums()

cellc_pivot <- tidyr::pivot_longer(cellcounts, cols = -Sample)

 
cellc_meta_total <- cellc_pivot %>% 
    left_join(., metadat2, by = "Sample") %>% mutate(logValue = log(value+1)) %>%
    mutate(lat = Latitude.y, long = Longitude.y) %>% arrange(., lat) 


# #log_cellcounts <- cellcounts %>% mutate_if(., is.numeric, list(~log(plusone(.))))
# Table for time series
 logcellc_pivot <- cellc_pivot %>% mutate(logVal = log(value+1))
 logcellc_meta <- left_join(logcellc_pivot, metadat2, by = "Sample")
 
 rm(cellc_pivot)
 
# Read taxonomy file
tax <- read_delim(here("data", "okokyst_taxonomy.txt"), delim = "\t")
classgroups <- tax$Classification_1 %>% unique()
classgroups <- classgroups[-14]


# Create columns with total of each "Classification" group
logcellc_meta_tax_wide <- left_join(cellc_meta_total, tax, by = c("name")) %>%
    dplyr::group_by(Classification_1, Day, Year, Month, Latitude.y, Longitude.y, Station, Tid_provetak, Station_code, Sample) %>%
    summarise_at(vars(value), sum) %>%
    mutate(logValuegroup = log(value+1)) %>%
    dplyr::select(-value) %>%
    pivot_wider(., names_from = Classification_1, values_from = logValuegroup) %>%
    left_join(., metadat2, by = "Sample") %>% 
    mutate(lat = Latitude.y.y, long = Longitude.y.y)

logcellc_meta_tax <- left_join(logcellc_meta, tax, by = c("name")) %>% 
                        mutate(Station_code = factor(Station_code, levels = unique(cellc_meta_total$Station_code), ordered = T))

rm(logcellc_meta)

# Read cell carbon data
cellcarbon0 <- read_xlsx(here("data", "okokyst_cellcarbon.xlsx")) 
cellcarbon1 <- cellcarbon0 %>% tidyr::separate(Tid_provetak, c("Year", "Month", "Day"), "-", remove = FALSE) %>% 
    tidyr::separate(Day, c("Day", "Time"), " ", remove = T) %>% 
    mutate(verdi = as.numeric(Verdi)) %>% 
    mutate(across(Tid_provetak, ~ as.Date(as.character(.), format = '%Y-%m-%d'))) %>% 
    mutate(Sample = str_c(station_code, Tid_provetak, sep = "."))

cellcarbon_tax <- left_join(cellcarbon1, tax, by = c("name")) %>% 
    mutate(doy = lubridate::yday(Tid_provetak)) %>% #Add variable day-of-year
    left_join(., stations, by = c("station_code" = "StationCode")) %>% # Join with station file with Latitude and Longitude
    dplyr::arrange(., by = Latitude)

firstbloom <- cellcarbon_tax %>% mutate(bloom = ifelse(Verdi > bloomlevel, 1, 0)) %>% 
    filter(bloom == 1) %>% 
    mutate(Station_code = factor(station_code, levels = unique(cellcarbon_tax$station_code), ordered = T))
    

# Define UI
source("panels/panel_ui_stationmap.R", local = TRUE)
source("panels/panel_ui_timeseries.R", local = TRUE)
source("panels/panel_ui_firstbloom.R", local = TRUE)

ui <- navbarPage(
    "Overview over Økokyst plankton count data",
    tabPanel("Økokyst stations", stationmappage),
    tabPanel("Time series", timeseriespage),
    tabPanel("Time at first bloom", bloomplotpage)

)

# Define server logic required to draw a map with leaflet
server <- function(input, output, session) {
    
    source("panels/panel_server_stationmap.R", local = TRUE)
    source("panels/panel_server_timeseries.R", local = TRUE)
    source("panels/panel_server_firstbloom.R", local = TRUE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
