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

#metadat$Station_code[!metadat$Station_code %in% stations$StationCode] %>% unique()
#[1] "VR4"  "VR55" "VR56" "VR58" "VR59" lat lon missing for these stations

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

tax <- readxl::read_xlsx(here("data", "Okokyst_taxonomy_full_2017-2020_SG_EEG.xlsx"))
newtax <- tax %>% mutate(across(.cols = Classification, list(~ifelse(Class == "Bacillariophyta", "Diatoms", ifelse(Classification == "NA", NA, ifelse(Division == "Haptophyta" | Class == "Prymnesiophyceae", "Haptophyta",.))))))
totaldummy <- c(NA, NA, "total", "Total", NA, NA, NA, NA, NA, NA, NA, NA, NA, "Total")
newtax <- rbind.data.frame(newtax, totaldummy)
classgroups <- newtax$Classification_1 %>% unique()
classgroups <- classgroups[-14]
# 
# Create columns with total of each "Classification" group
logcellc_meta_tax_wide <- left_join(cellc_meta_total, newtax, by = c("name")) %>%
    dplyr::group_by(Classification_1, Day, Year, Month, Latitude.y, Longitude.y, Station, Tid_provetak, Station_code, Sample) %>%
    summarise_at(vars(value), sum) %>%
    mutate(logValuegroup = log(value+1)) %>%
    dplyr::select(-value) %>%
    pivot_wider(., names_from = Classification_1, values_from = logValuegroup) %>%
    left_join(., metadat2, by = "Sample") %>% 
    mutate(lat = Latitude.y.y, long = Longitude.y.y)

logcellc_meta_tax <- left_join(logcellc_meta, newtax, by = c("name")) %>% 
                        mutate(Station_code = factor(Station_code, levels = unique(cellc_meta_total$Station_code), ordered = T))

rm(logcellc_meta)
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
