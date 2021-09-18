#### Output ####
output$map <- renderLeaflet({
  leaflet(cellc_meta_total) %>%
    addTiles() %>% 
    fitBounds(~min(long, na.rm = TRUE), 58.5, ~max(long, na.rm = TRUE), ~max(lat, na.rm = TRUE))
})


filteredData <- reactive({
  monthname <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                 "Aug", "Sep", "Oct", "Nov", "Dec")[[input$month]]
  st_year <-  logcellc_meta_tax_wide %>% dplyr::filter(Year.y == input$year, Month.y == monthname)
  return(st_year)
})

variableReact <- reactive({
  return(input$varriable)
  print(input$varriable)
})



observe({
  varriable <- input$varriable
  leafletProxy("map", data = filteredData()) %>%
    clearMarkers() %>%
    addCircleMarkers(color = "red", radius = ~get(varriable), popup = ~paste(get(varriable)))
  print(as.vector(filteredData()[,varriable][1]))
  #print(varriable)
})