#### Output ####
output$tsplot <- renderPlot({
  timeseriesplot()$tsplot
}, height = 1200)



timeseriesplot <- eventReactive(input$actionb_timeseries, {
  species <- cellc_meta %>% filter(name %in% input$taxgroups)
  
  tsplot <- ggplot(species, aes(x = Tid_provetak, y = value, color = name))+
    geom_line()+
    facet_grid(rows = vars(Station_code))
  list(tsplot = tsplot)
  
})