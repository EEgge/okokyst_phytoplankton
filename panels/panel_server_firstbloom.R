#### Output ####
output$bloomplotly <- renderPlotly({
  bloomplot()$bloomplotly
})


output$selectorTaxgroup1_bloom <- renderUI({
  selectizeInput("taxgroups1_bloom", "Choose taxonomic group(s):", taxgroups1_bloom()$taxvec1_bloom, multiple = TRUE)
})

output$selectorTaxgroup2_bloom <- renderUI({
  selectizeInput("taxgroups2_bloom", "Choose taxonomic group(s):", taxgroups2_bloom()$taxvec2_bloom, multiple = TRUE)
})



taxgroups1_bloom <- reactive({
  taxlev1_bloom <- input$level1_bloom #Why yes I need to learn namespaces
  print(taxlev1_bloom)
  taxvec1_bloom <- tax %>% pull(taxlev1_bloom) %>% unique()
  list(taxvec1_bloom = taxvec1_bloom)
})

taxgroups2_bloom <- reactive({
  taxlev2_bloom <- input$level2_bloom #Why yes I need to learn namespaces
  taxvec2_bloom <- tax %>% filter(.data[[input$level1_bloom]] %in% input$taxgroups1_bloom) %>% pull(taxlev2_bloom) %>% unique() #24.09 her er jeg!
  list(taxvec2_bloom = taxvec2_bloom)
})



bloomplot <- eventReactive(input$actionb_bloomplot, {
  if (input$level2_bloom == "name") {
  firstbloom_filt <- firstbloom %>% 
    filter(.data[[input$level1_bloom]] %in% input$taxgroups1_bloom) %>% 
    group_by(.data[[input$level2_bloom]], Year, station_code, StationName_new) %>% summarise_at(vars(doy), min) %>% 
    mutate(station_code = factor(station_code, levels = unique(cellcarbon_tax$station_code), ordered = T))
  
  } else {
  
    firstbloom_filt <- firstbloom %>% 
      filter(.data[[input$level1_bloom]] %in% input$taxgroups1_bloom) %>% 
      group_by(.data[[input$level2_bloom]], name, Year, station_code, StationName_new) %>% summarise_at(vars(doy), min) %>% 
      mutate(station_code = factor(station_code, levels = unique(cellcarbon_tax$station_code), ordered = T))
  }
  print(input$taxgroups1_bloom)
  print(head(firstbloom_filt))
  
  firstbloom_plt <- ggplot(firstbloom_filt, aes(x = station_code, y = doy, color = .data[[input$level2_bloom]], shape = Year, 
                                  text = sprintf("Taxon: %s<br>Station: %s<br> Doy: %s", .data[[input$level2_bloom]], StationName_new, doy))) +
    geom_point()+
    coord_flip()#+
  #scale_colour_manual(values = cbPalette)
  ggplotly(firstbloom_plt, tooltip = "text")
  
  list(bloomplotly = firstbloom_plt)
})