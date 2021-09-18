#### Output ####
output$tsplotly <- renderPlotly({
  timeseriesplot()$tsplotly
})


output$selectorTaxgroup <- renderUI({
  selectizeInput("taxgroup", "Choose taxonomic group(s):", taxgroups()$taxvec, multiple = TRUE)
})

taxgroups <- reactive({
  taxlev <- input$grouplev
  taxvec <- newtax %>% pull(taxlev) %>% unique()
  list(taxvec = taxvec)
})

#Some weird interaction here with the text aes and plotting, 
#cf. https://stackoverflow.com/questions/36325154/how-to-choose-variable-to-display-in-tooltip-when-using-ggplotly
#Solved with dummy grouping variable in geom_line() (I have no idea why)
timeseriesplot <- eventReactive(input$actionb_timeseries, {
  #otutab_prop_tax_filter <- logcellc_meta_tax %>% filter(.data[[input$grouplev]] %in% .env$input$taxgroup)
  if (input$grouplev == "name") {
    selectgroups <- logcellc_meta_tax %>% filter(name %in% .env$input$taxgroup) %>% mutate(logVal = log(value+1))
  } else {
  groupbytab <- logcellc_meta_tax %>% group_by(.data[[input$grouplev]], Day, Year, Month, Latitude.y, Longitude.y, Sample, Station, Station_code, Tid_provetak) %>% summarise_at(vars(value), sum)
  selectgroups <- groupbytab %>% filter(.data[[input$grouplev]] %in% .env$input$taxgroup) %>% mutate(logVal = log(value+1))
  }
  #species <- logcellc_meta_tax %>% filter(name %in% input$taxgroups)
  #print(max(species$logVal))
  
  tsplot <- ggplot(selectgroups, aes(x = Tid_provetak, y = logVal, color = .data[[input$grouplev]],
                                text = sprintf("Taxon: %s<br>Station: %s<br>Cell number: %s<br> Date: %s", .data[[input$grouplev]], Station, value, Tid_provetak)))+
    geom_line(group = 1)+ 
    facet_grid(rows = vars(Station_code))
  tsplotly <- ggplotly(tsplot, tooltip = "text")
  list(tsplotly = tsplotly)
  
})