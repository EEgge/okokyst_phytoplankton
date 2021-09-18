sbp_stationmap <- sidebarPanel(
  radioButtons("year",
               "Year:",
               c("2017", "2018", "2019")),
  sliderInput("month", "Month", 
              min = 1, max = 12, value = 1
  ),
  selectInput("varriable", "Size proportional to:", c("KLFA", "Total cell count" = "Total",
                                                      "sqrtN-NH4", "sqrtN-SNOX", "sqrtN-TOT", "sqrtP-PO4", "sqrtP-TOT", 
                                                      "Total diatoms" = "Diatoms", "Total dinoflagellates" = "Dinoflagellata"))
)

stationmappage <- fluidPage(
  headerPanel("Map of Økokyst stations"),
  sidebarLayout(
    sbp_stationmap,
    mainPanel(
      leafletOutput("map")
    )
  )
)