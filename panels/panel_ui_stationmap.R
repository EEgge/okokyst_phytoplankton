sbp_stationmap <- sidebarPanel(
  radioButtons("year",
               "Year:",
               c("2017", "2018", "2019")),
  sliderInput("month", "Month", 
              min = 1, max = 12, value = 1
  ),
  selectInput("varriable", "Size proportional to:", c("KLFA", "Total cell count" = "logTotal",
                                                      "sqrtN-NH4", "sqrtN-SNOX", "sqrtN-TOT", "sqrtP-PO4", "sqrtP-TOT"))
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