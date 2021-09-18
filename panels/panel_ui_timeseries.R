sbp_timeseries <- sidebarPanel(
  actionButton("actionb_timeseries", "Create time series plot", icon("signal")),
  radioButtons("grouplev", "Select grouping level", c("Division" = "Division", "Class" = "Class", "Species" = "name", "Main group" = "Classification_1")),
  #selectizeInput("taxgroups", "Select species/category:", names(cellcounts), multiple = TRUE)
  uiOutput("selectorTaxgroup")
  )

timeseriespage <- fluidPage(
  headerPanel("Time series for each station"),
  h5("Stations sorted from south to north"),
  sidebarLayout(
    sbp_timeseries,
    mainPanel(
       plotlyOutput("tsplotly", height = "1200px")
    )
  )
)
