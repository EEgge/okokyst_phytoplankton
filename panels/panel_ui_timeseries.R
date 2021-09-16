sbp_timeseries <- sidebarPanel(
  actionButton("actionb_timeseries", "Create time series plot", icon("signal")),
  selectizeInput("taxgroups", "Select species/category:", names(cellcounts), multiple = TRUE)
)

timeseriespage <- fluidPage(
  headerPanel("Time series for each station"),
  sidebarLayout(
    sbp_timeseries,
    mainPanel(
       plotOutput("tsplot", width = "100%")
    )
  )
)
