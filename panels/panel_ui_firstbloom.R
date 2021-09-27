sbp_bloomplot <- sidebarPanel(
  actionButton("actionb_bloomplot", "Create plot of first bloom", icon("signal")),
  radioButtons("level1_bloom", "Select grouping level 1:", c("Division" = "Division", "Class" = "Class", "Genus" = "Genus", "Species" = "name", "Main group" = "Classification_1")),
  uiOutput("selectorTaxgroup1_bloom"),
  radioButtons("level2_bloom", "Select grouping level 2:", c("Division" = "Division", "Class" = "Class", "Genus" = "Genus", "Species" = "name", "Main group" = "Classification_1")),
  uiOutput("selectorTaxgroup2_bloom")
)

bloomplotpage <- fluidPage(
  headerPanel("Plot of time of first day > bloom concentration"),
  h5("Stations sorted from south to north"),
  sidebarLayout(
    sbp_bloomplot,
    mainPanel(
      plotlyOutput("bloomplotly", height = "1200px", width = "800px")
    )
  )
)
