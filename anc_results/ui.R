#library(devtools)
#install_github("ShinyDash", "trestletech")
#install_github('leaflet-shiny', 'jcheng5')
library(leaflet)
library(ShinyDash)

shinyUI(fluidPage(
  tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),
  leafletMap(
    "map", "100%", 400,
    initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
    options=list(
      center = c(61.182427, -149.570774),
      zoom = 9,
      maxBounds = list(list(61.882838, -150.495871), list(60.516243, -148.492368))
    )
  ),
  fluidRow(
    column(8, offset=3,
      h2('Voting Results for Anchorage Fall 2014'),
      htmlWidgetOutput(
        outputId = 'desc',
        HTML(paste(
          'The map is centered at <span id="lat"></span>, <span id="lng"></span>',
          'with a zoom level of <span id="zoom"></span>.<br/>',
          'Top <span id="shownCities"></span> out of <span id="totalCities"></span> visible cities are displayed.'
        ))
      )
    )
  ),

  hr(),
  fluidRow(
    column(3,

      selectInput('year', 'Election', c(2000:2010), 2010),
      selectInput('maxCities', 'Maximum cities to display', choices=c(
        5, 25, 50, 100, 200, 500, 2000, 5000, 10000, All = 100000
      ), selected = 100)
     # selectInput("polygon_type", label = h3("Choose local Geometry"), 
     #       choices = list("Choice 1" = "Senate Districts", "Choice 2" = "House Districts", "Choice 3" = "Precincts"), 
     #       selected = 1),

     #selectInput("Race", label = h3("Choose local Geometry"), 
     #       choices = list("Ballot Measure 1 - Repeal of A0-37" = 1, "Ballot Measure 2 - Legal Cannibus" = 2, 
     #                      "Ballot Measure 3 - Increase Minimum Wage" = 3, "Ballot Measure 4 - Watershed Protection" = 4,
     #                      "Turnout of Registered Voters" = 5), 
     #       selected = 1),

#checkboxGroupInput("Elections", label = h3("Checkbox group"), 
#                   choices = list("Ballot Measure 1 - Repeal of A0-37" = 1, "Ballot Measure 2 - Legal Cannibus" = 2, 
#                                  "Ballot Measure 3 - Increase Minimum Wage" = 3, "Ballot Measure 4 - Watershed Protection" = 4),
#                   selected = 1),

    )
    )

  )
)