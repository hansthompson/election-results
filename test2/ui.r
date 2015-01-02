library(leaflet)
shinyUI(fluidPage(
  leafletMap("map", 1000, 1000, options = list(
    center = c(61.2, -150),
    zoom = 9
  )),
  htmlOutput("details")
))


