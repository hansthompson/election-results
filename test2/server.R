library(leaflet)
load("big_list.rda")



seattle_geojson <- list(
  type = "Feature",
  geometry = list(type = "MultiPolygon",
                  coordinates = list(
                    list(
                      list(
                        c(-150.0, 61.0),
                        c(-150.1, 61.1),
                        c(-150.1, 61.0)),
                      list( c(-150.2, 61.0),
                            c(-150.3, 61.1),
                            c(-150.5, 61.1)))
                    
                  )
  ),
  properties = list(
    name = "Ballard",
    population = 48000,
    style = list(
      fillColor = "yellow",
      weight = 2,
      color = "#000000"
    )
  ),
  id = "ballard"
)

#fileConn<-file("test2/triangles.json")
#writeLines(toJSON(seattle_geojson, digits = 9, pretty = T), fileConn)
#close(fileConn)


seattle_geojson <- district_polygons
























shinyServer(function(input, output, session) {
  map <- createLeafletMap(session, "map")
  session$onFlushed(once=TRUE, function() {
    map$addGeoJSON(seattle_geojson)
  })
  values <- reactiveValues(selectedFeature = NULL)
  observe({
    evt <- input$map_click
    if (is.null(evt))
      return()
    isolate({
      # An empty part of the map was clicked.
      # Null out the selected feature.
      values$selectedFeature <- NULL
    })
  })
  observe({
    evt <- input$map_geojson_click
    if (is.null(evt))
      return()
    isolate({
      # A GeoJSON feature was clicked. Save its properties
      # to selectedFeature.
      values$selectedFeature <- evt$properties
    })
  })
  output$details <- renderText({
    # Render values$selectedFeature, if it isn't NULL.
    if (is.null(values$selectedFeature))
      return(NULL)
    as.character(tags$div(
      tags$h3(values$selectedFeature$name),
      tags$div(
        "Population:",
        values$selectedFeature$population
      )
    ))
  })
})