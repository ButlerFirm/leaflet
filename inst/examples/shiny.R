library(shiny)
library(leaflet)

geodata <- paste(readLines(system.file("examples/test.json", package = "leaflet")), collapse = "\n")

ui <- fluidPage(
  leafletOutput("map1"),
  checkboxInput("addMarker", "Add marker on click"),
  actionButton("clearMarkers", "Clear all markers"),
  textOutput("message", container = h3)
)

server <- function(input, output, session) {
  v <- reactiveValues(msg = "")

  output$map1 <- renderLeaflet({
    leaflet() %>%
      addGeoJSON(geodata) %>%
      addCircles(-60, 60, radius = 5e5, layerId = "circle") %>%
      addTiles()
  })

  observeEvent(input$map1_geojson_mouseover, {
    v$msg <- paste("Mouse is over", input$map1_geojson_mouseover$featureId)
  })
  observeEvent(input$map1_geojson_mouseout, {
    v$msg <- ""
  })
  observeEvent(input$map1_geojson_click, {
    v$msg <- paste("Clicked on", input$map1_geojson_click$featureId)
  })
  observeEvent(input$map1_shape_mouseover, {
    v$msg <- paste("Mouse is over shape", input$map1_shape_mouseover$id)
  })
  observeEvent(input$map1_shape_mouseout, {
    v$msg <- ""
  })
  observeEvent(input$map1_shape_click, {
    v$msg <- paste("Clicked shape", input$map1_shape_click$id)
  })
  observeEvent(input$map1_click, {
    v$msg <- paste("Clicked map at", input$map1_click$lat, "/", input$map1_click$lng)
    if (input$addMarker) {
      leafletProxy("map1") %>%
        addLabelMarkers(lng = input$map1_click$lng, lat = input$map1_click$lat,options = markerOptions(draggable = TRUE), label="hahahahaha",labelclass="test121321")
    }
  })
  observeEvent(input$map1_labelmarker_drag, {
    v$msg <- paste("drag!!!", input$map1_marker_drag$lng)
  })
  observeEvent(input$map1_zoom, {
    v$msg <- paste("Zoom changed to", input$map1_zoom)
  })
  observeEvent(input$map1_bounds, {
    v$msg <- paste("Bounds changed to", paste(input$map1_bounds, collapse = ", "))
  })
  observeEvent(input$clearMarkers, {
    leafletProxy("map1") %>% clearMarkers()
  })

  output$message <- renderText(v$msg)
}

shinyApp(ui, server)
