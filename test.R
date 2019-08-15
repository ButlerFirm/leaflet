library(shiny)
library(leaflet)
library(leaflet.extras)
library(RJSONIO)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points"),
  actionButton("addjson", "New json"),
  actionButton("addlabel", "New Label"),
  actionButton("addlabelonly", "New Label Only"),
  actionButton("addploygon", "New Polygon")
)

server <- function(input, output, session) {

  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)

  observeEvent(input$addjson,{
    gjson <- fromJSON('inst/examples/test.json')
    gjson$style <- list('fill'='red',"stroke-width"="3")
    gjson$features <- lapply(gjson$features, function(feat) {
      feat$properties$style <- list(
        'color' = 'red',
        'weight' = 2,'stroke' = "true",'fill' = "true","fillColor"='red',
        'opacity' = 1
      )
      feat
    })
    leafletProxy('mymap') %>% addGeoJSON(gjson,layerId = 'testjson',hoverHighlightOptions = highlightOptions(color = "blue", weight = 4,
                                                                                                      bringToFront = TRUE))
  })

  observeEvent(input$addploygon,{
    leafletProxy('mymap') %>%  addRectangles(
      lng1=-118.456554, lat1=34.078039,
      lng2=-118.436383, lat2=34.062717,
      fillColor = "transparent",
      label ='testLable',
      labelOptions =labelOptions(permanent = TRUE, direction = "center", textOnly = FALSE, sticky = FALSE)
    )
  })

  observeEvent(input$addlabelonly, {
    leafletProxy('mymap') %>%
      addLabelOnlyMarkers(
        lng=-118.456554, lat=34.078039,
        label = 'testetsetsteswtesat',
        options = markerOptions(draggable = TRUE),
        labelOptions = leaflet::labelOptions(interactive =TRUE,
          permanent = TRUE,
          direction = "center",
          textOnly = TRUE,
          offset = c(0,10),
          opacity = 1
        )
      )
  })

  observeEvent(input$addlabel,{
    # leafletProxy('mymap') %>% addLabelOnlyMarkers(lng=-36.21094,lat=65.94647,label = 'this is a label',labelOptions = lapply(list, function))
    leafletProxy('mymap') %>% addLabelMarkers(lng = -36.21094,lat = 65.94647,layerId = paste0("labelmarker",01),
                              icon=makeIcon(iconUrl ="move.png",iconWidth =25,iconHeight=25,className="moveicon"),
                              options = markerOptions(draggable=TRUE),label='testfdafdsafdsa',labelclass=paste0("label",'01'))
  })

  observeEvent(input$mymap_geojson_mouseover,{
    # print(input$mymap_geojson_mouseover)
  })

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addBingTiles(apikey = 'Aiwtgh9AygSUzX3KFDrDde6UBJfoZZ0u8pQeZMVupW-QLBOIoUUbblgMgKtAO96x', imagerySet = 'Aerial') %>%
      addMarkers(data = points())
  })
}

shinyApp(ui, server)
