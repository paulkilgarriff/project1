# load packages

library(shiny)
library(leaflet)
library(sf)
library(leaflet.extras)

# Let's assume `sf_object` is your sf object with a "ratio" column
sf_object <- st_read("Output/eir_ratio.gpkg")
sf_object <- st_transform(sf_object,4326)
sf_object$rat_value_rnd <- round(sf_object$rat_value,digits=2)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      helpText("The 'ratio' variable represents the median new house sales price for first time buyers
               in March 2023 using a 12-month rolling median. This median is then display as the ratio 
               to the national median new sales price for first time buyers. The areas are the Eircode Routing
               Key area. Some areas do not have enough sales to report a ratio. Note: Some areas are quite large, e.g. H91 includes Connamara, Galway City, South Galway
               and parts of North Clare.")  # replace with your description
    ),
    mainPanel(
      leafletOutput("mymap", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(data = sf_object, color = ~colorQuantile("YlOrRd", sf_object$rat_value)(sf_object$rat_value), 
                  fillOpacity = 0.8, weight = 1,
                  label = ~paste("Eircode:", RoutingKey, "Ratio:", as.character(rat_value_rnd)),
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
      addControl("Click to Zoom", position = "topleft") 
  })
  
  observeEvent(input$mymap_shape_click, {
    click <- input$mymap_shape_click
    leafletProxy("mymap") %>% 
      setView(lng = click$lng, lat = click$lat, zoom = 10)
  })
}

shinyApp(ui, server)
