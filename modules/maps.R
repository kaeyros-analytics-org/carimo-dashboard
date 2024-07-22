localisation <- shiny::reactiveFileReader(1000, NULL, "data/localisation.rds", readRDS)

map_ui <- function(id){
 
  ns <- NS(id)
  fluentPage(
    h4("Map Overview"),
    leafletOutput(ns("map_plot"), width = "100%", height = 590)
  )
}


map_server <- function(input, output, session, filterStates){
  
  output$map_plot <- renderLeaflet({
    polygon_popup <- paste0("<strong>",localisation()$quartier,"</strong>", "<br>",
                            "<strong>Nombre de clients: </strong>", localisation()$count
    )%>% 
      lapply(htmltools::HTML)
    
    world <- maps::map("world", fill=TRUE, plot=FALSE)
    world_map <- maptools::map2SpatialPolygons(world, sub(":.*$", "", world$names))
    world_map <- sp::SpatialPolygonsDataFrame(world_map,
                                              data.frame(country=names(world_map),
                                                         stringsAsFactors=FALSE),
                                              FALSE)
    
    choosen_countries <- "Cameroon"
    
    target_map <- subset(world_map, country %in% choosen_countries)
    
    fig_map <- leaflet(data = target_map) %>%
      addTiles() %>%
      addPolygons(weight=1) %>%
      leaflet::addTiles() %>%
      leaflet::addAwesomeMarkers(
        data = localisation(),
        lng = ~longitude, lat = ~latitude,
        popup = polygon_popup, label = polygon_popup,
        clusterOptions = leaflet::markerClusterOptions())
    
    fig_map
  }) 
}