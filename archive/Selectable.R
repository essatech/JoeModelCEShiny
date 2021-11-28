library(raster)
library(shiny)
library(leaflet)

#load shapefile
rwa <- getData("GADM", country = "RWA", level = 1)

shinyApp(
  ui = fluidPage(
    leafletOutput("map")
  ), 
  
  server <- function(input, output, session){
    
    #create empty vector to hold all click ids
    clickedIds <- reactiveValues(ids = vector())
    
    #initial map output
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        addPolygons(data = rwa, 
                    fillColor = "white", 
                    fillOpacity = 1, 
                    color = "black", 
                    stroke = T, 
                    weight = 1, 
                    layerId = rwa@data$NAME_1, 
                    group = "regions", 
                    label = rwa@data$NAME_1)
    }) #END RENDER LEAFLET
    
    observeEvent(input$map_shape_click, {
      
      #create object for clicked polygon
      click <- input$map_shape_click
      
      #define leaflet proxy for second regional level map
      proxy <- leafletProxy("map")
      
      #append all click ids in empty vector 
      clickedIds$ids <- c(clickedIds$ids, click$id)
      
      #shapefile with all clicked polygons - original shapefile subsetted by all admin names from the click list
      clickedPolys <- rwa[rwa@data$NAME_1 %in% clickedIds$ids, ]
      
      #if the current click ID [from CC_1] exists in the clicked polygon (if it has been clicked twice)
      if(click$id %in% clickedPolys@data$CC_1){
        
        #define vector that subsets NAME that matches CC_1 click ID
        nameMatch <- clickedPolys@data$NAME_1[clickedPolys@data$CC_1 == click$id]
        
        #remove the current click$id AND its name match from the clickedPolys shapefile
        clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% click$id] 
        clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% nameMatch]
        
        #remove that highlighted polygon from the map
        proxy %>% removeShape(layerId = click$id)
        
      } else {
        
        #map highlighted polygons
        proxy %>% addPolygons(data = clickedPolys,
                              fillColor = "red",
                              fillOpacity = 1,
                              weight = 1,
                              color = "black",
                              stroke = T,
                              label = clickedPolys@data$CC_1, 
                              layerId = clickedPolys@data$CC_1)
      } #END CONDITIONAL
    }) #END OBSERVE EVENT
  }) #END SHINYAPP
