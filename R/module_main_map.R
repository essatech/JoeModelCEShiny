#' Main Map Module UI
#'
#' The UI portion of the main map module
#' 
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_main_map_ui <- function(id) {

  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      

      
      
      fluidRow(
        column(width = 9,
               htmlOutput(ns('txt_basin_name')),
               htmlOutput(ns('txt_huc_code')),
               leafletOutput(ns("mainmap"), height = 550),
               
                 fluidRow(
                   column(width = 12,
                   module_huc_results_ui(ns("huc_results"))
                   )
                 ),
               ),
        column(width = 3,
               style = "padding-left: 0; margin-left: -10px;",
               
               tags$div(
                 class = "stack-box",
                 style = "padding-left: 15px; color:#3c8dbc;",
                 tags$p("Basin-level Stressors"),
               ),
               
               tags$div(
                 class = "stack-box csc-box", id = ns("var_id"),
                 shinydashboard::box(
                   width = 12,
                   background = "light-blue",
                   tags$p("System Capacity", style = "float: left;"),
                   tags$p("[0.45%]", style = "float: right;"),
                   tags$div(numericInput(ns("hiddenload"), label = "hidden", value = 0), style = "display:none;")
                 )
               ),
               
               htmlOutput(ns('stressor_variable_list')),
      
        ), 
      ),
      
      
    )
    
  )
}


#' Main Map Module Server
#'
#' The Server portion of the Main Map module
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom dplyr tbl collect mutate arrange select filter pull
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#'
#' @param none
#'
#' @return None
#'
module_main_map_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$mainmap <- renderLeaflet({
        
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldTopoMap,# Esri.WorldTopoMap Esri.WorldGrayCanvas
                           options = providerTileOptions(noWrap = TRUE,
                                                         opacity = 0.9)
          ) %>%
          addPolygons(data = HUC.Map,
                      layerId = HUC.Map$uid,
                      color = "#444444",
                      weight = 1.2,
                      smoothFactor = 0.5,
                      opacity = 0.5,
                      fillOpacity = 0.5,
                      fillColor = color_vec,
                      highlightOptions = highlightOptions(color = "white",
                                                          weight = 2,
                                                          bringToFront = TRUE)
          ) %>%
          addLegend("bottomright",
                    colors = leg_col,
                    labels = leg_lab,
                    title = "Leg. Variable",
                    labFormat = labelFormat(suffix = "mg/L"),
                    opacity = 1
          )
          
      })
      
      
      
      
      # Define the stressor variables to plot
      output$stressor_variable_list <- renderUI({
        print("Re-populating stressor variables")
        snames <- rv_stressor_response$stressor_names
        pnames <- rv_stressor_response$pretty_names
        svar_list <- list()
        
        # Call submodules
        for(s in 1:length(snames)) {
          this_stressor <- snames[s]
          print(this_stressor)
          svar_list[[s]] <- module_stressor_variable_ui(ns(this_stressor))
          module_stressor_variable_server(this_stressor, stressor_index = s)
        }
        return(svar_list)
        
      })
      
      
      
      
      # Show the HUC Code and Basin Name
      output$txt_huc_code <- renderUI({
        if(rv_map_shape()) { 
          tags$p(rv_map_location$huc_id, style = "float: right; color:#3c8dbc;")
        } else {
          tags$p("HUC ID", style = "float: right; color:#3c8dbc;")
        }
      })
      output$txt_basin_name <- renderUI({
        if(rv_map_shape()) { 
          tags$p(rv_map_location$huc_name, style = "float: left; color:#3c8dbc;")
        } else {
          tags$p("Basin Name", style = "float: left; color:#3c8dbc;")
        }
      })
      
      
      # Observe mouseover events over leaflet map
      # note the event concatenation 'object name' + '_click'; 'object name' + '_shape_mouseover' 
      observeEvent(input$mainmap_shape_mouseout, {
        rv_map_shape(FALSE)
        rv_stressor_response$active_values_raw <- NULL
      })
      

      observeEvent(input$mainmap_shape_mouseover, {
        # User hovers mouse over a polygon (layer specific)
        mainmap_shape_mouseover_info <- input$mainmap_shape_mouseover

        if(!(is.null(mainmap_shape_mouseover_info))) {
          # Parse the ID and HUC name
          rv_map_shape(TRUE)
          poly_obj <- mainmap_shape_mouseover_info$id # note leaflet id slot
          parse_id <- strsplit(as.character(poly_obj),'\\|')[[1]]
          huc_id <- parse_id[1]
          huc_name <- parse_id[2]
          rv_map_location$huc_id <- huc_id
          rv_map_location$huc_name <- huc_name
          
          # Get target values for each variable
          # to prevent lag only run every half second
          ctime <- rv_stressor_response$active_refresh
          tdiff <- Sys.time() - ctime
          # Dont re run this more than twice a second
          if(tdiff < 0.5) {
            print("Re-run")
          } else {
            print("ok")
            rv_stressor_response$active_refresh <- Sys.time()
          }
          
          target_vals <- rv_stressor_magnitude$sm_dat %>% 
            filter(HUC_ID == huc_id) %>% dplyr::select("Stressor", "Mean")
          # Set active values
          if(nrow(target_vals) > 0) {
            rv_stressor_response$active_values_raw <- target_vals
          } else {
            # otherwise null
            rv_stressor_response$active_values_raw <- NULL
          }
        }
        
      })
      
      
      
      
      
      # Mouse click events on a given HUC
      observeEvent(input$mainmap_shape_click, {
        
        # create object for clicked polygon
        click <- input$mainmap_shape_click
        print(click)
        
        # define leaflet proxy for second regional level map
        proxy <- leafletProxy("mainmap")
        print(click$id)
        # note leaflet id slot
        # append all HUC click ids in empty vector
        clickedIds$ids <- c(clickedIds$ids, click$id)
        # shapefile with all clicked polygons
        clickedPolys <- HUC.Map[HUC.Map$uid %in% clickedIds$ids, ]
        print("clickedPolys")
        print(clickedPolys)
        
        # if the current click ID [from CC_1] exists in the clicked polygon (if it has been clicked twice)
        if(click$id %in% clickedPolys$uid) {
          
          print("Already in")
          
          # define vector that subsets NAME that matches CC_1 click ID
          #nameMatch <- clickedPolys@data$NAME_1[clickedPolys@data$CC_1 == click$id]
          
          #remove the current click$id AND its name match from the clickedPolys shapefile
          clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% click$id] 
          #clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% nameMatch]
          
          #remove that highlighted polygon from the map
          #proxy %>% removeShape(layerId = click$id)
          
          
          #map highlighted polygons
          proxy %>% addPolygons(data = clickedPolys,
                                fillColor = "red",
                                fillOpacity = 0.5,
                                weight = 2,
                                color = "#1BF6FF",
                                stroke = TRUE,
                                label = clickedPolys$uid, 
                                layerId = clickedPolys$uid)
          
          
        } else {
          
          #map highlighted polygons
          proxy %>% addPolygons(data = clickedPolys,
                                fillColor = "red",
                                fillOpacity = 0.5,
                                weight = 2,
                                color = "#1BF6FF",
                                stroke = TRUE,
                                label = clickedPolys$uid, 
                                layerId = clickedPolys$uid)
        } #END CONDITIONAL
        
      })
      

        
      
      
      
      
      

      
      
      
      
    }
  )
}

