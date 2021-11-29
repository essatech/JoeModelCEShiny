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
               
               # Mouse-over text for basin name and HUC code
               htmlOutput(ns('txt_basin_name')),
               htmlOutput(ns('txt_huc_code')),
               
               # Main leaflet output with variable width and fixed height
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
                 tags$b("Stressor Dose-Response Relationships"),
                 checkboxInput(ns("hover_values"), "Show values on mouse hover", FALSE),
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
               
               # Create the stressor variable sidebar list
               htmlOutput(ns('stressor_variable_list')),
        ), 
      ),
      
    ) # End of main box
  )   # End of main tagList
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
      
      # Call sub module for HUC results
      module_huc_results_server("huc_results")

      # Toggle mouse-over huc values on and off
      observeEvent(input$hover_values, {
        rv_stressor_response$hover_values <- input$hover_values
      })


      # --------------------------------------
      # Main leaflet map reactive expression
      # --------------------------------------
      # Main leaflet map is called here, but only static elements
      # are included that do not need updating with dynamic events.
      # HUCs are called later with proxy since we will update then frequently.
      output$mainmap <- renderLeaflet({
        print("Running renderLeaflet()...")    
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldTopoMap, # Esri.WorldTopoMap Esri.WorldGrayCanvas
                           options = providerTileOptions(noWrap = TRUE,
                                                         opacity = 0.9)
          ) %>%
          addPolygons(data = rv_HUC_layer_load$data,
              layerId = rv_HUC_layer_load$data$uid,
              color = "#444444",
              weight = 1.2,
              smoothFactor = 0.5,
              opacity = 0.5,
              fillOpacity = 0.5,
              fillColor = "#d9d9d9",
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE)
          )
          #fitBounds(-119.04060, 52.37167, -114.78314, 54.76054)
      })

      
      # ---------------------------------------------------------
      # Reactive expression to reload, repaint or redraw polygons
      # ---------------------------------------------------------
      r_huc_polygons <- reactive({
        print("r_huc_polygons() triggered ...")
        
        # If clear all selected - then trigger redraw.
        rv_redraw$redraw

        # Name of the variable to display
        var_name <- rv_stressor_response$active_layer

        # Data for HUCs (Stressor Magnitude)
        sm_df <- rv_stressor_magnitude$sm_dat
        sm_df <- sm_df[sm_df$Stressor == var_name, ]
        
        # HUC spatial geometry
        huc_geom <- rv_HUC_geom$huc_geom
        
        # Merge stressor magnitude values to sf object
        huc_geom$values <- NA
        huc_geom$values <- sm_df$Mean[match(huc_geom$HUC_ID, sm_df$HUC_ID)]
        
        # Look at relationship from response curve
        resp_curv <- rv_stressor_response$sr_dat[[var_name]]
        
        # Convert raw variable values to system capacity
        interp <- approx(x = resp_curv$value,
                         y = resp_curv$mean_system_capacity,
                         xout = huc_geom$values,
                         yleft = 0, yright = 100)
        
        huc_geom$values_sc <- interp$y
        # Apply color ramp function
        huc_geom$color_vec <- color_func(huc_geom$values_sc)

        # Update reference color dataframe rv to reset colors after selection
        col_df <- data.frame(id = huc_geom$HUC_ID, col = huc_geom$color_vec)
        rv_HUC_geom$color_df <- col_df

        return(huc_geom)
      })


      # --------------------------------------
      # HUC polygon draw
      # --------------------------------------
      # Draw update or edit HUC polygons on leaflet map
      # use an observe() function to capture changes and use
      # leafletProxy() to only update the target layer.
      observe({
        print("Updating polygons with observer()...")

        leafletProxy("mainmap") %>%
          clearShapes() %>%
            # Add or update HUC polygons on the map
            addPolygons(data = r_huc_polygons(),
              layerId = r_huc_polygons()$uid,
              color = "#444444",
              weight = 1.2,
              smoothFactor = 0.5,
              opacity = 0.5,
              fillOpacity = 0.5,
              fillColor = r_huc_polygons()$color_vec,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE)
            ) %>%
            # Delete any old pre-existing legend
            clearControls() %>%
            # Add new legend
            addLegend("bottomright",
                      colors = leg_col,
                      labels = leg_lab,
                      title = rv_stressor_response$active_layer,
                      opacity = 0.9
            )

          # Add on selected HUCs (if any)
          selected_hucs <- isolate(rv_clickedIds$ids)
          if(length(selected_hucs) > 0) {
            print("Adding selected HUCs")
            huc_geom_sel <- rv_HUC_geom$huc_geom

            # Get subset of selected HUCs
            huc_geom_sel <- huc_geom_sel[which(huc_geom_sel$uid %in% selected_hucs), ]
            # Update special ID
            huc_geom_sel$uid <- paste0("select|", huc_geom_sel$uid)
            
            # Add selected HUCs to map
            leafletProxy("mainmap") %>%
              addPolygons(data = huc_geom_sel,
                layerId = huc_geom_sel$uid,
                color = "#444444",
                weight = 1.2,
                smoothFactor = 0.5,
                opacity = 0.5,
                fillOpacity = 0.5,
                fillColor = "#4dfff3",
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 2,
                                                    bringToFront = TRUE)
              )
          }

      })

      
      # ---------------------------------------------------------
      # Mouse-Click on HUC events
      # ---------------------------------------------------------
      # Use a second observer to update the selected polygons...
      # Mouse click events on a given HUC
      observeEvent(input$mainmap_shape_click, {
        # create object for clicked polygon
        click <- input$mainmap_shape_click
        # Get all the previous clicked polys
        previous_click_ids <- rv_clickedIds$ids

        # Get the HUC geom for the specific click
        huc_geom <- rv_HUC_geom$huc_geom
        this_uid <- gsub("select\\|", "", click$id)
        huc_geom_single <- huc_geom[which(huc_geom$uid == this_uid), ]
        huc_geom_single$uid <- paste0("select|", huc_geom_single$uid)

        if(this_uid %in% previous_click_ids) {
          print("Remove from selected...")
          # Polygon has already been clicked
          # Need to remove it from the click vector
          new_click_vec <- previous_click_ids[previous_click_ids != this_uid]
          rv_clickedIds$ids <- new_click_vec
          leafletProxy("mainmap") %>%
            removeShape(huc_geom_single$uid)

        } else {
          print("Add to selected")
          # User select a new polygon - nedd to update the color and add it to select list
          # Need to add it from the click vector
          rv_clickedIds$ids <- c(rv_clickedIds$ids, this_uid)

          leafletProxy("mainmap") %>%
            addPolygons(data = huc_geom_single,
              layerId = huc_geom_single$uid,
              color = "#444444",
              weight = 1.2,
              smoothFactor = 0.5,
              opacity = 0.5,
              fillOpacity = 0.5,
              fillColor = "#4dfff3",
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE)
            )
           # End of deselect polygon and restor original color
        } 
        
      })





      
      

      
      
    
      # ---------------------------------------------------------
      # Define the stressor variables to plot div button side bar
      # ---------------------------------------------------------
      output$stressor_variable_list <- renderUI({
        
        print("Re-populating stressor variables...")
        snames <- rv_stressor_response$stressor_names
        pnames <- rv_stressor_response$pretty_names
        svar_list <- list()
        
        # Call sub modules
        for(s in 1:length(snames)) {
          this_stressor <- snames[s]
          print(this_stressor)
          svar_list[[s]] <- module_stressor_variable_ui(ns(this_stressor))
          module_stressor_variable_server(this_stressor, stressor_index = s)
        }
        return(svar_list)
        
      })
      
      
      
      # ---------------------------------------------------------
      # Show the HUC Code and Basin Name Above Map
      # ---------------------------------------------------------
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
      

      # ---------------------------------------------------------
      # Mouse-over and mouse-out events
      # ---------------------------------------------------------
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
          
          # Check box for user to toggle mouse-over display
          # lag and time intensive so turned off by default
          if(input$hover_values) {
            # Get target values for each variable
            # to prevent lag only run every half second
            ctime <- rv_stressor_response$active_refresh
            tdiff <- Sys.time() - ctime
            # Dont re run this more than twice a second
            if(tdiff < 0.5) {
              #print("Re-run")
            } else {
              #print("ok")
              rv_stressor_response$active_refresh <- Sys.time()
            }
            target_vals <- rv_stressor_magnitude$sm_dat %>% 
              dplyr::filter(HUC_ID == huc_id) %>% dplyr::select("Stressor", "Mean")
            # Set active values
            if(nrow(target_vals) > 0) {
              rv_stressor_response$active_values_raw <- target_vals
            } else {
              # otherwise null
              rv_stressor_response$active_values_raw <- NULL
            }
          } else {
            rv_stressor_response$active_values_raw <- NULL
          }

        }
        
      })
      
      
      


        
      
      
      
      
      

      
      
      
      
    }
  )
}

