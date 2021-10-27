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
      
      tags$h4("Map Overview"),
      
      fluidRow(
        column(width = 10,
               tags$p("Basin Name", style = "float: right; color:#3c8dbc;"),
               tags$p("HUC Code", style = "float: left; color:#3c8dbc;"),
               leafletOutput(ns("mymap"),
                             height = 550)
               
               ),
        column(width = 2,

               shinydashboard::box(
                 width = 13,
                 shinyWidgets::prettyRadioButtons("radio",
                                  h4("Radio buttons"),
                                  choices = list("Choice 1" = 1,
                                                 "Choice 2" = 2,
                                                 "Choice 3" = 3),
                                  selected = 1), 
               ),

               
               
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
      
      
      output$mymap <- renderLeaflet({
        
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldTopoMap,# Esri.WorldTopoMap Esri.WorldGrayCanvas
                           options = providerTileOptions(noWrap = TRUE,
                                                         opacity = 0.9)
          ) %>%
          addPolygons(data = HUC.Map,
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
      
      
      
    }
  )
}

