#' Matrix Model UI
#'
#' The UI portion of the matrix model
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_matrix_model_ui <- function(id) {
  ns <- NS(id)
  
  tagList(shinydashboard::box(width = 12,
                              tags$h3("Population Model"),),
          fluidRow(
            column(
              width = 8,
              shinydashboard::box(
                width = 12,
                tags$h4("Population Model Inputs (vital rates)"),
                module_matrix_model_inputs_ui(ns("mm_inputs"))
              ),
            ),
            column(
              width = 4,
              shinydashboard::box(
                width = 12,
                tags$div(tags$h4("Matrix Elements"), style = "text-aling: center;"),
                module_matrix_model_elements_ui(ns("mm_elements"))
              ),
              
              shinydashboard::box(
                width = 12,
                tags$div(tags$h4("Population Projection Preview (Sandbox)"), style = "text-aling: center;"),
                module_matrix_model_preview_ui(ns("mm_preview"))
              ),
              
            )
          ))
}


#' Matrix Model Server
#'
#' @param none
#'
#' @return None
#'
module_matrix_model_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("Matrix model main...")
                 
                 # Call sub module for HUC results
                 module_matrix_model_inputs_server("mm_inputs")
                 print("Load module_matrix_model_elements_server...")
                 module_matrix_model_elements_server("mm_elements")
                 print("Load module_matrix_model_preview_server...")
                 module_matrix_model_preview_server("mm_preview")
                 print("End load matrix inputs...")
                 
                 #-------------------------------------------------------
                 # DISABLE AND ENABLE
                 #-------------------------------------------------------
                 
               })
}