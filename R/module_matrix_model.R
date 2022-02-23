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
                module_matrix_model_preview_ui(ns("mm_preview"))
              ),
              
              shinydashboard::box(
                width = 12,
                tags$div(tags$h4("Population Projection Preview"), style = "text-aling: center;"),
                
                shinydashboard::box(
                  width = 12,
                  tags$div(
                    class = "lam_bb",
                    tags$p(
                      "Run a time series projection preview for a hypothetical sample population",
                    ),
                    
                    fluidRow(
                      column(width = 6,
                             numericInput(
                               ns("nsekjr"), label = "n years", value = 50
                             ),),
                      column(width = 6,
                             numericInput(
                               ns("hkjlhkjh"), label = "n replicates", value = 10
                             ),)
                    ),
                    
                    actionButton(
                      ns("demo_projection"),
                      "Demo Projection Time Series",
                      class = "btn btn-info",
                      style = "color:white;"
                    )
                  ),
                  
                ),
                
                
                shinydashboard::box(
                  width = 12,
          
                  tags$div(
                    class = "demo_stressors",
                    tags$p(
                      "Set hypothetical stressor values for the sample population projection preview"
                    ),
                    
                    numericInput(ns("njhjhkum"), label = "Temperature_adult", value = 0),
                    numericInput(ns("jlkjlk"), label = "Temperature_parr", value = 0),
                    numericInput(ns("uiou"), label = "Total_Mortality", value = 0),
                    numericInput(ns("ljlkjl"), label = "Habitat_loss", value = 0),
                    numericInput(ns("nmmn"), label = "Spring_flow_alevin", value = 0),
                    numericInput(ns("ssdfaf"), label = "Spring_flow_sub", value = 0),
                    
                  ),
                  
                  
                  
                ),
                
                
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
                 
                 print("Matrix model main")
                 
                 # Call sub module for HUC results
                 module_matrix_model_inputs_server("mm_inputs")
                 module_matrix_model_preview_server("mm_preview")
                 
                 
                 #-------------------------------------------------------
                 # DISABLE AND ENABLE
                 #-------------------------------------------------------
                 
               })
}